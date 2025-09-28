# Launch the app with:
# shiny::runApp(system.file("app", package = "gageRR"))

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop(
    "The gageRR Shiny app requires the 'shiny' package. Install it with install.packages('shiny').",
    call. = FALSE
  )
}

make_app_error <- function(message) {
  structure(list(message = message), class = "gageRR_app_error")
}

is_app_error <- function(x) inherits(x, "gageRR_app_error")

parse_limit <- function(value, label) {
  if (is.null(value)) {
    return(list(value = NULL, error = NULL))
  }

  trimmed <- trimws(value)
  if (trimmed == "") {
    return(list(value = NULL, error = NULL))
  }

  numeric_value <- suppressWarnings(as.numeric(trimmed))
  if (is.na(numeric_value)) {
    return(list(value = NULL, error = sprintf("%s must be numeric or left blank.", label)))
  }

  list(value = numeric_value, error = NULL)
}

ui <- shiny::fluidPage(
  shiny::titlePanel("gageRR Shiny Application"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(
        "datafile",
        "Upload measurement data",
        accept = c(".csv", ".xlsx")
      ),
      shiny::helpText("Upload a CSV or Excel file that includes part, operator, and measurement columns."),
      shiny::uiOutput("column_selectors"),
      shiny::textInput("lsl", "Lower Specification Limit (optional)", value = ""),
      shiny::textInput("usl", "Upper Specification Limit (optional)", value = ""),
      shiny::radioButtons(
        "method",
        "Analysis method",
        choices = c("ANOVA" = "anova", "Xbar / R" = "xbar_r"),
        selected = "anova"
      ),
      width = 4
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Variance Components",
          shiny::tableOutput("variance_components")
        ),
        shiny::tabPanel(
          "Gage Evaluation",
          shiny::tableOutput("gage_eval"),
          shiny::br(),
          shiny::textOutput("num_distinct_cats")
        ),
        shiny::tabPanel(
          "ANOVA Table",
          shiny::verbatimTextOutput("anova_table")
        )
      ),
      shiny::br(),
      shiny::h4("Data Preview"),
      shiny::tableOutput("data_preview"),
      width = 8
    )
  )
)

server <- function(input, output, session) {
  data <- shiny::reactive({
    file <- input$datafile
    shiny::req(file)

    extension <- tolower(tools::file_ext(file$name))
    if (!extension %in% c("csv", "xlsx")) {
      return(make_app_error("Please upload a .csv or .xlsx file."))
    }

    if (extension == "csv") {
      df <- utils::read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        return(make_app_error(
          "Reading Excel files requires the 'readxl' package. Install it with install.packages('readxl')."
        ))
      }
      df <- readxl::read_excel(file$datapath)
    }

    as.data.frame(df, stringsAsFactors = FALSE)
  })

  output$column_selectors <- shiny::renderUI({
    df <- data()
    if (is_app_error(df)) {
      shiny::validate(shiny::need(FALSE, df$message))
    }

    columns <- names(df)
    shiny::tagList(
      shiny::selectInput("part_col", "Part column", choices = columns),
      shiny::selectInput("operator_col", "Operator column", choices = columns),
      shiny::selectInput("meas_col", "Measurement column", choices = columns)
    )
  })

  analysis <- shiny::reactive({
    df <- data()
    if (is_app_error(df)) {
      return(df)
    }

    shiny::req(input$part_col, input$operator_col, input$meas_col, input$method)

    lsl <- parse_limit(input$lsl, "LSL")
    if (!is.null(lsl$error)) {
      return(make_app_error(lsl$error))
    }

    usl <- parse_limit(input$usl, "USL")
    if (!is.null(usl$error)) {
      return(make_app_error(usl$error))
    }

    tryCatch(
      gageRR::grr_calc(
        df,
        part = input$part_col,
        operator = input$operator_col,
        meas = input$meas_col,
        LSL = lsl$value,
        USL = usl$value,
        method = input$method
      ),
      error = function(e) make_app_error(e$message)
    )
  })

  output$variance_components <- shiny::renderTable({
    result <- analysis()
    if (is_app_error(result)) {
      shiny::validate(shiny::need(FALSE, result$message))
    }

    vc <- result$VarianceComponents
    data.frame(
      Component = rownames(vc),
      vc,
      row.names = NULL,
      check.names = FALSE
    )
  }, digits = 6)

  output$gage_eval <- shiny::renderTable({
    result <- analysis()
    if (is_app_error(result)) {
      shiny::validate(shiny::need(FALSE, result$message))
    }

    ge <- result$GageEval
    numeric_cols <- vapply(ge, is.numeric, logical(1))
    ge[numeric_cols] <- lapply(ge[numeric_cols], function(col) round(col * 100, 3))

    colnames(ge)[colnames(ge) == "StdDev"] <- "%StdDev"
    colnames(ge)[colnames(ge) == "StudyVar"] <- "%StudyVar"
    colnames(ge)[colnames(ge) == "PercentStudyVar"] <- "%PercentStudyVar"

    data.frame(
      Component = rownames(ge),
      ge,
      row.names = NULL,
      check.names = FALSE
    )
  })

  output$num_distinct_cats <- shiny::renderText({
    result <- analysis()
    if (is_app_error(result)) {
      shiny::validate(shiny::need(FALSE, result$message))
    }

    ndc <- result$NumDistinctCats
    sprintf("Num Distinct Categories: %s", ndc)
  })

  output$anova_table <- shiny::renderPrint({
    result <- analysis()
    if (is_app_error(result)) {
      shiny::validate(shiny::need(FALSE, result$message))
    }

    anova_tbl <- result$AnovaTable
    shiny::validate(shiny::need(!is.null(anova_tbl), "Select the ANOVA method to view this table."))
    print(anova_tbl)
  })

  output$data_preview <- shiny::renderTable({
    df <- data()
    if (is_app_error(df)) {
      shiny::validate(shiny::need(FALSE, df$message))
    }

    utils::head(df)
  })
}

shiny::shinyApp(ui, server)
