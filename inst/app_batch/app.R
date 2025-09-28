# Launch the app with:
# shiny::runApp(system.file("app_batch", package = "gageRR"))

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop(
    "The gageRR batch Shiny app requires the 'shiny' package. Install it with install.packages('shiny').",
    call. = FALSE
  )
}

make_app_error <- function(message) {
  structure(list(message = message), class = "gageRR_app_error")
}

is_app_error <- function(x) inherits(x, "gageRR_app_error")

read_uploaded_data <- function(file_input, label) {
  if (is.null(file_input)) {
    return(NULL)
  }

  extension <- tolower(tools::file_ext(file_input$name))
  if (!extension %in% c("csv", "xlsx")) {
    return(make_app_error(sprintf("%s must be a .csv or .xlsx file.", label)))
  }

  data <- tryCatch(
    {
      if (extension == "csv") {
        utils::read.csv(file_input$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Reading Excel files requires the 'readxl' package. Install it with install.packages('readxl').")
        }
        readxl::read_excel(file_input$datapath)
      }
    },
    error = function(e) make_app_error(e$message)
  )

  if (is_app_error(data)) {
    return(data)
  }

  as.data.frame(data, stringsAsFactors = FALSE)
}

parse_numeric_column <- function(values, label) {
  if (is.null(values)) {
    return(list(values = NULL, error = NULL))
  }

  if (is.numeric(values)) {
    return(list(values = values, error = NULL))
  }

  chars <- as.character(values)
  trimmed <- trimws(chars)
  trimmed[trimmed == ""] <- NA

  suppressWarnings(numeric_values <- as.numeric(trimmed))
  invalid <- !is.na(trimmed) & is.na(numeric_values)
  if (any(invalid)) {
    return(list(
      values = NULL,
      error = sprintf(
        "%s column contains non-numeric values: %s",
        label,
        paste(unique(trimmed[invalid]), collapse = ", ")
      )
    ))
  }

  list(values = numeric_values, error = NULL)
}

select_first_non_na <- function(values) {
  if (!length(values)) {
    return(NA_real_)
  }
  non_na <- values[!is.na(values)]
  if (!length(non_na)) {
    return(NA_real_)
  }
  non_na[[1]]
}

ui <- shiny::fluidPage(
  shiny::titlePanel("gageRR Batch Analysis"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(
        "datafile",
        "Upload measurement data",
        accept = c(".csv", ".xlsx")
      ),
      shiny::helpText("Upload a CSV or Excel file that includes part, operator, measurement, and feature description columns."),
      shiny::uiOutput("column_selectors"),
      shiny::tags$hr(),
      shiny::fileInput(
        "tolerancefile",
        "Upload tolerance data (optional)",
        accept = c(".csv", ".xlsx")
      ),
      shiny::helpText("Include dim_desc, LSL, and USL columns to calculate PercentTolerance."),
      shiny::uiOutput("tolerance_selectors"),
      shiny::tags$hr(),
      shiny::radioButtons(
        "method",
        "Analysis method",
        choices = c("ANOVA" = "anova", "Xbar / R" = "xbar_r"),
        selected = "anova"
      ),
      shiny::downloadButton("download_results", "Download results"),
      width = 4
    ),
    shiny::mainPanel(
      shiny::h3("Batch results"),
      shiny::tableOutput("results_table"),
      shiny::br(),
      shiny::h4("Measurement data preview"),
      shiny::tableOutput("data_preview"),
      shiny::br(),
      shiny::conditionalPanel(
        condition = "output.show_tolerance_preview",
        shiny::h4("Tolerance data preview"),
        shiny::tableOutput("tolerance_preview")
      ),
      width = 8
    )
  )
)

server <- function(input, output, session) {
  measurement_data <- shiny::reactive({
    file <- input$datafile
    shiny::req(file)
    read_uploaded_data(file, "Measurement data")
  })

  tolerance_data <- shiny::reactive({
    file <- input$tolerancefile
    if (is.null(file)) {
      return(NULL)
    }
    read_uploaded_data(file, "Tolerance data")
  })

  output$column_selectors <- shiny::renderUI({
    df <- measurement_data()
    if (is_app_error(df)) {
      shiny::validate(shiny::need(FALSE, df$message))
    }

    columns <- names(df)
    shiny::tagList(
      shiny::selectInput("part_col", "Part column", choices = columns),
      shiny::selectInput("operator_col", "Operator column", choices = columns),
      shiny::selectInput("meas_col", "Measurement column", choices = columns),
      shiny::selectInput("dim_col", "Dimension description column", choices = columns)
    )
  })

  output$tolerance_selectors <- shiny::renderUI({
    tol_df <- tolerance_data()
    if (is.null(tol_df)) {
      return(shiny::helpText("No tolerance file uploaded."))
    }

    if (is_app_error(tol_df)) {
      shiny::validate(shiny::need(FALSE, tol_df$message))
    }

    columns <- names(tol_df)
    shiny::tagList(
      shiny::selectInput("tolerance_dim_col", "Tolerance dim_desc column", choices = columns),
      shiny::selectInput("tolerance_lsl_col", "Tolerance LSL column", choices = columns),
      shiny::selectInput("tolerance_usl_col", "Tolerance USL column", choices = columns)
    )
  })

  analysis <- shiny::reactive({
    df <- measurement_data()
    if (is_app_error(df)) {
      return(df)
    }

    shiny::req(input$part_col, input$operator_col, input$meas_col, input$dim_col, input$method)

    tol_df <- tolerance_data()
    if (is_app_error(tol_df)) {
      return(tol_df)
    }

    tolerance_lookup <- NULL
    if (!is.null(tol_df)) {
      shiny::req(input$tolerance_dim_col, input$tolerance_lsl_col, input$tolerance_usl_col)

      tol_dim <- as.character(tol_df[[input$tolerance_dim_col]])

      lsl_parsed <- parse_numeric_column(tol_df[[input$tolerance_lsl_col]], "LSL")
      if (!is.null(lsl_parsed$error)) {
        return(make_app_error(lsl_parsed$error))
      }

      usl_parsed <- parse_numeric_column(tol_df[[input$tolerance_usl_col]], "USL")
      if (!is.null(usl_parsed$error)) {
        return(make_app_error(usl_parsed$error))
      }

      tolerance_lookup <- data.frame(
        dim = tol_dim,
        LSL = lsl_parsed$values,
        USL = usl_parsed$values,
        stringsAsFactors = FALSE
      )
    }

    dim_values <- df[[input$dim_col]]
    shiny::req(dim_values)

    dim_strings <- as.character(dim_values)
    unique_dims <- unique(dim_strings[!is.na(dim_strings)])
    if (!length(unique_dims)) {
      return(make_app_error("No dimension descriptions found in the selected column."))
    }

    results <- lapply(unique_dims, function(current_dim) {
      dim_idx <- which(dim_strings == current_dim)
      subset_df <- df[dim_idx, , drop = FALSE]

      LSL_value <- NULL
      USL_value <- NULL
      if (!is.null(tolerance_lookup)) {
        tol_rows <- tolerance_lookup[tolerance_lookup$dim == current_dim, , drop = FALSE]
        if (nrow(tol_rows) > 0) {
          LSL_value <- select_first_non_na(tol_rows$LSL)
          USL_value <- select_first_non_na(tol_rows$USL)
        }
      }

      if (is.null(LSL_value) || is.na(LSL_value)) {
        LSL_value <- NULL
      }
      if (is.null(USL_value) || is.na(USL_value)) {
        USL_value <- NULL
      }

      calc <- tryCatch(
        gageRR::grr_calc(
          subset_df,
          part = input$part_col,
          operator = input$operator_col,
          meas = input$meas_col,
          LSL = LSL_value,
          USL = USL_value,
          method = input$method
        ),
        error = function(e) make_app_error(sprintf("Error analyzing %s: %s", current_dim, e$message))
      )

      if (is_app_error(calc)) {
        return(calc)
      }

      gage_eval <- calc$GageEval
      if (is.null(rownames(gage_eval))) {
        if ("Component" %in% colnames(gage_eval)) {
          rownames(gage_eval) <- as.character(gage_eval$Component)
        }
      }

      if (!"total_grr" %in% rownames(gage_eval)) {
        return(make_app_error(sprintf("Result for %s does not include total_grr row.", current_dim)))
      }

      percent_study <- gage_eval["total_grr", "PercentStudyVar"]
      percent_tol <- if ("PercentTolerance" %in% colnames(gage_eval)) {
        gage_eval["total_grr", "PercentTolerance"]
      } else {
        NA_real_
      }

      data.frame(
        dim_desc = current_dim,
        NumDistinctCats = calc$NumDistinctCats,
        PercentStudyVariation = percent_study,
        PercentTolerance = percent_tol,
        stringsAsFactors = FALSE
      )
    })

    has_error <- vapply(results, is_app_error, logical(1))
    if (any(has_error)) {
      return(results[[which(has_error)[1]]])
    }

    combined <- do.call(rbind, results)
    combined$PercentStudyVariation <- round(combined$PercentStudyVariation * 100, 3)

    tolerance_available <- !is.null(tolerance_lookup)
    if (tolerance_available) {
      combined$PercentTolerance <- round(combined$PercentTolerance * 100, 3)
    } else {
      combined$PercentTolerance <- NULL
    }

    combined[order(combined$dim_desc), , drop = FALSE]
  })

  output$results_table <- shiny::renderTable({
    result <- analysis()
    if (is_app_error(result)) {
      shiny::validate(shiny::need(FALSE, result$message))
    }
    result
  }, digits = 3)

  output$data_preview <- shiny::renderTable({
    df <- measurement_data()
    if (is_app_error(df)) {
      shiny::validate(shiny::need(FALSE, df$message))
    }
    utils::head(df)
  })

  output$tolerance_preview <- shiny::renderTable({
    tol_df <- tolerance_data()
    if (is.null(tol_df)) {
      return(NULL)
    }
    if (is_app_error(tol_df)) {
      shiny::validate(shiny::need(FALSE, tol_df$message))
    }
    utils::head(tol_df)
  })

  output$show_tolerance_preview <- shiny::reactive({
    tol_df <- tolerance_data()
    !is.null(tol_df) && !is_app_error(tol_df)
  })
  shiny::outputOptions(output, "show_tolerance_preview", suspendWhenHidden = FALSE)

  output$download_results <- shiny::downloadHandler(
    filename = function() {
      "gageRR_batch_results.csv"
    },
    content = function(file) {
      result <- analysis()
      if (is_app_error(result)) {
        stop(result$message)
      }
      utils::write.csv(result, file, row.names = FALSE)
    }
  )
}

shiny::shinyApp(ui, server)
