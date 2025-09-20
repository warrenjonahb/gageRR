# Internal helper to validate GRR inputs
validate_grr_inputs <- function(data, part_col, operator_col = NULL, measure_col) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame or tibble.")
  }

  required_cols <- c(part_col, measure_col)
  if (!is.null(operator_col)) {
    required_cols <- c(required_cols, operator_col)
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Column%s %s %s missing from `data`.",
      if (length(missing_cols) > 1) "s" else "",
      paste(shQuote(missing_cols), collapse = ", "),
      if (length(missing_cols) > 1) "are" else "is"
    ))
  }

  measurement_values <- data[[measure_col]]
  if (!is.numeric(measurement_values)) {
    stop(sprintf("Column %s must be numeric.", shQuote(measure_col)))
  }

  if (anyNA(measurement_values)) {
    stop(sprintf("Column %s contains missing (NA/NaN) values.", shQuote(measure_col)))
  }

  if (any(is.infinite(measurement_values))) {
    stop(sprintf("Column %s contains non-finite (Inf/-Inf) values.", shQuote(measure_col)))
  }

  part_values <- data[[part_col]]
  if (!is.factor(part_values) && !is.character(part_values)) {
    stop(sprintf("Column %s must be a character or factor identifier.", shQuote(part_col)))
  }

  if (!is.null(operator_col)) {
    operator_values <- data[[operator_col]]
    if (!is.factor(operator_values) && !is.character(operator_values)) {
      stop(sprintf("Column %s must be a character or factor identifier.", shQuote(operator_col)))
    }
  }

  invisible(TRUE)
}
