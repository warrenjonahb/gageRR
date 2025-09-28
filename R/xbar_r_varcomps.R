
#' Average and Range Method Repeatability Calculation
#'
#' @param data An R dataframe or tibble containing the required identifier and measurement columns.
#' @param part Column name (unquoted) specifying the unique ID of the part being measured. The column should be a
#'   character or factor vector.
#' @param operator Column name (unquoted) specifying the operator for the recorded measurement. The column should be a
#'   character or factor vector.
#' @param meas Column name (unquoted) where the measurement value is recorded. The column must be numeric and contain no
#'   missing or infinite values.
#'
#' @return A number. The measure of repeatability for the given data.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   SN = rep(c("SerialNumber_01","SerialNumber_02"), each = 4),
#'   Operator = rep(c("Operator_01","Operator_02"), each = 2, times = 2),
#'   Measure = c(0.0172,0.0177,0.0155,0.0159,0.0174,0.0181,0.0152,0.0176)
#' )
#' xbar_repeat(data, part = 'SN', operator = 'Operator', meas = 'Measure')
xbar_repeat <- function(data, part, operator, meas) {
  data <- validate_grr_inputs(data, part_col = part, operator_col = operator, measure_col = meas)

  # number of replicates per part/operator
  rep_counts <- aggregate(data[[meas]],
                          by = list(data[[part]], data[[operator]]),
                          FUN = length)

  num_parts <- length(unique(data[[part]]))
  num_opers <- length(unique(data[[operator]]))
  expected_cells <- num_parts * num_opers

  if (nrow(rep_counts) != expected_cells) {
    stop("Balanced studies require every operator to measure every part.")
  }

  if (length(unique(rep_counts$x)) != 1) {
    stop("Each part must have an equal number of replicates")
  }
  reps <- unique(rep_counts$x)

  g1 <- num_parts * num_opers

  if (g1 <= 20 & reps <= 20){
    d <- d_table[d_table$g == g1 & d_table$m == reps, "d2"]
  } else{
    d <- d2_minitab_df(m = reps, g = g1)
  }

  xbar_rep <- aggregate(data[[meas]],
                        by = list(data[[part]], data[[operator]]),
                        FUN = function(x) (max(x) - min(x)) / (num_parts * num_opers))

  repeatability <- (sum(xbar_rep$x) / d)^2
  return(repeatability)
}

#' Average and Range Method Reproducibility Calculation
#'
#' @param data An R dataframe or tibble containing the required identifier and measurement columns.
#' @param part Column name (unquoted) specifying the unique ID of the part being measured. The column should be a
#'   character or factor vector.
#' @param operator Column name (unquoted) specifying the operator for the recorded measurement. The column should be a
#'   character or factor vector.
#' @param meas Column name (unquoted) where the measurement value is recorded. The column must be numeric and contain no
#'   missing or infinite values.
#'
#' @return A number. The measure of reproducibility for the given data.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   SN = rep(c("SerialNumber_01","SerialNumber_02"), each = 4),
#'   Operator = rep(c("Operator_01","Operator_02"), each = 2, times = 2),
#'   Measure = c(0.0172,0.0177,0.0155,0.0159,0.0174,0.0181,0.0152,0.0176)
#' )
#' xbar_reproduce(data, part = 'SN', operator = 'Operator', meas = 'Measure')

xbar_reproduce <- function(data, part, operator, meas) {
  data <- validate_grr_inputs(data, part_col = part, operator_col = operator, measure_col = meas)

  rep_counts <- aggregate(data[[meas]],
                          by = list(data[[part]], data[[operator]]),
                          FUN = length)

  num_parts <- length(unique(data[[part]]))
  num_opers <- length(unique(data[[operator]]))
  expected_cells <- num_parts * num_opers

  if (nrow(rep_counts) != expected_cells) {
    stop("Balanced studies require every operator to measure every part.")
  }
  if (length(unique(rep_counts$x)) != 1) {
    stop("Each part must have an equal number of replicates")
  }
  r <- unique(rep_counts$x)

  if (num_opers == 1) return(0)

  if (num_opers <= 20){
    d <- d_table[d_table$g == 1 & d_table$m == num_opers, "d2"]
    } else{
    d <- d2_minitab_df(m = num_opers, g = 1)
    }

  xbar_i <- aggregate(data[[meas]],
                      by = list(data[[operator]]),
                      FUN = mean)
  x_diff <- max(xbar_i$x) - min(xbar_i$x)

  repeatability <- xbar_repeat(data, part, operator, meas)

  reproducibility <- max((x_diff / d)^2 - (repeatability / (num_parts * r)), 0)
  return(reproducibility)
}


#' Average and Range Method Part-to-Part Variance Calculation
#'
#' @param data An R dataframe or tibble containing the required identifier and measurement columns.
#' @param part Column name (unquoted) specifying the unique ID of the part being measured. The column should be a
#'   character or factor vector.
#' @param meas Column name (unquoted) where the measurement value is recorded. The column must be numeric and contain no
#'   missing or infinite values.
#'
#' @return A number. The measure of part-to-part variation for the given data.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   SN = rep(c("SerialNumber_01","SerialNumber_02"), each = 4),
#'   Operator = rep(c("Operator_01","Operator_02"), each = 2, times = 2),
#'   Measure = c(0.0172,0.0177,0.0155,0.0159,0.0174,0.0181,0.0152,0.0176)
#' )
#' part_to_part(data, part = 'SN', meas = 'Measure')
part_to_part <- function(data, part, meas) {
  data <- validate_grr_inputs(data, part_col = part, operator_col = NULL, measure_col = meas)

  a <- length(unique(data[[part]]))
  if (a < 2) {
    return(0)
  } else {
    part_meas <- aggregate(data[[meas]],
                           by = list(data[[part]]),
                           FUN = mean)

    if (a <= 20){
      d <- d_table[d_table$g == 1 & d_table$m == a, "d2"]
    } else{
      d <- d2_minitab_df(m = a, g = 1)
    }

    r_p <- ((max(part_meas$x) - min(part_meas$x)) / d)^2

    return(r_p)
  }

}


#' Average and Range Method Variance Component Summary
#'
#' @param data An R dataframe or tibble.
#' @param part Column name (unquoted) specifying the unique ID of the part being measured.
#' @param operator Column name (unquoted) specifying the operator for the recorded measurement.
#' @param meas Column name (unquoted) where the measurement value is recorded.
#'
#' @return A list of numeric values for repeatability, reproducibility, total GRR, part-to-part, and total variance components.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   SN = rep(c("SerialNumber_01","SerialNumber_02"), each = 4),
#'   Operator = rep(c("Operator_01","Operator_02"), each = 2, times = 2),
#'   Measure = c(0.0172,0.0177,0.0155,0.0159,0.0174,0.0181,0.0152,0.0176)
#' )
#' xbar_varcomps(data, part = 'SN', operator = 'Operator', meas = 'Measure')
xbar_varcomps <- function(data, part, operator, meas) {
  repeatability <- xbar_repeat(data, part, operator, meas)
  reproducibility <- xbar_reproduce(data, part, operator, meas)
  ptp <- part_to_part(data, part, meas)

  total_grr <- repeatability + reproducibility
  total_var <- total_grr + ptp

  return(list(
    total_grr = total_grr,
    repeatability = repeatability,
    reproducibility = reproducibility,
    part_to_part = ptp,
    total_var = total_var
  ))
}
