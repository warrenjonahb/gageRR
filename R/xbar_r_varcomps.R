
#' Average and Range Method Repeatability Calculation
#'
#' @param data An R dataframe or tibble.
#' @param part Column name (unquoted) specifying the unique ID of the part being measured.
#' @param operator Column name (unquoted) specifying the operator for the recorded measurement.
#' @param meas Column name (unquoted) where the measurement value is recorded.
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
  # number of replicates per part/operator
  reps <- aggregate(data[[meas]],
                    by = list(data[[part]], data[[operator]]),
                    FUN = length)
  if (length(unique(reps$x)) != 1) {
    stop("Each part must have an equal number of replicates")
  }
  reps <- unique(reps$x)

  a <- length(unique(data[[part]]))
  k <- length(unique(data[[operator]]))
  g1 <- a * k

  d <- d2_minitab_df(m = reps, g = g1)

  xbar_rep <- aggregate(data[[meas]],
                        by = list(data[[part]], data[[operator]]),
                        FUN = function(x) (max(x) - min(x)) / (a * k))

  repeatability <- (sum(xbar_rep$x) / d)^2
  return(repeatability)
}

#' Average and Range Method Reproducibility Calculation
#'
#' @param data An R dataframe or tibble.
#' @param part Column name (unquoted) specifying the unique ID of the part being measured.
#' @param operator Column name (unquoted) specifying the operator for the recorded measurement.
#' @param meas Column name (unquoted) where the measurement value is recorded.
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
  reps <- aggregate(data[[meas]],
                    by = list(data[[part]], data[[operator]]),
                    FUN = length)
  if (length(unique(reps$x)) != 1) {
    stop("Each part must have an equal number of replicates")
  }
  r <- unique(reps$x)

  a <- length(unique(data[[part]]))
  m1 <- length(unique(data[[operator]]))

  if (m1 == 1) return(0)

  d <- d2_minitab_df(m = m1, g = 1)

  xbar_i <- aggregate(data[[meas]],
                      by = list(data[[operator]]),
                      FUN = mean)
  x_diff <- max(xbar_i$x) - min(xbar_i$x)

  repeatability <- xbar_repeat(data, part, operator, meas)

  reproducibility <- max((x_diff / d)^2 - (repeatability / (a * r)), 0)
  return(reproducibility)
}


#' Average and Range Method Part-to-Part Variance Calculation
#'
#' @param data An R dataframe or tibble.
#' @param part Column name (unquoted) specifying the unique ID of the part being measured.
#' @param meas Column name (unquoted) where the measurement value is recorded.
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
  a <- length(unique(data[[part]]))
  d <- d2_minitab_df(m = a, g = 1)

  part_meas <- aggregate(data[[meas]],
                         by = list(data[[part]]),
                         FUN = mean)

  if (a == 1) {
    r_p <- 0
  } else {
    r_p <- ((max(part_meas$x) - min(part_meas$x)) / d)^2
  }
  return(r_p)
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
