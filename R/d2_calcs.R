# d2_calcs.R

#' Expected Range Constant d2
#'
#' Computes the expected range constant \eqn{d_2(m)} used in control chart
#' calculations, following the AIAG approach.
#' This is the expected value of the range of \eqn{m} independent standard
#' normal samples.
#'
#' @param m Integer. Subgroup size (sample size per subgroup). Must be >= 2.
#' @param method Character. Approximation method. One of:
#'   - `"integral"`: Numerical integration of the exact expected range.
#'   - `"aiag"`: Chi-squared approximation (used in AIAG tables).
#'
#' @return Numeric. The constant \eqn{d_2(m)}.
#' @examples
#' d2(5, method = "integral")
#' d2(5, method = "aiag")
#'
#' @export
d2 <- function(m, method = c("integral", "aiag")) {
  method <- match.arg(method)

  if (m < 2) stop("Subgroup size m must be at least 2.")

  if (method == "integral") {
    return(d2_integral(m))
  } else if (method == "aiag") {
    return(d2_aiag(m))
  }
}

#' d2 via Numerical Integration
#'
#' Internal function: computes \eqn{d_2(m)} using the exact integral definition.
#'
#' @param m Integer. Subgroup size.
#' @return Numeric. Expected range constant.
#' @keywords internal
d2_integral <- function(m) {
  integrand <- function(x) {
    m * (pnorm(x))^(m - 1) * dnorm(x)
  }
  upper <- integrate(integrand, -Inf, Inf)$value
  return(2 * upper)  # symmetry
}

#' d2 via AIAG Chi-Squared Approximation
#'
#' Internal function: computes \eqn{d_2(m)} using the AIAG chi-squared approximation.
#' This matches published AIAG d2 tables for finite subgroup sizes.
#'
#' @param m Integer. Subgroup size.
#' @return Numeric. Approximated expected range constant.
#' @keywords internal
d2_aiag <- function(m) {
  if (m < 2) stop("Subgroup size m must be at least 2.")

  g <- m - 1  # degrees of freedom for chi-squared approximation
  cd <- sqrt(2) * gamma((m + 1) / 2) / gamma(m / 2)
  d2_value <- cd * sqrt(2 * g) * gamma(g / 2) / gamma((g - 1) / 2)

  return(d2_value)
}
