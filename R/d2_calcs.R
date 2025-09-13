#' @title d2 and c4 Constants for Gage R&R
#'
#' @description
#' Functions to calculate the \eqn{d_2} constant (expected range of a normal subgroup),
#' the chi-square bias correction factor \eqn{c_4}, and a Minitab-style adjusted
#' \eqn{d_2} that incorporates the number of subgroups.
#'
#' @details
#' - \code{d2_integral(m)} computes the exact \eqn{d_2(m)} by numerical integration.
#' - \code{c4(n)} computes the chi-square bias correction factor \eqn{c_4(n)}.
#' - \code{d2_minitab_df(m, g)} computes a finite-sample adjusted constant
#'   \eqn{d_2^{adj}(m,g) = d_2(m) / c_4(df)}, with
#'   \eqn{df = g \times (m-1)} degrees of freedom, consistent with Minitab/AIAG tables.
#'
#' @param m Integer. Subgroup size (must be \eqn{\ge 2}).
#' @param g Integer. Number of subgroups (must be \eqn{\ge 1}).
#' @param n Integer. Degrees of freedom argument for \code{c4}.
#' @param rel.tol Relative tolerance for integration (passed to \code{integrate()}).
#'
#' @return
#' - \code{d2_integral()} returns a numeric scalar (expected range for subgroup size \code{m}).
#' - \code{c4()} returns a numeric scalar (bias correction factor).
#' - \code{d2_minitab_df()} returns a numeric scalar (adjusted constant).
#'
#' @examples
#' # Exact d2 for subgroup size 5
#' d2_integral(5)
#'
#' # Chi-square bias correction for df = 8
#' c4(8)
#'
#' # Minitab-style adjusted constant for m = 5, g = 2
#' d2_minitab_df(5, 2)
#'
#' @seealso \code{\link{integrate}}, \code{\link{gamma}}
#' @export
d2_integral <- function(m, rel.tol = .Machine$double.eps^0.5) {
  if (m < 2) stop("m must be >= 2")
  integrand <- function(x) 1 - (1 - pnorm(x))^m - pnorm(x)^m
  integrate(integrand, lower = -Inf, upper = Inf, rel.tol = rel.tol)$value
}

#' @rdname d2_integral
#' @export
c4 <- function(n) {
  if (n < 2) stop("n must be >= 2")
  sqrt(2 / (n - 1)) * gamma(n / 2) / gamma((n - 1) / 2)
}

#' @rdname d2_integral
#' @export
d2_minitab_df <- function(m, g) {
  d2 <- d2_integral(m)
  df <- g * (m - 1)
  d2 / c4(df)
}
