#' Calculate squared_error
#'
#' @param meas A numeric vector
#'
#' @return A number.
#' @export
#'
#' @examples
#' squared_error(c(1.001, 1.003, 1.001))
#' squared_error(c(-.005, .003, .001))
squared_error = function(meas){
   xbar = mean(meas)
   sq_error = (meas - xbar)^2
   return(sq_error)
}
