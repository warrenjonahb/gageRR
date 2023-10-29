#' Title
#'
#' @param meas Measured value for the feature.
#'
#' @return Xbar repeatability calculation.
#' @export
#'
#' @examples
repeat_calc = function(meas){
   xbar = mean(means)
   R = max(meas) - min(meas)
   gage_repeat = xbar / R
   return(gage_repeat)
}
