#' Gage R&R Evaluation
#'
#' @param data An R dataframe or tibble.
#' @param part A string giving the column name specifying the unique ID of the part being measured.
#' @param operator A string giving the column name specifying the operator for the recorded measurement.
#' @param meas A string giving the column name where the measurement value is recorded.
#' @param method A string specifying "anova" or "xbar_r".
#' @param LSL A number specifying the lower specification limit.
#' @param USL A number specifying the upper specification limit.
#'
#' @return A list containing:
#' \itemize{
#'   \item VarianceComponents: Data frame of variance components and percent contribution
#'   \item GageEval: Data frame of study variation metrics
#'   \item AnovaTable: ANOVA table (if method = "anova")
#' }
#' @export
#'
#' @examples
#' data <- data.frame(
#'   SN = rep(c("SerialNumber_01","SerialNumber_02"), each = 4),
#'   Operator = rep(c("Operator_01","Operator_02"), each = 2, times = 2),
#'   Measure = c(0.0172,0.0177,0.0155,0.0159,0.0174,0.0181,0.0152,0.0176)
#' )
#'
#' grr_calc(data, part = "SN", operator = "Operator",
#'          meas = "Measure", LSL = 0, USL = 0.040, method = "xbar_r")
grr_calc <- function(data, part, operator, meas, LSL = NULL, USL = NULL, method = "anova") {
  if (method == "anova") {
    varComps <- anova_var_calcs(data, part, operator, meas)
    anovaTable <- anova_table(data, part, operator, meas)
  } else if (method == "xbar_r") {
    varComps <- xbar_varcomps(data, part, operator, meas)
    anovaTable <- NULL
  } else {
    stop("Supplied method is not supported. Use 'anova' or 'xbar_r'.")
  }

  # Build VarianceComponents data frame
  VarianceComponents <- data.frame(matrix(unlist(varComps)))
  row.names(VarianceComponents) <- names(varComps)
  colnames(VarianceComponents) <- "VarComp"

  TotalVariation <- VarianceComponents["total_var", ]
  VarianceComponents["PercentContribution"] <- VarianceComponents$VarComp / TotalVariation

  # Build GageEval data frame
  GageEval <- data.frame(row.names = rownames(VarianceComponents))
  GageEval["StdDev"] <- sqrt(VarianceComponents$VarComp)
  GageEval["StudyVar"] <- GageEval$StdDev * 6
  TotalStudyVar <- GageEval["total_var", "StudyVar"]
  GageEval["PercentStudyVar"] <- GageEval$StudyVar / TotalStudyVar

  if (!is.null(USL) | !is.null(LSL)) {
    if (is.null(USL) & !is.null(LSL)) {
      stop("LSL provided with no USL. Unable to interpret tolerance band.")
    } else if (!is.null(USL) & is.null(LSL)) {
      tolerance_band <- USL
    } else {
      tolerance_band <- USL - LSL
    }
    GageEval["PercentTolerance"] <- GageEval$StudyVar / tolerance_band
  }

  return(list(
    VarianceComponents = VarianceComponents,
    GageEval = GageEval,
    AnovaTable = anovaTable
  ))
}
