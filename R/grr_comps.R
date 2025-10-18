#' Gage R&R Evaluation
#'
#' @param data An R dataframe or tibble containing the required identifier and measurement columns.
#' @param part A string giving the column name specifying the unique ID of the part being measured. The column
#'   should be a character or factor.
#' @param operator A string giving the column name specifying the operator for the recorded measurement. The column
#'   should be a character or factor.
#' @param meas A string giving the column name where the measurement value is recorded. The column must be numeric and
#'   contain no missing or infinite values.
#' @param method A string specifying "anova" or "xbar_r".  The ANOVA method fits a mixed-effects
#'   model and permits unequal replicate counts per operator/part combination (each combination still
#'   requires at least two measurements).  The average/range method continues to require a balanced
#'   design with an equal number of replicates for every combination.
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

  data <- validate_grr_inputs(data, part_col = part, operator_col = operator, measure_col = meas)

  if (method == "anova") {
    varComps <- anova_var_calcs(data, part, operator, meas)
    if (!isTRUE(attr(varComps, "balanced_design"))) {
      message("Note: This study has unequal replicate counts, so a pooled mixed-effects estimator is used for the variance components.")
    }
    anovaTable <- anova_table(data, part, operator, meas)
  } else if (method == "xbar_r") {
    varComps <- xbar_varcomps(data, part, operator, meas)
    anovaTable <- NULL
  } else {
    stop("Supplied method is not supported. Use 'anova' or 'xbar_r'.")
  }

  if (!is.null(LSL) && !is.null(USL) && USL <= LSL) {
    stop("USL must be greater than LSL", call. = FALSE)
  }

  if (is.null(LSL) && !is.null(USL) && USL <= 0) {
    stop("USL must be greater than 0", call. = FALSE)
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

  num_dist_cats <- sqrt(2) * (GageEval["part_to_part", "StdDev"] /
                                GageEval["repeatability", "StdDev"])

  num_dist_cats_int <- floor(num_dist_cats)

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

  result <- list(
    VarianceComponents = VarianceComponents,
    GageEval = GageEval,
    NumDistinctCats = num_dist_cats_int,
    AnovaTable = anovaTable
  )

  class(result) <- c("grr_result", "list")

  result
}
