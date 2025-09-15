#' Gage R&R Evaluation
#'
#' @param data An R dataframe or tible.
#' @param part A column in the data specifying the unique ID of the part being measured
#' @param operator A column in data specifying the operator for the recorded measurement
#' @param meas A column in data where the measurement value is recorded.
#' @param method A string specifying 'anova' or 'xbar_r'
#' @param LSL A number to specify the lower specification limit.
#' @param USL A number to specify the upper specification limit.
#'
#' @return A list of two dataframes: VarianceComponents and GageEval
#' @export
#'
#' @examples
#'
#' data = data.frame(
#' SN = c(
#' 'SerialNumber_01',
#' 'SerialNumber_01',
#' 'SerialNumber_02',
#' 'SerialNumber_02',
#' 'SerialNumber_01',
#' 'SerialNumber_01',
#' 'SerialNumber_02',
#' 'SerialNumber_02'),
#'
#' Operator = c(
#' 'Operator_01',
#' 'Operator_01',
#' 'Operator_01',
#' 'Operator_01',
#' 'Operator_02',
#' 'Operator_02',
#' 'Operator_02',
#' 'Operator_02'),
#'
#'Measure = c(
#' 0.0172,
#' 0.0177,
#' 0.0155,
#' 0.0159,
#' 0.0174,
#' 0.0181,
#' 0.0152,
#' 0.0176))
#'
#'grr_calc(data, part = SN, operator = Operator,
#'meas = Measure, LSL = 0, USL = .040, method = 'xbar_r')

grr_calc = function(data, part, operator, meas, LSL = NULL, USL = NULL, method = 'anova') {

  if(method == 'anova') {
    varComps = anova_var_calcs(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
    anovaTable = anova_table(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
  } else if (method == 'xbar_r') {
    varComps = xbar_varcomps(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
  } else {
    stop(print('Supplied method is not supported'))
  }

    VarianceComponents = data.frame(matrix(unlist(varComps)))
    row.names(VarianceComponents) <-names(varComps)
    colnames(VarianceComponents) <- 'VarComp'

    TotalVariation = VarianceComponents['total_var',]
    VarianceComponents['PercentContribution'] = VarianceComponents['VarComp']/TotalVariation

    GageEval = data.frame(row.names = rownames(VarianceComponents))
    GageEval['StdDev'] = sqrt(VarianceComponents$VarComp)
    GageEval['StudyVar'] =  GageEval * 6
    TotalStudyVar = GageEval['total_var', 'StudyVar']
    GageEval['PercentStudyVar'] = GageEval['StudyVar'] / TotalStudyVar

    if(!is.null(USL) | !is.null(LSL)){

      if(is.null(USL) & !is.null(LSL)){
        stop('LSL provided with no USL.  Unable to interpret tolerance band')
      } else if (!is.null(USL) & is.null(LSL)){
        tolerance_band = USL
      } else {
        tolerance_band = USL - LSL
      }

      GageEval['PercentTolerance'] = GageEval['StudyVar'] / tolerance_band

    }
if(method == 'anova') {
return(list(anovaTable = annovaTable, VarianceComponents = VarianceComponents, GageEval = GageEval))
}else{
  return(list(VarianceComponents = VarianceComponents, GageEval = GageEval))
}
}
