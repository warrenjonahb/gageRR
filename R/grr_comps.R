#' GRR Calculations
#'
#' @param data An R dataframe or tible.
#' @param part The column in data specifying the unique ID of the part being measured
#' @param operator A column in data specifying the operator for the recorded measurement
#' @param meas A column in data where the measurement value is recorded.
#' @param method A string specifying 'anova' or 'xbar_r'
#'
#' @return A list of two dataframes: VarianceComponents and GageEval
#' @export
#'
#' @examples
grr_calc = function(data, part, operator, meas, method = 'anova') {

  if(method == 'anova') {
    varComps = anova_var_calcs(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
  }else{
    varComps = xbar_varcomps(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
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

return(list(VarianceComponents = VarianceComponents, GageEval = GageEval))
}
