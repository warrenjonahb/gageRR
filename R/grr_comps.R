
source("anova_varcomps.R")
source("xbar_r_vacromps.R")

grr_calc = function(data, part, operator = NULL, meas, method = 'anova') {

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
