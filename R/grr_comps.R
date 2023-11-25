#TO DO
#GRR comps
#Based on user selection "xbar_R" or "ANOVA"
source("anova_varcomps.R")
source("xbar_r_vacromps.R")

grr_calc = function(data, part, operator = NULL, meas, method = 'anova') {
  if(method == 'anova') {
    varComps = anova_var_calcs(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
    gage_eval = data.frame(matrix(unlist(varComps)))
    row.names(gage_eval) <-names(varComps)
    colnames(gage_eval) <- 'VarComp'
  }

  if(method == 'xbar_r') {
    varComps = xbar_varcomps(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
    gage_eval = data.frame(matrix(unlist(varComps)))
    row.names(gage_eval) <-names(varComps)
    colnames(gage_eval) <- 'VarComp'
    }
return(gage_eval)
}
