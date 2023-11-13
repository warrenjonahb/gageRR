#TO DO
#GRR comps
#Based on user selection "xbar_R" or "ANOVA"
source("anova_varcomps.R")
source("xbar_r_vacromps.R")

grr_calc = function(data, part, operator = NULL, meas, method = 'anova') {
  if(method == 'anova') {

    anova_var_calcs(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})


  }

  if(method == 'xbar_r') {

    xbar_varcomps(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})

    }

}
