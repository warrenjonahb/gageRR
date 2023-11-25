#todo
#load d_tables https://r-pkgs.org/data.html#sec-data-sysdata
library(tidyverse)
library(openxlsx)
data = read_csv("C://Users//warre//Desktop//Grad School//STAT 600//sampleGrrData_1.csv")
data = read.xlsx('C://Users//warre//Downloads//grr_trial.xlsx')
colnames(data) <- c('part', 'operator', 'meas')

xbar_varcomps(data = data2, part = SN, operator = Operator, meas = CN28_AFT_RUNOUT)
anova_var_calcs(data = data2, part = SN, operator = Operator, meas = CN28_AFT_RUNOUT)
anova_grr = grr_calc(data = data, part = part, operator = operator, meas = meas, method = 'anova')
xbar_grr = grr_calc(data = data, part = part, operator = operator, meas = meas, method = 'xbar_r')
