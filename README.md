
# gageRR <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

Analyzing measurement system error is a critical component of the
manufacturing process in numerous industries. While a number of methods
exist to analyze measurement error they can all be broadly described as
repeatability and reproducibilty studies (or often referred to as Gage
R&R studies). Industries can then provide guidelines or limitations on
the amount of error that is acceptable for a given product feature based
on the outcome of a Gage R&R study.

The gageRR package provides two methods to analyze repeatability and
reproducibility: Analysis of Variance (ANOVA) method and Average and
Range method. These methods require a balanced study, that is the same
number of repetitions for each operator and part combination.

## Installation

You can install the development version of gageRR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("warrenjonahb/gageRR")
```

## Example

This is a basic example which takes the appropriately formatted data
(one row per operator and part measurement) and calculates summary gage
evaluation statistics. For more information please see the
gageRR_vignette.

Here, a sample dataset is created to demonstrate the gageRR functions:

``` r
library(gageRR)
data = data.frame(
SN = c(
'SerialNumber_01',
'SerialNumber_01',
'SerialNumber_02',
'SerialNumber_02',
'SerialNumber_01',
'SerialNumber_01',
'SerialNumber_02',
'SerialNumber_02'),

Operator = c(
'Operator_01',
'Operator_01',
'Operator_01',
'Operator_01',
'Operator_02',
'Operator_02',
'Operator_02',
'Operator_02'),

Measure = c(
0.0172,
0.0177,
0.0155,
0.0159,
0.0174,
0.0181,
0.0152,
0.0176))
```

Next we can calculate the sum of squares and ANOVA variance components:

``` r
ss_calcs(data, part = SN, operator = Operator, meas = Measure)
#> $reps
#> [1] 2
#> 
#> $num_parts
#> [1] 2
#> 
#> $num_opers
#> [1] 2
#> 
#> $SS_oper_error
#> [1] 5e-07
#> 
#> $SS_part_error
#> [1] 4.805e-06
#> 
#> $SS_equip_error
#> [1] 3.33e-06
#> 
#> $SS_op_part_error
#> [1] 8e-08
#> 
#> $SS_total_error
#> [1] 8.715e-06
anova_var_calcs(data, part = SN, operator = Operator, meas = Measure)
#> $repeatability
#> [1] 8.325e-07
#> 
#> $reproducibility
#> [1] 1.05e-07
#> 
#> $total_grr
#> [1] 9.375e-07
#> 
#> $part_to_part
#> [1] 1.18125e-06
#> 
#> $total_var
#> [1] 2.11875e-06
```

With these variance components we can then calculate the final gage
evaluation statistics:

``` r
grr_calc(data, part = SN, operator = Operator, meas = Measure, LSL = 0, USL = .040, method = 'anova')
#> $VarianceComponents
#>                     VarComp PercentContribution
#> repeatability   8.32500e-07          0.39292035
#> reproducibility 1.05000e-07          0.04955752
#> total_grr       9.37500e-07          0.44247788
#> part_to_part    1.18125e-06          0.55752212
#> total_var       2.11875e-06          1.00000000
#> 
#> $GageEval
#>                       StdDev    StudyVar PercentStudyVar PercentTolerance
#> repeatability   0.0009124144 0.005474486       0.6268336       0.13686216
#> reproducibility 0.0003240370 0.001944222       0.2226152       0.04860556
#> total_grr       0.0009682458 0.005809475       0.6651901       0.14523688
#> part_to_part    0.0010868533 0.006521120       0.7466740       0.16302799
#> total_var       0.0014555927 0.008733556       1.0000000       0.21833890
```
