
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
number of repetitions and for each operator and part combination.

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

grr_calc(data, part = SN, operator = Operator, meas = Measure, LSL = 0, USL = .040, method = 'xbar_r')
#> $VarianceComponents
#>                      VarComp PercentContribution
#> repeatability   6.873124e-07          0.34134037
#> reproducibility 1.250005e-07          0.06207908
#> total_grr       8.123129e-07          0.40341945
#> part_to_part    1.201256e-06          0.59658055
#> total_var       2.013569e-06          1.00000000
#> 
#> $GageEval
#>                       StdDev    StudyVar PercentStudyVar PercentTolerance
#> repeatability   0.0008290430 0.004974258       0.5842434       0.12435646
#> reproducibility 0.0003535541 0.002121325       0.2491567       0.05303312
#> total_grr       0.0009012840 0.005407704       0.6351531       0.13519260
#> part_to_part    0.0010960183 0.006576110       0.7723863       0.16440274
#> total_var       0.0014190028 0.008514017       1.0000000       0.21285042
```
