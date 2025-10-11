test_that("ANOVA/XBAR_R GRR Computation Check", {
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

  grr_anova_check = grr_calc(data, part = 'SN', operator = 'Operator', meas = 'Measure', LSL = .150, USL = .180, method = 'anova')

  expect_equal(round(grr_anova_check$VarianceComponents["repeatability", "PercentContribution"],3), .366)
  expect_equal(round(grr_anova_check$VarianceComponents["reproducibility", "PercentContribution"],3), .000)
  expect_equal(round(grr_anova_check$VarianceComponents["total_grr", "PercentContribution"],3), .366)
  expect_equal(round(grr_anova_check$VarianceComponents["part_to_part", "PercentContribution"],3), .634)
  expect_equal(round(grr_anova_check$VarianceComponents["total_var", "PercentContribution"],3), 1.000)

  expect_equal(round(grr_anova_check$GageEval["repeatability", "PercentTolerance"],3), .165)
  expect_equal(round(grr_anova_check$GageEval["reproducibility", "PercentTolerance"],3), .000)
  expect_equal(round(grr_anova_check$GageEval["total_grr", "PercentTolerance"],3), .165)
  expect_equal(round(grr_anova_check$GageEval["part_to_part", "PercentTolerance"],3), .217)
  expect_equal(round(grr_anova_check$GageEval["total_var", "PercentTolerance"],3), .273)

  grr_xbar_check = grr_calc(data, part = 'SN', operator = 'Operator', meas = 'Measure', LSL = .150, USL = .180, method = 'xbar_r')

  expect_equal(round(grr_xbar_check$VarianceComponents["repeatability", "PercentContribution"],3), .364)
  expect_equal(round(grr_xbar_check$VarianceComponents["reproducibility", "PercentContribution"],3), 0)
  expect_equal(round(grr_xbar_check$VarianceComponents["total_grr", "PercentContribution"],3), .364)
  expect_equal(round(grr_xbar_check$VarianceComponents["part_to_part", "PercentContribution"],3), .636)
  expect_equal(round(grr_xbar_check$VarianceComponents["total_var", "PercentContribution"],3), 1.000)

  expect_equal(round(grr_xbar_check$GageEval["repeatability", "PercentTolerance"],3), .166)
  expect_equal(round(grr_xbar_check$GageEval["reproducibility", "PercentTolerance"],3), 0)
  expect_equal(round(grr_xbar_check$GageEval["total_grr", "PercentTolerance"],3), .166)
  expect_equal(round(grr_xbar_check$GageEval["part_to_part", "PercentTolerance"],3), .219)
  expect_equal(round(grr_xbar_check$GageEval["total_var", "PercentTolerance"],3), .275)

  expect_error(grr_calc(data, part = SN, operator = Operator, meas = Measure, LSL = .150, USL = NULL, method = 'anova'))
  expect_error(grr_calc(data, part = SN, operator = Operator, meas = Measure, LSL = .150, USL = NULL, method = 'avg_range'))
  expect_error(
    grr_calc(data, part = 'SN', operator = 'Operator', meas = 'Measure', LSL = .180, USL = .150),
    "USL must be greater than LSL"
  )
  expect_error(
    grr_calc(data, part = 'SN', operator = 'Operator', meas = 'Measure', USL = 0),
    "USL must be greater than 0"
  )
  })
