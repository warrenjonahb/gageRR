test_that("multiplication works", {

  data = data.frame(
    SN = c(
      'SerialNumber_01',
      'SerialNumber_01',
      'SerialNumber_01',
      'SerialNumber_02',
      'SerialNumber_02',
      'SerialNumber_02'),

    Operator = c(
      'Operator_01',
      'Operator_01',
      'Operator_01',
      'Operator_01',
      'Operator_01',
      'Operator_01'),

    Measure = c(
      1,
      1,
      1,
      3,
      3,
      3))

  singleOperTest_SS = ss_calcs(data, part = 'SN', operator = 'Operator', meas = 'Measure')
  expect_equal(singleOperTest_SS$reps, 3)
  expect_equal(singleOperTest_SS$num_parts, 2)
  expect_equal(singleOperTest_SS$num_opers, 1)
  expect_equal(singleOperTest_SS$SS_oper_error, 0)
  expect_equal(singleOperTest_SS$SS_part_error, 6)
  expect_equal(singleOperTest_SS$SS_op_part_error, 0)
  expect_equal(singleOperTest_SS$SS_total_error, 6)

  singleOperTest_anova = anova_var_calcs(data, part = 'SN', operator = 'Operator', meas = 'Measure')
  expect_equal(singleOperTest_anova$repeatability,0)
  expect_equal(singleOperTest_anova$reproducibility,0)
  expect_equal(singleOperTest_anova$total_grr,0)
  expect_equal(singleOperTest_anova$part_to_part,2)
  expect_equal(singleOperTest_anova$total_var,2)

})
