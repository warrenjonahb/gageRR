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

test_that("ss_calcs requires at least two replicates per part/operator", {
  data <- data.frame(
    SN = c(
      "SerialNumber_01",
      "SerialNumber_02",
      "SerialNumber_01",
      "SerialNumber_02"
    ),
    Operator = c(
      "Operator_01",
      "Operator_02",
      "Operator_02",
      "Operator_01"
    ),
    Measure = c(1, 2, 2, 1)
  )

  expect_error(
    ss_calcs(data, part = "SN", operator = "Operator", meas = "Measure"),
    "At least two replicates per part/operator are required.",
    fixed = TRUE
  )
})

test_that("ANOVA requires every operator-part combination", {
  data <- data.frame(
    SN = c(
      "SerialNumber_01",
      "SerialNumber_01",
      "SerialNumber_02",
      "SerialNumber_02",
      "SerialNumber_01",
      "SerialNumber_01"
    ),
    Operator = c(
      "Operator_01",
      "Operator_01",
      "Operator_01",
      "Operator_01",
      "Operator_02",
      "Operator_02"
    ),
    Measure = c(1, 1.1, 2, 2.1, 1.5, 1.6)
  )

  expect_error(
    ss_calcs(data, part = "SN", operator = "Operator", meas = "Measure"),
    "Balanced studies require every operator to measure every part."
  )

  expect_error(
    anova_var_calcs(data, part = "SN", operator = "Operator", meas = "Measure"),
    "Balanced studies require every operator to measure every part."
  )
})
