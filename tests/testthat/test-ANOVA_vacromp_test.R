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

test_that("ss_calcs and ANOVA variance comps handle unbalanced replicate counts", {
  data <- data.frame(
    SN = c(
      rep("SerialNumber_01", 5),
      rep("SerialNumber_02", 7)
    ),
    Operator = c(
      rep("Operator_01", 3),
      rep("Operator_02", 2),
      rep("Operator_01", 4),
      rep("Operator_02", 3)
    ),
    Measure = c(1.00, 1.05, 0.98, 1.90, 1.95, 2.05, 2.00, 2.10, 2.20, 2.15, 2.18, 2.12)
  )

  ss <- ss_calcs(data, part = "SN", operator = "Operator", meas = "Measure")
  expect_true(is.na(ss$reps))
  expect_equal(ss$num_parts, 2)
  expect_equal(ss$num_opers, 2)
  expect_gt(ss$SS_total_error, 0)
  expect_gt(ss$SS_part_error, 0)

  anova_res <- anova_var_calcs(data, part = "SN", operator = "Operator", meas = "Measure")
  expect_true(all(vapply(anova_res, is.numeric, logical(1))))
  expect_gt(anova_res$total_var, 0)
  expect_equal(anova_res$total_grr + anova_res$part_to_part, anova_res$total_var, tolerance = 1e-10)
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
