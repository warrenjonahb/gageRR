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

  expect_equal(xbar_repeat(data, part = 'SN', operator = 'Operator', meas = 'Measure'), 0)
  expect_equal(xbar_reproduce(data, part = 'SN', operator = 'Operator', meas = 'Measure'), 0)
  expect_equal(round(part_to_part(data, part = 'SN', meas = 'Measure'),3),2)

  xbar_varcompTest = xbar_varcomps(data, part = 'SN', operator = 'Operator', meas = 'Measure')
  expect_equal(xbar_varcompTest$repeatability,0)
  expect_equal(xbar_varcompTest$reproducibility,0)
  expect_equal(xbar_varcompTest$total_grr,0)
  expect_equal(round(xbar_varcompTest$part_to_part,3),2)
  expect_equal(round(xbar_varcompTest$total_var,3),2)
})
