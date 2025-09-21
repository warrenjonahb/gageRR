test_that("grr_calc validates required measurement column", {
  data <- data.frame(
    SN = rep(c("SerialNumber_01", "SerialNumber_02"), each = 2),
    Operator = rep(c("Operator_01", "Operator_02"), times = 2)
  )

  expect_error(
    grr_calc(data, part = "SN", operator = "Operator", meas = "Measure"),
    'Column "Measure" is missing'
  )
})

test_that("grr_calc rejects non-numeric measurements", {
  data <- data.frame(
    SN = rep(c("SerialNumber_01", "SerialNumber_02"), each = 2),
    Operator = rep(c("Operator_01", "Operator_02"), times = 2),
    Measure = rep("bad", 4)
  )

  expect_error(
    grr_calc(data, part = "SN", operator = "Operator", meas = "Measure"),
    "must be numeric"
  )
})

test_that("grr_calc rejects missing measurement values", {
  data <- data.frame(
    SN = rep(c("SerialNumber_01", "SerialNumber_02"), each = 2),
    Operator = rep(c("Operator_01", "Operator_02"), times = 2),
    Measure = c(0.1, NA, 0.2, 0.3)
  )

  expect_error(
    grr_calc(data, part = "SN", operator = "Operator", meas = "Measure"),
    "(NA/NaN)"
  )
})
