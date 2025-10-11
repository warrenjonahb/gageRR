test_that("grr_calc validates required measurement column", {
  data <- data.frame(
    SN = rep(c("SerialNumber_01", "SerialNumber_02"), each = 2),
    Operator = rep(c("Operator_01", "Operator_02"), times = 2)
  )

  expect_error(
    grr_calc(data, part = "SN", operator = "Operator", meas = "Measure"),
    "missing"
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

test_that("validate_grr_inputs coerces numeric identifiers to character", {
  data <- data.frame(
    SN = c(101, 101, 102, 102),
    Operator = c(1, 1, 2, 2),
    Measure = c(0.1, 0.2, 0.3, 0.4)
  )

  validated <- gageRR:::validate_grr_inputs(
    data,
    part_col = "SN",
    operator_col = "Operator",
    measure_col = "Measure"
  )

  expect_type(validated$SN, "character")
  expect_type(validated$Operator, "character")
})

test_that("grr_calc accepts numeric identifiers by coercing them", {
  data <- data.frame(
    SN = rep(1:2, each = 4),
    Operator = rep(1:2, each = 2, times = 2),
    Measure = c(0.0172, 0.0177, 0.0155, 0.0159, 0.0174, 0.0181, 0.0152, 0.0176)
  )

  expect_no_error(
    grr_calc(data, part = "SN", operator = "Operator", meas = "Measure")
  )
})
