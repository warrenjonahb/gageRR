test_that("xbar_r varcomp test", {
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

test_that("xbar reproducibility matches manual calculation for balanced 2x2 design", {
  data <- data.frame(
    SN = rep(c("Part1", "Part2"), each = 4),
    Operator = rep(c("Op1", "Op2", "Op1", "Op2"), each = 2),
    Measure = c(10, 11, 8, 9, 12, 13, 10, 11)
  )

  a <- length(unique(data$SN))

  rep_counts <- aggregate(data$Measure,
                          by = list(data$SN, data$Operator),
                          FUN = length)
  r <- unique(rep_counts$x)
  expect_equal(length(r), 1)
  r <- r[[1]]

  m1 <- length(unique(data$Operator))

  if (m1 <= 20){
    d <- d_table[d_table$g == 1 & d_table$m == m1, "d2"]
  } else{
    d <- d2_minitab_df(m = m1, g = 1)
  }

  operator_means <- aggregate(data$Measure,
                              by = list(data$Operator),
                              FUN = mean)
  x_diff <- max(operator_means$x) - min(operator_means$x)

  repeatability <- xbar_repeat(data, part = "SN", operator = "Operator", meas = "Measure")

  expected_reproducibility <- max((x_diff / d)^2 - (repeatability / (a * r)), 0)

  expect_equal(
    xbar_reproduce(data, part = "SN", operator = "Operator", meas = "Measure"),
    expected_reproducibility
  )
})
