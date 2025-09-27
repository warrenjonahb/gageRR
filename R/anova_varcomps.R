#' ANOVA Sum of Squares Calculations
#'
#' @param data An R dataframe or tibble containing the required identifier and measurement columns.
#' @param part A column in data specifying the unique ID of the part being measured. The column should be a character or
#'   factor vector.
#' @param operator A column in data specifying the operator for the recorded measurement. The column should be a
#'   character or factor vector.
#' @param meas A column in data where the measurement value is recorded. The column must be numeric and contain no
#'   missing or infinite values.
#'
#' @return A list of numeric values for the sum of squares error for operator, part, equipment, operator and part interaction, and total error.
#' @export
#'
#' @examples
#' data = data.frame(
#' SN = c(
#' 'SerialNumber_01',
#' 'SerialNumber_01',
#' 'SerialNumber_02',
#' 'SerialNumber_02',
#' 'SerialNumber_01',
#' 'SerialNumber_01',
#' 'SerialNumber_02',
#' 'SerialNumber_02'),
#'
#' Operator = c(
#' 'Operator_01',
#' 'Operator_01',
#' 'Operator_01',
#' 'Operator_01',
#' 'Operator_02',
#' 'Operator_02',
#' 'Operator_02',
#' 'Operator_02'),
#'
#'Measure = c(
#' 0.0172,
#' 0.0177,
#' 0.0155,
#' 0.0159,
#' 0.0174,
#' 0.0181,
#' 0.0152,
#' 0.0176))
#'
#'ss_calcs(data, part = 'SN', operator = 'Operator', meas = 'Measure')

ss_calcs <- function(data, part, operator, meas) {
  data <- validate_grr_inputs(data, part_col = part, operator_col = operator, measure_col = meas)

  # count reps per part/operator
  reps <- aggregate(data[[meas]],
                    by = list(data[[part]], data[[operator]]),
                    FUN = length)

  num_parts <- length(unique(data[[part]]))
  num_opers <- length(unique(data[[operator]]))
  expected_cells <- num_parts * num_opers

  if (nrow(reps) != expected_cells) {
    stop("Balanced studies require every operator to measure every part.")
  }


  if (length(unique(reps$x)) != 1) {
    stop("Each part must have an equal number of replicates")
  }
  r <- unique(reps$x)

  if (r < 2) {
    stop("At least two replicates per part/operator are required.", call. = FALSE)
  }

  overall_mean <- mean(data[[meas]])

  # Operator SS
  op_means <- tapply(data[[meas]], data[[operator]], mean)
  SS_oper_error <- sum((op_means - overall_mean)^2) * num_parts * r

  # Part SS
  part_means <- tapply(data[[meas]], data[[part]], mean)
  SS_part_error <- sum((part_means - overall_mean)^2) * num_opers * r

  # Total SS
  SS_total_error <- sum((data[[meas]] - overall_mean)^2)

  # Equipment (repeatability)
  op_part_means <- aggregate(data[[meas]],
                             by = list(data[[operator]], data[[part]]),
                             FUN = mean)
  merged <- merge(data, op_part_means,
                  by.x = c(operator, part),
                  by.y = c("Group.1", "Group.2"))
  SS_equip_error <- sum((merged$x - merged[[meas]])^2) # deviation within op*part

  # Operator*Part interaction
  SS_op_part_error <- SS_total_error - (SS_oper_error + SS_part_error + SS_equip_error)

  return(list(
    reps = as.integer(r),
    num_parts = as.integer(num_parts),
    num_opers = as.integer(num_opers),
    SS_oper_error = as.double(SS_oper_error),
    SS_part_error = as.double(SS_part_error),
    SS_equip_error = as.double(SS_equip_error),
    SS_op_part_error = as.double(SS_op_part_error),
    SS_total_error = as.double(SS_total_error)
  ))
}


#' ANOVA Variance Component Calculations
#'
#' @param data An R dataframe or tibble.
#' @param part A column in data specifying the unique ID of the part being measured
#' @param operator A column in data specifying the operator for the recorded measurement
#' @param meas A column in data where the measurement value is recorded.
#'
#' @return A list of numeric values for repeatability, reproducibility, total GRR, part-to-part, and total variance components.
#' @export
#'
#' @examples
#' data = data.frame(
#' SN = c(
#' 'SerialNumber_01',
#' 'SerialNumber_01',
#' 'SerialNumber_02',
#' 'SerialNumber_02',
#' 'SerialNumber_01',
#' 'SerialNumber_01',
#' 'SerialNumber_02',
#' 'SerialNumber_02'),
#'
#' Operator = c(
#' 'Operator_01',
#' 'Operator_01',
#' 'Operator_01',
#' 'Operator_01',
#' 'Operator_02',
#' 'Operator_02',
#' 'Operator_02',
#' 'Operator_02'),
#'
#'Measure = c(
#' 0.0172,
#' 0.0177,
#' 0.0155,
#' 0.0159,
#' 0.0174,
#' 0.0181,
#' 0.0152,
#' 0.0176))
#'
#'anova_var_calcs(data, part = 'SN', operator = 'Operator', meas = 'Measure')

anova_var_calcs <- function(data, part, operator, meas) {
  ss_comp <- ss_calcs(data, part, operator, meas)

  reps <- ss_comp$reps
  num_parts <- ss_comp$num_parts
  num_opers <- ss_comp$num_opers
  SS_oper_error <- ss_comp$SS_oper_error
  SS_part_error <- ss_comp$SS_part_error
  SS_equip_error <- ss_comp$SS_equip_error
  SS_op_part_error <- ss_comp$SS_op_part_error
  SS_total_error <- ss_comp$SS_total_error

  MS_oper <- if (num_opers == 1) 0 else SS_oper_error / (num_opers - 1)
  MS_part <- if (num_parts == 1) 0 else SS_part_error / (num_parts - 1)
  MS_oper_part <- if (num_parts == 1 | num_opers == 1) 0 else SS_op_part_error / ((num_opers - 1) * (num_parts - 1))
  MS_equip <- SS_equip_error / (num_parts * num_opers * (reps - 1))

  # Compute p-value for interaction
  if (num_parts == 1 | num_opers == 1) {
    p_val <- NULL
  } else {
    F_stat <- MS_oper_part / MS_equip
    p_val <- stats::pf(F_stat,
                       df1 = as.integer((num_opers - 1) * (num_parts - 1)),
                       df2 = as.integer(num_parts * num_opers * (reps - 1)),
                       lower.tail = FALSE)
  }

  if (!is.null(p_val) && p_val < .05) {
    MS_equip <- (SS_equip_error ) / (num_parts * num_opers * (reps - 1))

    var_oper <- max((MS_oper - MS_oper_part) / (reps * num_parts), 0)
  }

  if (is.null(p_val) || p_val > .05) {
    MS_equip <- (SS_equip_error + SS_op_part_error) /
      (((num_opers - 1) * (num_parts - 1)) + (num_parts * num_opers * (reps - 1)))

    var_oper <- max((SS_op_part_error + SS_equip_error) /
                      ((num_opers-1)*(num_parts-1)+num_opers*num_parts*(reps-1)), 0)

  }

  var_repeat <- max(MS_equip, 0)
  var_oper_part <- max((MS_part - MS_equip) / reps, 0)
  var_part <- max((MS_part - MS_oper_part) / (reps * num_opers), 0)


  repeatability <- var_repeat
  reproducibility <- var_oper + var_oper_part
  total_grr <- repeatability + reproducibility
  part_to_part <- var_part
  total_var <- total_grr + part_to_part

  return(list(
    total_grr = total_grr,
    repeatability = repeatability,
    reproducibility = reproducibility,
    part_to_part = part_to_part,
    total_var = total_var
  ))
}


#' ANOVA Table Calculation
#'
#' @param data An R dataframe or tibble.
#' @param part A column in data specifying the unique ID of the part being measured
#' @param operator A column in data specifying the operator for the recorded measurement
#' @param meas A column in data where the measurement value is recorded.
#'
#' @return An anova table of meas ~ operator x part
#' @export
#'
#' @examples
#' data = data.frame(
#' SN = c(
#' 'SerialNumber_01',
#' 'SerialNumber_01',
#' 'SerialNumber_02',
#' 'SerialNumber_02',
#' 'SerialNumber_01',
#' 'SerialNumber_01',
#' 'SerialNumber_02',
#' 'SerialNumber_02'),
#'
#' Operator = c(
#' 'Operator_01',
#' 'Operator_01',
#' 'Operator_01',
#' 'Operator_01',
#' 'Operator_02',
#' 'Operator_02',
#' 'Operator_02',
#' 'Operator_02'),
#'
#'Measure = c(
#' 0.0172,
#' 0.0177,
#' 0.0155,
#' 0.0159,
#' 0.0174,
#' 0.0181,
#' 0.0152,
#' 0.0176))
#'
#'anova_table(data, part = 'SN', operator = 'Operator', meas = 'Measure')

anova_table <- function(data, part, operator, meas) {
  # build formula from strings
  form_text <- sprintf("%s ~ %s * %s", meas, operator, part)
  form <- as.formula(form_text, env = parent.frame())

  # run ANOVA
  anova_stats <- aov(form, data = data)
  summary(anova_stats)
}
