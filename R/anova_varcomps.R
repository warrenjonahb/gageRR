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
#'   When the study is unbalanced (different replicate counts per operator/part combination) the \code{reps} element is returned
#'   as \code{NA}.
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

  if (any(reps$x < 2)) {
    stop("At least two replicates per part/operator are required.", call. = FALSE)
  }

  overall_mean <- mean(data[[meas]])

  # Operator SS (weighted by number of measurements per operator)
  op_means <- tapply(data[[meas]], data[[operator]], mean)
  op_counts <- table(data[[operator]])
  SS_oper_error <- sum(op_counts * (op_means - overall_mean)^2)

  # Part SS (weighted by number of measurements per part)
  part_means <- tapply(data[[meas]], data[[part]], mean)
  part_counts <- table(data[[part]])
  SS_part_error <- sum(part_counts * (part_means - overall_mean)^2)

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

  # No Operator*Part interaction
  SS_no_interaction <- SS_op_part_error + SS_equip_error

  reps_value <- unique(reps$x)
  reps_out <- if (length(reps_value) == 1) as.integer(reps_value) else NA_integer_

  return(list(
    reps = reps_out,
    num_parts = as.integer(num_parts),
    num_opers = as.integer(num_opers),
    SS_oper_error = as.double(SS_oper_error),
    SS_part_error = as.double(SS_part_error),
    SS_equip_error = as.double(SS_equip_error),
    SS_op_part_error = as.double(SS_op_part_error),
    SS_no_interaction = as.double(SS_no_interaction),
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
#'   Variance components are estimated with a random-effects model fit via restricted maximum likelihood so that unequal
#'   replicate counts per operator/part combination are supported.
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
  data <- validate_grr_inputs(data, part_col = part, operator_col = operator, measure_col = meas)

  # Ensure the identifiers are factors so that mixed-model fits have
  # consistent behaviour across R versions
  data[[part]] <- as.factor(data[[part]])
  if (!is.null(operator)) {
    data[[operator]] <- as.factor(data[[operator]])
  }

  ss <- ss_calcs(data, part = part, operator = operator, meas = meas)

  reps <- ss$reps
  num_parts <- ss$num_parts
  num_opers <- ss$num_opers

  balanced_design <- !is.na(reps)

  if (balanced_design) {
    reps_per_cell <- reps

    df_repeat <- num_parts * num_opers * (reps_per_cell - 1)
    df_oper <- max(num_opers - 1, 0)
    df_part <- max(num_parts - 1, 0)
    df_interaction <- max((num_parts - 1) * (num_opers - 1), 0)

    ms_repeat <- if (df_repeat > 0) ss$SS_equip_error / df_repeat else 0
    ms_part <- if (df_part > 0) ss$SS_part_error / df_part else 0
    ms_oper <- if (df_oper > 0) ss$SS_oper_error / df_oper else 0
    ms_interaction <- if (df_interaction > 0) ss$SS_op_part_error / df_interaction else NA_real_
    ms_interaction_raw <- ms_interaction

    pooled <- !is.na(ms_interaction) && df_interaction > 0 && ms_interaction <= ms_repeat

    if (pooled) {
      df_no_interaction <- df_repeat + df_interaction
      ms_repeat <- if (df_no_interaction > 0) ss$SS_no_interaction / df_no_interaction else ms_repeat
      ms_interaction <- ms_repeat
    }

    repeatability <- max(ms_repeat, 0)

    interaction_var <- if (!is.na(ms_interaction) && df_interaction > 0 && !pooled) {
      max((ms_interaction - ms_repeat) / reps_per_cell, 0)
    } else {
      0
    }

    ms_for_operator <- if (!is.na(ms_interaction_raw) && df_interaction > 0 && !pooled) ms_interaction_raw else ms_repeat

    operator_var <- if (num_opers > 1) {
      max((ms_oper - ms_for_operator) / (num_parts * reps_per_cell), 0)
    } else {
      0
    }

    reproducibility <- operator_var + interaction_var

    ms_for_part <- if (!is.na(ms_interaction_raw) && df_interaction > 0) ms_interaction_raw else ms_repeat

    part_to_part <- if (num_parts > 1) {
      max((ms_part - ms_for_part) / (num_opers * reps_per_cell), 0)
    } else {
      0
    }

    total_grr <- repeatability + reproducibility
    total_var <- total_grr + part_to_part

    return(list(
      total_grr = total_grr,
      repeatability = repeatability,
      reproducibility = reproducibility,
      part_to_part = part_to_part,
      total_var = total_var
    ))
  }

  random_terms <- character()

  if (num_parts > 1) {
    random_terms <- c(random_terms, sprintf("(1|%s)", part))
  }

  if (num_opers > 1) {
    random_terms <- c(random_terms, sprintf("(1|%s)", operator))
  }

  if (num_parts > 1 && num_opers > 1) {
    random_terms <- c(random_terms, sprintf("(1|%s:%s)", operator, part))
  }

  form_text <- paste(meas, "~ 1")

  if (length(random_terms) > 0) {
    form_text <- paste(form_text, "+", paste(random_terms, collapse = " + "))
  }

  form <- stats::as.formula(form_text, env = parent.frame())

  lmm_fit <- tryCatch(
    lme4::lmer(form, data = data, REML = TRUE),
    error = function(e) {
      stop("Failed to fit mixed-effects model for variance components: ", e$message, call. = FALSE)
    }
  )

  vc <- lme4::VarCorr(lmm_fit)
  vc_df <- as.data.frame(vc)

  get_var <- function(group) {
    idx <- which(vc_df$grp == group)
    if (length(idx) == 0) {
      return(0)
    }
    vc_df$vcov[idx[1]]
  }

  interaction_term <- sprintf("%s:%s", operator, part)

  var_repeat <- attr(vc, "sc")^2
  var_part <- if (num_parts > 1) get_var(part) else 0
  var_oper <- if (num_opers > 1) get_var(operator) else 0
  var_oper_part <- if (num_parts > 1 && num_opers > 1) get_var(interaction_term) else 0

  repeatability <- max(var_repeat, 0)
  reproducibility <- max(var_oper, 0) + max(var_oper_part, 0)
  total_grr <- repeatability + reproducibility
  part_to_part <- max(var_part, 0)
  total_var <- total_grr + part_to_part

  list(
    total_grr = total_grr,
    repeatability = repeatability,
    reproducibility = reproducibility,
    part_to_part = part_to_part,
    total_var = total_var
  )
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
