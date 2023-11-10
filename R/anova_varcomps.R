ss_calcs = function(data, part, operator = NULL, meas){

  if (is.null(operator)){
    data$operator = 'A'}

    reps = data %>%
      select(part, operator) %>%
      group_by(part, operator) %>%
      summarize(rep = n())

    if (length(unique(reps$rep)) != 1) {
      stop("Each part must have an equal number of replicates")
    }

    r = unique(reps$rep)

    p = data %>%
      select(part) %>%
      distinct() %>%
      count()

    t = data %>%
      select(operator) %>%
      distinct() %>%
      count()

    SS_oper = data %>%
      group_by(operator) %>%
      summarize(op_mean = mean(meas))

    SS_oper_error = sum((SS_oper$op_mean - mean(data$meas))^2)

    SS_part = data %>%
      group_by(part) %>%
      summarize(part_mean = mean(meas))

    SS_part_error = sum((SS_part$part_mean - mean(data$meas))^2)

    SS_total_error = sum((data$meas -  mean(data$meas))^2)

    SS_equip = data %>%
      group_by(operator, part) %>%
      summarize(op_part_mean = mean(meas))

    SS_equip_error = sum((SS_equip$op_part_mean - data$meas)^2)

    SS_op_part_error = SS_total_error - (SS_tech_error + SS_part_error + SS_equip_error)

    return(list(reps = r, num_parts = p, num_opers = t,
                SS_oper_error = SS_oper_error,
                SS_part_error=SS_part_error,
                SS_equip_error = SS_equip_error,
                SS_op_part_error = SS_op_part_error,
                SS_total_error = SS_total_error))
}

var_calcs = function(data, reps, num_parts, num_opers, SS_oper_error, SS_part_error, SS_equip_error, SS_op_part_error)  {

  MS_oper = SS_oper_error/(num_opers - 1)
  MS_part = SS_part_error/(num_parts - 1)
  MS_oper_part = SS_op_part_error/((num_opers - 1)*(num_parts - 1))
  MS_equip = SS_equip_error / (num_part * num_opers * (reps-1))

  var_repeat = MS_equip
  var_oper_part = (MS_part - MS_equip)/reps
  var_part = (MS_part - MS_oper_part)/(reps * num_opers)
  var_tech = (MS_tech - MS_oper_part)/(reps * num_parts)

  if(var_repeat<0){var_repeat=0}
  if(var_oper_part<0){var_oper_part=0}
  if(var_part<0){var_part=0}
  if(var_tech<0){var_tech=0}
}
