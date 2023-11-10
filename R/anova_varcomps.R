ss_calcs = function(data, part, operator = NULL, meas){

  if (!is.null(operator)){

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

    SS_op_part = SS_total_error - (SS_tech_error + SS_part_error + SS_equip_error)

    repeatVar =
}

}
part_to_part = function(data, part, meas){

  part_meas = data %>%
    group_by(part) %>%
    summarize(avg_meas = mean(meas))

  d = d_table %>%
    filter(g == 1 & m == m)

  r_p = range(part_meas$avg_meas)/d

  return(r_p)

}
