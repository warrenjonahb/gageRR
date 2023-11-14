ss_calcs = function(data, part, operator = NULL, meas){

  if (is.null(data$operator)){
    data$operator = 'A'}

    reps = data %>%
      select({{part}}, {{operator}}) %>%
      group_by({{part}}, {{operator}}) %>%
      summarize(rep = n())

  #Small correction here need equal number of reps across each operator/part combo
    if (length(unique(reps$rep)) != 1) {
      stop("Each part must have an equal number of replicates")
    }

    r = unique(reps$rep)

    p = data %>%
        select({{part}}) %>%
        distinct() %>%
        count()

    t = data %>%
      select({{operator}}) %>%
      distinct() %>%
      count()

    SS_oper = data %>%
      mutate(overall_mean = mean({{meas}})) %>%
      group_by({{operator}}) %>%
      mutate(op_mean = mean({{meas}})) %>%
      mutate(sq_error = (op_mean - overall_mean)^2)

    SS_oper_error = sum(SS_oper$sq_error)

    SS_part = data %>%
      mutate(overall_mean = mean({{meas}})) %>%
      group_by({{part}}) %>%
      mutate(part_mean = mean({{meas}})) %>%
      mutate(sq_error = (part_mean -overall_mean)^2)

    SS_part_error = sum(SS_part$sq_error)

    SS_total = data %>%
      mutate(overall_mean = mean({{meas}})) %>%
      mutate(sq_error = ({{meas}} - overall_mean)^2)

    SS_total_error = sum(SS_total$sq_error)

    SS_equip = data %>%
      group_by({{operator}}, {{part}}) %>%
      mutate(op_part_mean = mean({{meas}})) %>%
      mutate(sq_error = (op_part_mean - {{meas}})^2)

    SS_equip_error = sum(SS_equip$sq_error)

    SS_op_part_error = SS_total_error - (SS_oper_error + SS_part_error + SS_equip_error)

    return(list(reps = as.integer(r), num_parts = as.integer(p), num_opers = as.integer(t),
                SS_oper_error = as.double(SS_oper_error),
                SS_part_error = as.double(SS_part_error),
                SS_equip_error = as.double(SS_equip_error),
                SS_op_part_error = as.double(SS_op_part_error),
                SS_total_error = as.double(SS_total_error)))
}

#Change inputs here to requirements to veed into ss_calcs
anova_var_calcs = function(data, part, operator = NULL, meas)  {

  ss_comp = ss_calcs(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})

  reps = ss_comp$reps
  num_parts = ss_comp$num_parts
  num_opers = ss_comp$num_opers
  SS_oper_error = ss_comp$SS_oper_error
  SS_part_error = ss_comp$SS_part_error
  SS_equip_error = ss_comp$SS_equip_error
  SS_op_part_error = ss_comp$SS_op_part_error
  SS_total_error = ss_comp$SS_total_error

  MS_oper = SS_oper_error/(num_opers - 1)
  MS_part = SS_part_error/(num_parts - 1)
  MS_oper_part = SS_op_part_error/((num_opers - 1)*(num_parts - 1))
  MS_equip = SS_equip_error / (num_parts * num_opers * (reps-1))

  #compute p-val part_oper interaction
  F_stat = MS_oper_part/(MS_equip)
  p_val = pf(F_stat[[1]],
             df1 = as.integer((num_opers - 1)*(num_parts - 1)),
             df2 = as.integer(num_parts * num_opers * (reps-1)),
            lower.tail = FALSE    )

  if (p_val < .05) {
    MS_equip = (SS_equip_error + SS_op_part_error)/((num_opers - 1)*(num_parts - 1)+(num_parts * num_opers * (reps-1)))
  }else{
    MS_equip = SS_equip_error / (num_parts * num_opers * (reps-1))
    }

  var_repeat = MS_equip
  var_oper_part = (MS_part - MS_equip)/reps
  var_part = (MS_part - MS_oper_part)/(reps * num_opers)
  var_oper = (MS_oper - MS_oper_part)/(reps * num_parts)

  if (p_val > .05) {
    var_oper_part = 0
  }

  if(var_repeat<0){var_repeat=0}
  if(var_oper_part<0){var_oper_part=0}
  if(var_part<0){var_part=0}
  if(var_oper<0){var_oper=0}

  repeatability = var_repeat
  reproducibility = var_oper + var_oper_part
  total_grr = repeatability + reproducibility
  part_to_part = var_part
  total_var = total_grr + part_to_part

  return(list(repeatability = as.double(repeatability),
              reproducibility = as.double(reproducibility),
              total_grr = as.double(total_grr),
              part_to_part = as.double(part_to_part),
              total_var = as.double(total_var)))
}
