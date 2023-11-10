xbar_repeat = function(data, part, operator, meas){
  reps = data %>%
    select(part, operator) %>%
    group_by(part, operator) %>%
    summarize(rep = n())

  if (length(unique(reps$rep)) != 1) {
   stop("Each part must have an equal number of replicates")
  }

  a = data %>%
    select(part) %>%
    distinct() %>%
    count()

  k = data %>%
    select(operator) %>%
    distinct() %>%
    count()

  g = a*k

  #need to data mask?
  d = d_table %>%
    filter(d_table$g == g & d_table$m == reps) %>%
    select(d)

  xbar_rep = data %>%
    select(part, operator, meas) %>%
    group_by(part, operator) %>%
    summarize(repeatbility = range(meas)/(a*k)*1/d)

  repeatability = sum(xbar_rep$repeatbility)

  return(repeatability)

}

xbar_reproduce = function(data, part, operator, meas){
  reps  = data %>%
    select(part, operator) %>%
    group_by(part, operator) %>%
    summarize(rep = n())

  if (length(unique(reps$rep)) != 1) {
    stop("Each part must have an equal number of replicates")
  }

  r = unique(reps$rep)

  a = data %>%
    select(part) %>%
    distinct() %>%
    count()

  m = data %>%
    select(operator) %>%
    distinct() %>%
    count()

  #need to data mask?
  d = d_table %>%
    filter(g == 1 & m == m)

  xbar_i = data %>%
    select(operator, meas) %>%
    group_by(operator) %>%
    summarize(op_range = range(meas))

  x_diff = max(xbar_i$op_range) - min(xbar_i$op_range)

  repeatability = xbar_repeat(data = data, part = part, operator = operator, meas = meas)

  reproducibility = sqrt((x_diff *1/d)^2 - (repeatabilty^2/(a*r)))

  return(reproducibility)

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
