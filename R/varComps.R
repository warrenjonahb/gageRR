xbar_repeat = function(data, part, operator, meas){
  reps = data %>%
    select(part, operator) %>%
    group_by(part, operator) %>%
    summarize(rep = n())

  if (nrow(reps > 1)) {
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

  return(xbar_rep$repeatbility)

}

xbar_repeat = function(data, part, operator, meas){
  r = data %>%
    select(part, operator) %>%
    group_by(part, operator) %>%
    summarize(rep = n())

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

  x_diff = max(xbar_i$meas) - min(xbar_i$meas)

  reproducibility = xbar_repeat(data = data, part = part, operator = operator, meas = meas)

    sqrt((x_diff *1/d)^2 - (reproducibility^2/(a*r)))


}
