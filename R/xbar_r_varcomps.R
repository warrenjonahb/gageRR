#TODO
#Source data.R script for g/m vals

xbar_repeat = function(data, part, operator, meas){
  reps = data %>%
    select({{part}}, {{operator}}) %>%
    group_by({{part}}, {{operator}}) %>%
    summarize(rep = n())

  #Small correction here need equal number of reps across each operator/part combo
  if (length(unique(reps$rep)) != 1) {
    stop("Each part must have an equal number of replicates")
  }

  a = data %>%
    select({{part}}) %>%
    distinct() %>%
    count()

  k = data %>%
    select({{operator}}) %>%
    distinct() %>%
    count()

  g1 = a*k

  d = d_table %>%
    filter(g == g1 & m == reps) %>%
    select(d)

  xbar_rep = data %>%
    select({{part}}, {{operator}}, {{meas}}) %>%
    group_by({{part}}, {{operator}}) %>%
    summarize(repeatbility = range({{meas}})/(a*k)*1/d)

  repeatability = sum(xbar_rep$repeatbility)

  return(repeatability)

}

xbar_reproduce = function(data, part, operator, meas){

  reps = data %>%
    select({{part}}, {{operator}}) %>%
    group_by({{part}}, {{operator}}) %>%
    summarize(rep = n())

  #Small correction here need equal number of reps across each operator/part combo
  if (length(unique(reps$rep)) != 1) {
    stop("Each part must have an equal number of replicates")
  }

  r = unique(reps$rep)

  a = data %>%
    select({{part}}) %>%
    distinct() %>%
    count()

  m1 = data %>%
    select({{operator}}) %>%
    distinct() %>%
    count()

  #need to data mask?
  d = d_table %>%
    filter(g == 1 & m == m1)

  xbar_i = data %>%
    select({{operator}}, {{meas}}) %>%
    group_by({{operator}}) %>%
    summarize(op_range = range({{meas}}))

  x_diff = max(xbar_i$op_range) - min(xbar_i$op_range)

  repeatability = xbar_repeat(data = data, part = part, operator = operator, meas = meas)

  reproducibility = sqrt((x_diff *1/d)^2 - (repeatabilty^2/(a*r)))

  return(reproducibility)

}

#Need to pass inputs to ss_calcs and get back g and m.
part_to_part = function(data, part, meas){

  part_meas = data %>%
    group_by({{part}}) %>%
    summarize(avg_meas = mean({{meas}}))

  d = d_table %>%
    filter(g == 1 & m == m)

  r_p = range(part_meas$avg_meas)/d

  return(r_p)

}

xbar_varcomps = function(data, part, operator = NULL, meas)  {

  repeatability = xbar_repeat(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
  reproducibility = xbar_reproduce(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
  part_to_part = part_to_part(data = {{data}}, part = {{part}}, meas = {{meas}})
  total_grr = repeatabiity + reproducibility
  total_var = total_grr + part_to_part

  return(list(
    repeatability,
    reproducibility,
    total_grr,
    part_to_part,
    total_var
  ))
}
