#TODO
#Source data.R script for g/m vals
source("data.R")

xbar_repeat = function(data, part, operator, meas){
  reps = data %>%
    select({{part}}, {{operator}}) %>%
    group_by({{part}}, {{operator}}) %>%
    summarize(rep = n(), .groups = 'keep')

  #Small correction here need equal number of reps across each operator/part combo
  if (length(unique(reps$rep)) != 1) {
    stop("Each part must have an equal number of replicates")
  }

  reps = unique(reps$rep)

  a = data %>%
    select({{part}}) %>%
    distinct() %>%
    count()

  k = data %>%
    select({{operator}}) %>%
    distinct() %>%
    count()

  g1 = as.integer(a*k)

  d = d_table %>%
    filter(g == g1 & m == reps) %>%
    select(d2) %>%
    as.double()

  xbar_rep = data %>%
    select({{part}}, {{operator}}, {{meas}}) %>%
    group_by({{part}}, {{operator}}) %>%
    summarize(repeatbility = (max({{meas}})-min({{meas}}))/(a*k), .groups = 'keep')

  repeatability = (sum(xbar_rep$repeatbility)/d)^2 #Squaring to return varComp not SD

  return(repeatability)

}

xbar_reproduce = function(data, part, operator, meas){

  reps = data %>%
    select({{part}}, {{operator}}) %>%
    group_by({{part}}, {{operator}}) %>%
    summarize(rep = n(), .groups = 'keep')

  #Small correction here need equal number of reps across each operator/part combo
  if (length(unique(reps$rep)) != 1) {
    stop("Each part must have an equal number of replicates")
  }

  r = unique(reps$rep)

  a = data %>%
    select({{part}}) %>%
    distinct() %>%
    count() %>%
    as.integer()

  m1 = data %>%
    select({{operator}}) %>%
    distinct() %>%
    count() %>%
    as.integer()

  d = d_table %>%
    filter(g == 1 & m == m1) %>%
    select(d2) %>%
    as.double()

  xbar_i = data %>%
    select({{operator}}, {{meas}}) %>%
    group_by({{operator}}) %>%
    summarize(op_avg = mean({{meas}}), .groups = 'keep')

  x_diff = max(xbar_i$op_avg) - min(xbar_i$op_avg)

  repeatability = xbar_repeat(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})

  if((x_diff *1/d)^2 - (repeatability^2/(a*r)) > 0) {
    reproducibility = (x_diff *1/d)^2 - (repeatability^2/(a*r))
  }else{
    reproducibility = 0
  }
  return(reproducibility)

}

#Need to pass inputs to ss_calcs and get back g and m.
part_to_part = function(data, part, meas){

  a = data %>%
    select({{part}}) %>%
    distinct() %>%
    count() %>%
    as.integer()

  d = d_table %>%
    filter(g == 1 & m == a) %>%
    select(d2) %>%
    as.double()

  part_meas = data %>%
    group_by({{part}}) %>%
    summarize(avg_meas = mean({{meas}}), .groups = 'keep')

  r_p = ((max(part_meas$avg_meas) - min(part_meas$avg_meas))/d)^2

  return(r_p)

}

xbar_varcomps = function(data, part, operator, meas)  {

  repeatability = xbar_repeat(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
  reproducibility = xbar_reproduce(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
  part_to_part = part_to_part(data = {{data}}, part = {{part}}, meas = {{meas}})
  total_grr = repeatability + reproducibility
  total_var = total_grr + part_to_part

  return(list(
    repeatability = repeatability,
    reproducibility = reproducibility,
    total_grr = total_grr,
    part_to_part = part_to_part,
    total_var = total_var
  ))
}
