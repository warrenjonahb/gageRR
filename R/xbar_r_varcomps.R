
#' Average and Range Method Repeatability Calculation
#'
#' @param data An R dataframe or tibble.
#' @param part The column in data specifying the unique ID of the part being measured
#' @param operator A column in data specifying the operator for the recorded measurement
#' @param meas A column in data where the measurement value is recorded.
#'
#' @return A number. The measure of repeatability for the given data.
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
#'xbar_repeat(data, part = SN, operator = Operator, meas = Measure)

xbar_repeat = function(data, part, operator, meas){
  reps = data %>%
    dplyr::select({{part}}, {{operator}}) %>%
    dplyr::group_by({{part}}, {{operator}}) %>%
    dplyr::summarize(rep = n(), .groups = 'keep')

  #Small correction here need equal number of reps across each operator/part combo
  if (length(unique(reps$rep)) != 1) {
    stop("Each part must have an equal number of replicates")
  }

  reps = unique(reps$rep)

  a = data %>%
    dplyr::select({{part}}) %>%
    distinct() %>%
    count()

  k = data %>%
    dplyr::select({{operator}}) %>%
    distinct() %>%
    count()

  g1 = as.integer(a*k)

  d = d2_minitab_df(m = reps, g = g1)

  xbar_rep = data %>%
    dplyr::select({{part}}, {{operator}}, {{meas}}) %>%
    dplyr::group_by({{part}}, {{operator}}) %>%
    dplyr::summarize(repeatbility = (max({{meas}})-min({{meas}}))/(a*k), .groups = 'keep')

  repeatability = (sum(xbar_rep$repeatbility)/d)^2 #Squaring to return varComp not SD

  return(repeatability)

}

#' Average and Range Method Reproducibility Calculation
#'
#' @param data An R dataframe or tibble.
#' @param part The column in data specifying the unique ID of the part being measured
#' @param operator A column in data specifying the operator for the recorded measurement
#' @param meas A column in data where the measurement value is recorded.
#'
#' @return A number. The measure of reproducibility for the given data.
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
#'xbar_reproduce(data, part = SN, operator = Operator, meas = Measure)

xbar_reproduce = function(data, part, operator, meas){

  reps = data %>%
    dplyr::select({{part}}, {{operator}}) %>%
    dplyr::group_by({{part}}, {{operator}}) %>%
    dplyr::summarize(rep = n(), .groups = 'keep')

  if (length(unique(reps$rep)) != 1) {
    stop("Each part must have an equal number of replicates")
  }

  r = unique(reps$rep)

  a = data %>%
    dplyr::select({{part}}) %>%
    distinct() %>%
    count() %>%
    as.integer()

  m1 = data %>%
    dplyr::select({{operator}}) %>%
    distinct() %>%
    count() %>%
    as.integer()

  #Insert code here for stop. If m1 =1 then repeatability is 0.
  if (m1 == 1) {
    reproducibility = 0
  } else {

  d = d2_minitab_df(m = m1, g = 1)

  xbar_i = data %>%
    dplyr::select({{operator}}, {{meas}}) %>%
    dplyr::group_by({{operator}}) %>%
    dplyr::summarize(op_avg = mean({{meas}}), .groups = 'keep')

  x_diff = max(xbar_i$op_avg) - min(xbar_i$op_avg)

  repeatability = xbar_repeat(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})

  if(m1 == 1){
    reproducibility = 0
  }else  if((x_diff *1/d)^2 - (repeatability^2/(a*r)) > 0) {
    reproducibility = (x_diff *1/d)^2 - (repeatability^2/(a*r))
  }else{
    reproducibility = 0
  }}
  return(reproducibility)

}

#' Average and Range Method Part to Part Variance Calculation
#'
#' @param data An R dataframe or tibble.
#' @param part The column in data specifying the unique ID of the part being measured
#' @param meas A column in data where the measurement value is recorded.
#'
#' @return A number. The measure of part to part variation for the given data.
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
#'part_to_part(data, part = SN, meas = Measure)

part_to_part = function(data, part, meas){

  a = data %>%
    dplyr::select({{part}}) %>%
    distinct() %>%
    count() %>%
    as.integer()

  d = d2_minitab_df(m = a, g = 1)

  part_meas = data %>%
    dplyr::group_by({{part}}) %>%
    dplyr::summarize(avg_meas = mean({{meas}}), .groups = 'keep')

    if(a == 1) {
      r_p = 0
    }else {
  r_p = ((max(part_meas$avg_meas) - min(part_meas$avg_meas))/d)^2}

  return(r_p)

}

#' Average and Range Method Variance Component Summary
#'
#' @param data An R dataframe or tibble.
#' @param part The column in data specifying the unique ID of the part being measured
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
#'xbar_varcomps(data, part = SN, operator = Operator, meas = Measure)

xbar_varcomps = function(data, part, operator, meas)  {

  repeatability = xbar_repeat(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
  reproducibility = xbar_reproduce(data = {{data}}, part = {{part}}, operator = {{operator}}, meas = {{meas}})
  part_to_part = part_to_part(data = {{data}}, part = {{part}}, meas = {{meas}})
  total_grr = repeatability + reproducibility
  total_var = total_grr + part_to_part

  return(list(
    total_grr = total_grr,
    repeatability = repeatability,
    reproducibility = reproducibility,
    part_to_part = part_to_part,
    total_var = total_var
  ))
}
