#'
#' @title Calculate the quadratic functional response to temperature in the respiration relationship
#'
#' @description Function to calculate the quadratic functional response to temperature in the respiration relationship.
#'
#' @param t - temperature (deg C)
#' @param a - quadratic coefficient in quadratic equation
#' @param b - linear coefficient in quadratic equation
#' @param c - constant coefficient in quadratic equation
#'
#' @return value
#'
#' @details Based on eq. S6
#' in the Supplement to Hurst et al. 2018\cr
#' \eqn{val = a*t^2+b*t+c}  [unitless]
#'
#' @family juvenile bioenergetic functions
#'
#' @export
#'
juv_fR<-function(t,
                     a=-0.006,
                     b=0.1954,
                     c=-0.5885){
  val<-a*t^2+b*t+c;
  return(val);
}

#'
#' @title Calculate the quadratic functional response to temperature in the consumption relationship
#'
#' @description Function to calculate the quadratic functional response to temperature in the consumption relationship.
#'
#' @param t - temperature (deg C)
#' @param a - quadratic coefficient in quadratic equation
#' @param b - linear coefficient in quadratic equation
#' @param c - constant coefficient in quadratic equation
#'
#' @return value
#'
#' @details Based on eq. S10
#' in the Supplement to Hurst et al. 2018\cr
#' \eqn{val = a*t^2+b*t+c}  [unitless]
#'
#' @family juvenile bioenergetic functions
#'
#' @export
#'
juv_fC<-function(t,
                   a=-0.0076,
                   b=0.1859,
                   c=-0.1424){
  val<-a*t^2+b*t+c;
  return(val);
}

