#'
#' @title Calculate observed growth (J/g fish/day) of a (nominally) 6.8 g fish as function of temperature
#'
#' @description Function to calculate observed growth rate (J/g fish/day) of a (nominally) 6.8 g fish as a function of temperature.
#'
#' @param T - temperature (deg C)
#' @param EDc - energy density of Pacific cod juveniles (J/g)
#'
#' @return value for growth rate (fraction wet weight/day) of juvenile Pacific cod weighing (nominally) 6.8 g.
#'
#' @details Based on eq. S8 (from Laurel et al. 2016, in % body mass/day) as
#' given in Supplement to Hurst et al. 2018: \cr
#' \itemize{
#'   \item{\eqn{ GT = [(0.2494 + 0.3216*T - 0.0069*T^2 - 0.0004*T^3)/100]*EDc},[J/g fish/day]}
#' }
#'
#' @family juvenile bioenergetic functions
#'
#' @export
#'
juv_GT<-function(T,EDc=4138){
  gt <- 0.2494 + 0.3216*T - 0.0069*T^2 - 0.0004*T^3;#--in % wet weight per day
  gt <- gt / 100 * EDc;                             #--in J/g fish/day
  return(gt);
}

#'
#' @title Calculate consumed energy (J/g fish/day) of a (nominal) 6.8 g fish as function of temperature
#'
#' @description Function to calculate consumed energy (J/g fish/day) of (nominal) 6.8 g fish as function of temperature.
#'
#' @param T - temperature (deg C)
#' @param W - weight of fish (nominally 6.8 g)
#'
#' @return value of consumed energy in [J/g fish/day]
#'
#' @details Based on eq. S9 (in J/day) in Supplement to Hurst et al. 2018:\cr
#' \eqn{  CT = juv_GT(T)  + juv_Ra(W)*juv_fR(T)}  [J/g fish/day], \cr
#' where W=6.8 g.
#'
#' @family juvenile bioenergetic functions
#'
#' @export
#'
juv_CT<-function(T,
                 W=6.8){
  ct<-juv_GT(T) + juv_Ra(W)*juv_fR(T);#--units in J/g fish/day
  return(ct);
}

#'
#' @title Calculate functional reponse of respiration to temperature
#'
#' @description Function to calculate functional reponse of respiration to temperature.
#'
#' @param T - temperature (deg C)
#'
#' @return value for functional response (units??)
#'
#' @details Based on eq. S5 (from Oh et al. 2010) as
#' given in Supplement to Hurst et al. 2018:
#' \itemize{
#'   \item{\eqn{mt = (-1.04e-5)*T^2 + (3.38e-4)*T + (-1.02e-3)}, units??}
#' }
#'
#' @family juvenile bioenergetic functions
#'
#' @export
#'
juv_MT<-function(T){
  mt<-(-1.04e-5)*T^2 + (3.38e-4)*T + (-1.02e-3); #---units??
  return(mt);
}

