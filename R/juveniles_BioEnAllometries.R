#'
#' @title Calculate the allometric component of the respiration relationship
#'
#' @description Function to calculate the allometric component of the respiration relationship.
#'
#' @param w - weight (g)
#' @param AM - adjustment for average metabolic activity
#' @param AR - scale in allometric relationship
#' @param BR - weight exponent in allometric equation
#' @param O  - conversion for calorific equivalent of oxygen consumption (13560 J/g O2)
#'
#' @return value in J/g fish/d
#'
#' @details Based on allometric part of eq. S2 and values in Table 1
#' in the Supplement to Hurst et al. 2018: \cr
#' \eqn{Ro = AM * AR * w^BR}  [g O2/g fish/d]
#' \eqn{R = Ro * O}           [   J/g fish/d]
#'
#' @family juvenile bioenergetic functions
#'
#' @export
#'
juv_Ra<-function(w,
                 AM=1.0,
                 AR=0.003,
                 BR=-0.291,
                 O=13560){
  r<-AM*(AR*(w^BR));#--in   g O2/g fish/day
  r<-r * O;         #--in      J/g fish/day
  return(r);
}

#'
#' @title Calculate the allometric component of consumption
#'
#' @description Function to calculate the allometric component of consumption.
#'
#' @param w - weight (g)
#' @param P - fraction of maximum consumption actually consumed
#' @param AC - scale in allometric relationship
#' @param BC - weight exponent in allometric equation
#' @param EDp - energy density of prey [J/g prey]
#'
#' @return value (in J/g fish/d)
#'
#' @details Based on allometric part of eq. S3 and values in Table 1
#' in the Supplement to Hurst et al. 2018:\cr
#' \eqn{  Cg = P * AC * w^BC }   [g prey/g fish/d]
#' \eqn{  Cj = Cg * EDp}         [   J/g fish/d]
#'
#'
#' @family juvenile bioenergetic functions
#'
#' @export
#'
juv_Ca<-function(w,
                 P=1.0,
                 AC=0.063,
                 BC=-0.070,
                 EDp=4113){
  c <- P*(AC*w^BC); #--in g prey/g fish/d
  c <- c * EDp;     #--in      J/g fish/d
  return(c);
}

