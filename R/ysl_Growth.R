#'
#' @title Calculate the "point-of-no-return" for non-feeding yolk-sac larvae after yolk-sac absorption
#'
#' @description Function to calculate the "point-of-no-return" for
#' non-feeding yolk-sac larvae after yolk-sac absorption.
#'
#' @param T - temperature (deg C)
#'
#' @return vector of times to point-of-no-return for non-feeding YSL.
#'
#' @details None.
#'
#' @export
#'
ysl_PNR<-function(T){
  PNR <- 34.67*exp(-0.126*T);#in days
  return(PNR);
}

#'
#' @title Calculate time to yolk-sac absorption for yolk-sac larvae
#'
#' @description Function to calculate time to yolk-sac absorption for yolk-sac larvae.
#'
#' @param T - temperature (deg C)
#'
#' @return vector of times to yolk sac absorption
#'
#' @details None.
#'
#' @export
#'
ysl_YolkSacAbsorption<-function(T){
  ysa <- 14.7662*exp(-0.235*T);#in days
  return(ysa);
}

#'
#' @title Calculate growth rate (mm/day SL) for yolk-sac larvae
#'
#' @description Function to calculate growth rate (mm/day SL) for yolk-sac larvae.
#'
#' @param T - temperature (deg C)
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 2-11 deg C.
#'
#' @export
#'
ysl_GrowthRate_SL<-function(T){
  gL <- 0.0179 + (0.015*T) - (0.00001*T^2);#--Hurst et al 2010, preflexion eq, mm per day
  return(gL);
}

#'
#' @title Calculate growth rate (1/day dry weight) for yolk-sac larvae
#'
#' @description Function to calculate growth rate (1/day dry weight) for yolk-sac larvae.
#'
#' @param T - temperature (deg C)
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 2-11 deg C.
#'
#' @export
#'
ysl_GrowthRate_DW<-function(T){
  gM <- 2.990 + 0.772*T - 0.077*T^2;#--Hurst et al 2010, same as preflexion eq, per day
  return(gM);
}
