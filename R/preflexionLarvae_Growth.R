#'
#' @title Calculate growth rate (mm/day SL) for preflexion larvae
#'
#' @description Function to calculate growth rate (mm/day SL) for preflexion larvae.
#'
#' @param T - temperature (deg C)
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 2-11 deg C.
#'
#' @export
#'
preflexionLarvae_GrowthRate_SL<-function(T){
  gL <- 0.0179 + (0.015*T) - (0.00001*T^2);#--Hurst et al 2010, preflexion eq, mm per day
  return(gL);
}

#'
#' @title Calculate growth rate (1/day dry weight) for preflexion larvae
#'
#' @description Function to calculate growth rate (1/day dry weight) for preflexion larvae.
#'
#' @param T - temperature (deg C)
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 2-11 deg C.
#'
#' @export
#'
preflexionLarvae_GrowthRate_DW<-function(T){
  gM <- (2.990 + 0.772*T - 0.077*T^2)/100;#--Hurst et al 2010, preflexion eq, per day
  return(gM);
}
