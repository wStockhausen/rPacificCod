#'
#' @title Calculate growth rate (mm/day TL) for juvenile larvae
#'
#' @description Function to calculate growth rate (mm/day SL) for juvenile larvae.
#'
#' @param T - temperature (deg C)
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 3-11 deg C.
#'
#' @export
#'
juveniles_GrowthRate_TL<-function(T){
  gL <- -0.081 + (0.079*T) - (0.003*T^2);#--Hurst et al 2010, juvenile eq, mm per day
  return(gL);
}

#'
#' @title Calculate growth rate (1/day wet weight) for juvenile larvae
#'
#' @description Function to calculate growth rate (1/day wet weight) for juvenile larvae.
#'
#' @param T - temperature (deg C)
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 2-12 deg C.
#'
#' @export
#'
juveniles_GrowthRate_WW<-function(T){
  gM <- (-0.998 + 0.579*T - 0.022*T^2)/100;#--Hurst et al 2010, juvenile eq, per day
  return(gM);
}
