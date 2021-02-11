#'
#' @title Calculate growth rate (mm/day SL) for postflexion larvae
#'
#' @description Function to calculate growth rate (mm/day SL) for postflexion larvae.
#'
#' @param T - temperature (deg C)
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 3-11 deg C.
#'
#' @export
#'
postflexionLarvae_GrowthRateSL<-function(T){
  gL <- 0.034 + (0.043*T) - (0.0008*T^2);#--Hurst et al 2010, postflexion eq, mm per day
  return(gL);
}

#'
#' @title Calculate growth rate (mm/day TL) for postflexion larvae
#'
#' @description Function to calculate growth rate (mm/day TL) for postflexion larvae.
#'
#' @param T - temperature (deg C)
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 3-11 deg C.
#'
#' @export
#'
postflexionLarvae_GrowthRateTL<-function(T){
  gL <- 0.044 + (0.019*T) - (0.001*T^2);#--Hurst et al 2010, postflexion eq, mm per day
  return(gL);
}

#'
#' @title Calculate growth rate (1/day dry weight) for postflexion larvae
#'
#' @description Function to calculate growth rate (1/day dry weight) for postflexion larvae.
#'
#' @param T - temperature (deg C)
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 3-11 deg C.
#'
#' @export
#'
postflexionLarvae_GrowthRateDW<-function(T){
  gM <- (1.652 + 1.059*T - 0.028*T^2)/100;#--Hurst et al 2010, postflexion eq, per day (orig. eq. in %/d)
  return(gM);
}

#'
#' @title Calculate growth rate (1/day wet weight) for postflexion larvae
#'
#' @description Function to calculate growth rate (1/day wet weight) for postflexion larvae.
#'
#' @param T - temperature (deg C)
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 3-11 deg C.
#'
#' @export
#'
postflexionLarvae_GrowthRateWW<-function(T){
  gM <- (0.531 + 0.857*T - 0.024*T^2)/100;#--Hurst et al 2010, postflexion eq, per day (orig. eq. in %/d)
  return(gM);
}
