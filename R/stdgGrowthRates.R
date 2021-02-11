#'
#' @title Calculate STDG growth rates (mm/day SL) for egg stages
#'
#' @description Function to calculate STDG growth rates (mm/day SL) for egg stages.
#'
#' @param T - vector of temperature (deg C)
#' @param L - vector of standard length (mm)
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 3-11 deg C.
#' T and L should have the same length, unless one has length 1.
#'
#' Note that these growth rates are independent of L.
#'
#' @export
#'
stdgGrowthRateSL_EggStages<-function(T,L){
  gL <- (0.076 + 0.029*T - 0.00002*T^2);#--Hurst et al 2010, (orig. eq. in mm/d)
  return(gL);
}

#'
#' @title Calculate STDG growth rates (mm/day SL) for non-egg stages
#'
#' @description Function to calculate STDG growth rates (mm/day SL) for non-egg stages.
#'
#' @param T - vector of temperatures (deg C)
#' @param L - vector of standard lengths (mm)
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 3-11 deg C.
#' T and M should have the same length, unless one has length 1.
#'
#' @export
#'
stdgGrowthRateSL_NonEggStages<-function(T,L){
  gL = (0.076 + 0.029*T - 0.00002*T^2)/(1-0.059/exp(L^0.0758));#--Hurst et al 2010, (orig. eq. in mm/d)
  return(gL);
}

#'
#' @title Calculate STDG growth rates (1/day wet weight) for egg stages
#'
#' @description Function to calculate STDG growth rates (1/day wet weight) for egg stages.
#'
#' @param T - vector of temperature (deg C)
#' @param M - vector of dry weight (micrograms))
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 3-11 deg C.
#' T and M should have the same length, unless one has length 1.
#'
#' @export
#'
stdgGrowthRateDW_EggStages<-function(T,M){
  gM = ((0.454 + 1.610*T - 0.069*T^2)*exp(-6.725*M)+3.705)/100;#--Hurst et al 2010, (orig. eq. in %/d)
  return(gM);
}

#'
#' @title Calculate STDG growth rates (1/day wet weight) for post-egg stages
#'
#' @description Function to calculate STDG growth rates (1/day wet weight) for post-egg stages.
#'
#' @param T - vector of temperature (deg C)
#' @param M - vector of dry weight (micrograms))
#'
#' @return vector of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 3-11 deg C.
#' T and M should have the same length, unless one has length 1.
#'
#' @export
#'
stdgGrowthRateDW_NonEggStages<-function(T,M){
  gM <- (0.454 + 1.610*T - 0.069*T^2)*exp(-6.725*M)/100;#--Hurst et al 2010, (orig. eq. in %/d)
  return(gM);
}

