#'
#' @title Integrate SL based on a temperature time series
#'
#' @description Function to integrate SL based on a temperature time series.
#'
#' @param dt - integration time step (in days)
#' @param SL - initial standard length (mm)
#' @param T - vector of temperatures (deg C)
#' @param growthFcn - function calculating temperature-dependent growth rates
#'
#' @return tibble with columns age, T, and SL
#'
#' @details None.
#'
#' @examples
#' ## T = 5;
#' ## D = ysl_YolkSacAbsorption(T);#--calculate stage duration at T
#' ## vTs = 5+0*(1:100);           #--time series of temperatures
#' ## tbl1 = integrateGrowthSL(D/100,0,vTs,ysl_GrowthRateSL)
#'
#' @export
#'
integrateGrowthSL<-function(dt,SL,T,growthFcn){
  nT = length(T);
  vTs = vector(mode="numeric",length=nT-1);
  vTs = 0.5*(T[1:(nT-1)] + T[2:nT]);
  vSL = vector(mode="numeric",length=nT);
  vSL = cumsum(c(SL,dt*growthFcn(vTs)));
  tbl = tibble::tibble(age=dt*(0:(nT-1)),T=T,SL=vSL);
  return(tbl);
}

#'
#' @title Calculate SL-at-age for a constant temperature.
#'
#' @description Function to calculate SL-at-age at a constant temperature.
#'
#' @param n - number of integration time steps
#' @param SL - initial standard length (mm)
#' @param T - temperature (deg C)
#' @param durationFcn - temperature-dependent function giving duration
#' @param growthFcn - temperature-dependent function giving growth rate
#'
#' @return tibble with columns age and SL
#'
#' @details Uses \code{\link{integrateGrowthSL}} to calculate a time series of size-at-age given
#' temperature-dependent duration and growth functions.
#'
#' @example
#' ## tbl2 = calcSLatAgeAtConstantTemp(100,0,5,ysl_YolkSacAbsorption,ysl_GrowthRateSL)
#'
#' @export
#'
calcSLatAgeAtConstantTemp<-function(n,SL,T,durationFcn,growthFcn){
  tYSA = durationFcn(T);
  dt   = tYSA/n;
  vT   = T+0*1:n;
  tbl  = integrateGrowthSL(dt,SL,vT,growthFcn);
  return(tbl);
}

#'
#' @title Integrate SL based on a temperature time series
#'
#' @description Function to integrate SL based on a temperature time series.
#'
#' @param dt - integration time step (in days)
#' @param SL - initial standard length (mm)
#' @param T - vector of temperatures (deg C)
#' @param growthFcn - function calculating temperature-dependent growth rates
#'
#' @return tibble with columns age, T, grDW, and DW
#'
#' @details None.
#'
#' @examples
#' ## T = 5;
#' ## D = ysl_YolkSacAbsorption(T);#--calculate stage duration at T
#' ## vTs = 5+0*(1:100);           #--time series of temperatures
#' ## tbl1 = integrateGrowthDW(D/100,0.01,vTs,ysl_GrowthRateSL)
#'
#' @export
#'
integrateGrowthDW<-function(dt,DW,T,growthFcn){
  nT = length(T);
  vTs = vector(mode="numeric",length=nT-1);
  vTs = 0.5*(T[1:(nT-1)] + T[2:nT]);
  vGr = growthFcn(vTs);
  vDW = vector(mode="numeric",length=nT);
  vDW[1] = DW;
  for (i in 1:(nT-1)) vDW[i+1] = vDW[i] + vGr[i]*vDW[i]*dt;
  tbl = tibble::tibble(age=dt*(0:(nT-1)),T=T,grDW=c(vGr,0),DW=vDW);
  return(tbl);
}

#'
#' @title Calculate time series of dry weight (DW)-at-age for a constant temperature.
#'
#' @description Function to calculate time series of DW-at-age at a constant temperature.
#'
#' @param n - number of integration time steps
#' @param DW - initial dry weight (units are arbitrary)
#' @param T - temperature (deg C)
#' @param durationFcn - temperature-dependent function giving duration
#' @param growthFcn - temperature-dependent function giving DW growth rate
#'
#' @return tibble with columns age-in-stage,temperature, and DW
#'
#' @details Uses \code{\link{integrateGrowthDW}} to calculate a time series of DW-at-age given
#' temperature-dependent duration and DW growth functions.
#'
#' @examples
#' ## initDW = 0.01; #--milligrams
#' ## tbl1 = calcDWatAgeAtConstantTemp(100,initDW,5,egg_StageDuration,egg_GrowthRateDW);
#'
#'##  initDW = 0.14; #--milligrams
#' ## tbl2 = calcDWatAgeAtConstantTemp(100,initDW,5,ysl_YolkSacAbsorption,ysl_GrowthRateDW)
#'
#' @export
#'
calcDWatAgeAtConstantTemp<-function(n,DW,T,durationFcn,growthFcn){
  tYSA = durationFcn(T);
  dt   = tYSA/n;
  vT   = T+0*1:n;
  tbl  = integrateGrowthDW(dt,DW,vT,growthFcn);
  return(tbl);
}
