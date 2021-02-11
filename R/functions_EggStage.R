#'
#' @title Calculate the temperature-dependent duration of the Pacific cod egg stage
#'
#' @description Function to calculate the temperature-dependent duration of the Pacific cod egg stage.
#'
#' @param T - vector of temperatures (deg C)
#'
#' @return vector of corresponding stage durations (days)
#'
#' @details Based on Hurst et al. 2010.
#'
#' @export
#'
egg_StageDuration<-function(T){
    D = 46.597 - 4.079 * T;
    return(D);
}

#'
#' @title Calculate the temperature-dependent growth rate (SL) for Pacific cod embryos
#'
#' @description Function to calculate the temperature-dependent growth rate (SL) for Pacific cod embryos.
#'
#' @param T - vector of temperatures (deg C)
#'
#' @return vector of corresponding growth rates (mm/d)
#'
#' @details Based on Hurst et al. 2010.
#'
#' @export
#'
egg_GrowthRateSL<-function(T){
    r = 0.104 + (0.024 * T) - (0.00002 * T^2);
    return(r);
}

#'
#' @title Calculate the temperature-dependent growth rate (DW) for Pacific cod embryos
#'
#' @description Function to calculate the temperature-dependent growth rate (DW) for Pacific cod embryos.
#'
#' @param T - vector of temperatures (deg C)
#'
#' @return vector of corresponding growth rates (g/g/d)
#'
#' @details Based on Hurst et al. 2010.
#'
#' @export
#'
egg_GrowthRateDW<-function(T){
    r = (3.807 + (1.493 * T) - (0.032 * T^2))/100;
    return(r);
}

#'
#' @title Calculate the hatch success of Pacific cod eggs by temperature
#'
#' @description Function to calculate the hatch success of Pacific cod eggs by temperature.
#'
#' @param T - temperature (deg C)
#'
#' @return value
#'
#' @details Based on Laurel and Rogers (2020)
#'
#' @export
#'
egg_CalcHatchSuccess<-function(T){
    h = 0.453/(1.0+(((T-4.192)/2.125)^2.0));
    h[T>11.0]<-0;
    return(h);
}

#'
#' @title Plot the hatch success of Pacific cod eggs by temtperature
#'
#' @description Function to plot the hatch success of Pacific cod eggs by temtperature.
#'
#' @param T - temperature (deg C)
#'
#' @return ggplot2 objct
#'
#' @details Based on Laurel and Rogers (2020)
#'
#' @import ggplot2
#'
#' @export
#'
egg_PlotHatchSuccess<-function(T=seq(from=0,to=15,by=0.1)){
  #--plot temperature dependence
  h<-egg_CalcHatchSuccess(T);
  dfr<-data.frame(T=T,h=h);
  p <- ggplot2::ggplot(dfr,aes_string(x=T,y=h)) + geom_line();
  p <- p + ggplot2::labs(x="temperature (deg C)",y="Hatch Success",title="Pacific cod egg hatch success");
  return(p);
}

