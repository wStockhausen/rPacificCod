#'
#' @title Calculate the hatch success of Pacific cod eggs by temtperature
#'
#' @description Function to calculate the hatch success of Pacific cod eggs by temtperature.
#'
#' @param T - temperature (deg C)
#'
#' @return value
#'
#' @details Based on Laurel and Rogers (2020)
#'
#' @export
#'
eggs_CalcHatchSuccess<-function(T){
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
eggs_PlotHatchSuccess<-function(T=seq(from=0,to=15,by=0.1)){
  #--plot temperature dependence
  h<-eggs_CalcHatchSuccess(T);
  dfr<-data.frame(T=T,h=h);
  p <- ggplot2::ggplot(dfr,aes_string(x=T,y=h)) + geom_line();
  p <- p + ggplot2::labs(x="temperature (deg C)",y="Hatch Success",title="Pacific cod egg hatch success");
  return(p);
}

