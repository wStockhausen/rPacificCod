#'
#' @title Plot the bioenergetic relationships for juvenile Pacific cod
#'
#' @description Function to plot the bioenergetic relationships for juvenile Pacific cod.
#'
#' @param W - weights, in g
#' @param T - temperatures, in deg C
#'
#' @return list of \code{ggplot2}-type plots
#'
#' @import ggplot2
#' @import reshape2
#'
#' @family juvenile bioenergetic functions
#'
#' @export
#'
juvBioenergetics_PlotRelationships<-function(
           W=seq(from=3.0,to=10.0,by=0.2), #--weights (g)
           T=seq(from=0.0,to=20.0,by=2.0)  #--temperatures (degrees)
                                                  ){
  plots<-list();

  dfrFRs<-data.frame(T=T,respiration=juv_fR(T),consumption=juv_fC(T));
  dfrp<-reshape2::melt(dfrFRs,id.vars="T",variable.name="response");
  p <- ggplot2::ggplot(data=dfrp,mapping=ggplot2::aes_string(x="T",y="value",colour="response"));
  p <- p + ggplot2::geom_line();
  p <- p + ggplot2::labs(x="temperature (deg C)",y="functional response");
  #print(p);
  plots[["functional responses"]]<-p;

  dfrRCs<-data.frame(W=W,respiration=juv_Ra(W),consumption=juv_C(W));
  dfrp<-reshape2::melt(dfrRCs,id.vars="W",variable.name="type");
  p <- ggplot2::ggplot(data=dfrp,mapping=ggplot2::aes_string(x="W",y="value",colour="type"));
  p <- p + ggplot2::geom_line();
  p <- p + ggplot2::labs(x="weight (g)",y="allometry (g/g/d)")
  #print(p);
  plots[["respiration and consumption"]]<-p;

  p <- p + ggplot2::scale_y_log10();
  #print(p);
  plots[["ln(respiration and consumption)"]]<-p;

  dfrCGT<-data.frame("T"=T,"GT"=juv_GT(T),"CT"=juv_CT(T));
  dfrp<-reshape2::melt(dfrCGT,id.vars="T",variable.name="type");
  p <- ggplot2::ggplot(data=dfrp,mapping=ggplot2::aes_string(x="T",y="value",colour="type"));
  p <- p + ggplot2::geom_line();
  p <- p + ggplot2::labs(x="temperature",y="mass equivalent (g/d)")
  #print(p);
  plots[["growth and consumption"]]<-p;

  #--calculate growth
  dfrG<-juv_BioEnGrowth(W,T);
  #--plot growth as a function of weight at different temperatures
  dfrp<-dfrG;
  dfrp$T<-as.factor(dfrp$T)
  p <- ggplot2::ggplot(data=dfrp,mapping=ggplot2::aes_string(x="W",y="G",colour="T"));
  p <- p + ggplot2::geom_line();
  #print(p);
  plots[["g(W|T)"]]<-p;

  #--plot growth as a function of temperature at different weights
  dfrp<-dfrG;
  dfrp$W<-as.factor(dfrp$W)
  p <- ggplot2::ggplot(data=dfrp,mapping=ggplot2::aes_string(x="T",y="G",colour="W"));
  p <- p + ggplot2::geom_line();
  #print(p);
  plots[["g(T|W)"]]<-p;

  #--compare GT with G(w=6.8,T)
  dfrGTw<-juv_BioEnGrowth(W=6.8,T)
  dfrp$W<-as.factor(dfrp$W)
  dfrp<-dfrGTw;
  p <- ggplot2::ggplot(data=dfrp,mapping=ggplot2::aes_string(x="T",y="G",colour="W"));
  p <- p + ggplot2::geom_line();
  p <- p + ggplot2::geom_line(data=dfrCGT,mapping=ggplot2::aes_string(y="GT"),colour="black")
  print(p);
  plots[["g(T|W=6.8g)"]]<-p;

  return(plots);
}

#'
#' @title Plot the bioenergetic relationships for juvenile Pacific cod
#'
#' @description Function to plot the bioenergetic relationships for juvenile Pacific cod.
#'
#' @param W - weights, in g
#' @param T - temperatures, in deg C
#' @param EDc - energy density of P. cod
#'
#' @return list of \code{ggplot2}-type plots
#'
#' @import ggplot2
#' @import reshape2
#'
#' @family juvenile bioenergetic functions
#'
#' @export
#'
juvBioenergetics_CompareRelationships<-function(
           W=6.8,                           #--weights (g)
           T=seq(from=0.0,to=20.0,by=2.0),  #--temperatures (degrees)
           EDc=4138                         #--energy density of P. cod (J/g fish)
                                                  ){
  plots<-list();

  gt<-juv_GT(T);          #--J/g fish/day
  ct<-juv_CT(T,W);        #--J/g fish/day
  rt<-juv_Ra(W)*juv_fR(T);#--J/g fish/day
  mt<-juv_MT(T);          #--?? units
  dfrCGT<-data.frame("T"=T,"fitted growth"=gt,"estimated consumption"=ct,"estimated respiration"=rt,"fitted respiration"=mt);
  dfrp<-reshape2::melt(dfrCGT,id.vars="T",variable.name="type");
  p <- ggplot2::ggplot(data=dfrp,mapping=ggplot2::aes_string(x="T",y="value",colour="type"));
  p <- p + ggplot2::geom_line();
  p <- p + ggplot2::ylim(c(0,NA));
  p <- p + ggplot2::labs(x="temperature",y="energy equivalent (J/g fish/day)")
  print(p);
  plots[["GCR"]]<-p;
  # p <- ggplot2::ggplot(data=dfrp,mapping=ggplot2::aes_string(x="T",y="value",colour="type"));
  # p <- p + ggplot2::geom_line();
  # p <- p + ggplot2::facet_grid(rows=ggplot2::vars(type),scales="free_y");
  # #p <- p + ggplot2::ylim(c(0,NA));
  # p <- p + ggplot2::labs(x="temperature",y="mass equivalent (g/day)")
  # print(p);
  # plots[["GCRbyT"]]<-p;

  return(plots);
}
