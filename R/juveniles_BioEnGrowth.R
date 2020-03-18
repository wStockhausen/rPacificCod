#'
#' @title Calculate growth (\% wet mass per day) by weight and temperature
#'
#' @description Function to calculate growth (\% wet mass per day) by weight and temperature.
#'
#' @param W - weight(s) of individual fish
#' @param T - temperature(s)
#' @param DSa - fraction of assimilated energy lost to specific dynamic action
#' @param Fa - fraction of consumed energy lost to egestion
#' @param Ua - fraction of assimilated energy lost to execretion
#' @param EDp - energy denisty of prey (J/g prey)
#'
#' @return dataframe with columns W (weight), T (temperature),
#' G (growth rate), C (consumption rate), R (total respiration rate), F (egestion rate),
#' U (excretion rate), Ra (non-SDA repsiration rate), and SDA (specific dynamic action rate).
#'
#' @details Based on eq.s S1, S2, and S3 in Supplement to Hurst et al. 2018 as corrected by
#' those in Cianelli et al, 1998:
#' \itemize{
#' \item{\eqn{G(W,T)   = C(W,T) - [R(W,T) + F(W,T) + U(W,T)] }, growth []}
#' \item{\eqn{C(W,T)   = C(W,EDp)*fC(T) }, consumption []}
#' \item{\eqn{R(W,T)   = Ra(W,T) + SDA(W,T) }, total respiration []}
#' \item{\eqn{Ra(W,T)  = R(W)*fR(T) }, non-SDA respiration}
#' \item{\eqn{F(W,T)   = Fa*C }, egestion []}
#' \item{\eqn{U(W,T)   = Ua*(C-F) }, excretion []}
#' \item{\eqn{SDA = DSa*(C-F) }, specific dynamic action []}
#'}
#'
#' @section TODO:
#' There are clearly some issues with these equations, probably in terms
#' of units.
#'
#' @family juvenile bioenergetic functions
#'
#' @export
#'
juv_BioEnGrowth<-function(W,
                         T,
                         DSa=0.125,
                         Fa=0.15,
                         Ua=0.11,
                         EDp=4113){
  dfr<-NULL;
  for (w in W){
    for (t in T){
      c  <- juv_C(w,EDp)*juv_fC(t);  #in J/g fish/d
      Rp <- juv_Ra(w)*juv_fR(t);     #in J/g fish/d
      f  <-Fa*c;                             #in J/g fish/d
      u  <-Ua*(c-f);                         #in J/g fish/d
      sda<-DSa*(c-f);                        #in J/g fish/d
      #r<-Rp+sda;
      r <- Rp;
      g <- c-(r+f+u); #in J/g fish/d
      dfrp<-data.frame("W"=w,"T"=t,"G"=g,"C"=c,"R"=r,"F"=f,"U"=u,"Rp"=Rp,"SDA"=sda);
      dfr<-rbind(dfr,dfrp);
    }
  }
  return(dfr);
}


