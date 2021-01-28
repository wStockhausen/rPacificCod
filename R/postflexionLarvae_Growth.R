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
postflexionLarvae_GrowthRate_SL<-function(T){
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
postflexionLarvae_GrowthRate_TL<-function(T){
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
postflexionLarvae_GrowthRate_DW<-function(T){
  gM <- (1.652 + 1.059*T - 0.028*T^2)/100;#--Hurst et al 2010, postflexion eq, per day
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
postflexionLarvae_GrowthRate_WW<-function(T){
  gM <- (0.531 + 0.857*T - 0.024*T^2)/100;#--Hurst et al 2010, postflexion eq, per day
  return(gM);
}

#'
#' @title Map growth rates (1/day wet weight) for postflexion larvae
#'
#' @description Function to map growth rates (1/day wet weight) for postflexion larvae.
#'
#' @param roms_grid - sf dataframe representing a ROMS grid object
#' @param sf_mo - sf dataframe representing a ROMS model object, or path to a ROMS model netcdf file
#' @param timeslice - time slice to extract
#' @param vertical_layer - vertical layer to extract
#' @param basemap - basemap to plot growth rates on top of
#'
#' @return map of growth rates
#'
#' @details From Hurst et al., 2010. Valid range is 3-11 deg C.
#'
#' @import grDevices
#' @import lwgeom
#' @import wtsROMS
#'
#' @export
#'
postflexionLarvae_MapGrowthRate_WW<-function(roms_grid=wtsROMS::getGrid("CGOA"),
                                             sf_mo="~/Work/Projects/ROMS/CGOA/cgoa_avg_0005.nc",
                                             timeslice=1,
                                             vertical_layer=1,
                                             basemap=NULL){

  roms_grid<-sf::st_transform(roms_grid,3338);#Alaska Albers
  idx<-is.na(roms_grid$Z);
  if (is.character(sf_mo)){
    sf_mo<-wtsROMS::netCDF_Extract4DVarAsSF(sf_mo,roms_grid,"temp",timeslice,vertical_layer);
    g<-postflexionLarvae_GrowthRate_WW(sf_mo$temp);
    sf_mo["growth rate (1/day)"]<-g;
    sf_mo<-sf_mo[!idx,];
  }
  if (is.null(basemap)){
    bbox   <-wtsROMS::getStandardBoundingBox("CGOA shelf");
    basemap<-wtsROMS::tmap_CreateBasemap(bbox=bbox);
  }

  map    <-wtsROMS::tmap_CreateMap(sf_mo,col="growth rate (1/day)",basemap=basemap);
  return(map);
}

