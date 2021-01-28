#'
#' @title Get a pkg{sf} dataframe with spatially-explicit temperature-dependent growth rates from ROMS model output
#'
#' @description Function to get a pkg{sf} dataframe with spatially-explicit temperature-dependent growth rates from ROMS model output.
#'
#' @param rateFcn - temperature-dependent rate function to use
#' @param roms_grid - sf dataframe representing a ROMS grid object
#' @param roms_mo - sf dataframe representing a ROMS model object, or path to a ROMS model netcdf file
#' @param timeslice - time slice to extract (if roms_mo is a file path)
#' @param vertical_layer - vertical layer to extract (if roms_mo is a file path)
#' @param crs - \pkg{sf}-style coordinate reference system object, or NULL (default)
#'
#' @return \pkg{sf} dataframe with spataly-explicit temperature-dependent growth rates
#'
#' @details Output dataframe will be in the coordinate reference system of the ROMS model
#' grid unless the input \code{crs} was not NULL (the default).
#'
#'  @note This function can also calculate spatially-explicit egg hatch success if \code{egg_HatchSuccess}
#'  is given as the \code{rateFcn}. The "growth_rate" column of the resulting \pkg{sf} dataframe
#'  will contain the hatch success information.
#'
#' @import sf
#' @import wtsROMS
#'
#' @export
#'
sf_CalcTdepGrowthRatesFromROMS<-function(rateFcn=postflexionLarvae_GrowthRate_WW,
                                         roms_grid=wtsROMS::getGrid("CGOA"),
                                         roms_mo="~/Work/Projects/ROMS/CGOA/cgoa_avg_0005.nc",
                                         timeslice=1,
                                         vertical_layer=1,
                                         crs=NULL){

  if (!is.null(crs)) roms_grid<-sf::st_transform(roms_grid,crs);#transform to crs
  idx<-is.na(roms_grid$Z);
  if (is.character(roms_mo))
    roms_mo<-wtsROMS::netCDF_Extract4DVarAsSF(roms_mo,roms_grid,"temp",timeslice,vertical_layer);
  g<-rateFcn(roms_mo$temp);
  sf_dfr<-sf::st_sf(growth_rate=g,temp=roms_mo$temp,ID=roms_mo$ID,geometry=roms_mo$geometry);
  sf_dfr<-sf_dfr[!idx,];
  return(sf_dfr);
}


# sf_dfr = sf_CalcTdepGrowthRatesFromROMS();

