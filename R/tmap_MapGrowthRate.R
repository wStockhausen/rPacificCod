#'
#' @title Create a \pkg{tmap}-style map of growth rates
#'
#' @description Function to create a \pkg{tmap}-style map of growth rates.
#'
#' @param sf_dfr - \pkg{sf}-style dataframe with rate information (or NULL)
#' @param rate_col - name of column with rate information (default="growth_rate")
#' @param label - label for map legend (default = "growth rate (1/day)")
#' @param palette - palette for growth info (default = NULL to use palette of basemap)
#' @param basemap - \pkg{tmap}-style basemap to plot growth rates on top of
#' @param rateFcn - temperature-dependent rate function to use (if \code{sf_dfr} is NULL)
#' @param roms_grid - sf dataframe representing a ROMS grid object (if \code{sf_dfr} is NULL)
#' @param roms_mo - sf dataframe representing a ROMS model object, or path to a ROMS model netcdf file  (if \code{sf_dfr} is NULL)
#' @param timeslice - time slice to extract (if roms_mo is a file path) (if \code{sf_dfr} is NULL)
#' @param vertical_layer - vertical layer to extract (if roms_mo is a file path) (if \code{sf_dfr} is NULL)
#'
#' @return \pkg{tmap}-style map of growth rates
#'
#' @details If sf_dfr is NULL, then spatially-explicit growth rates are calculated using the \code{rateFcn}
#' and associated information, then mapped.
#'
#'  @note This function can also map other spatially-explicit values by correctly specifying the
#'  \code{rateCol} for the input \code{sf_dfr}.
#'
#' @import tmap
#' @import sf
#' @import wtsROMS
#'
#' @export
#'
tmap_MapGrowthRate<-function(sf_dfr=NULL,
                             rate_col="growth_rate",
                             label="growth rate (1/day)",
                             palette=NULL,
                             basemap=NULL,
                             rateFcn=postflexionLarvae_GrowthRate_WW,
                             roms_grid=wtsROMS::getGrid("CGOA"),
                             roms_mo="~/Work/Projects/ROMS/CGOA/cgoa_avg_0005.nc",
                             timeslice=1,
                             vertical_layer=1){
  if (is.null(sf_dfr)){
    sf_dfr = sf_CalcTdepGrowthRatesFromROMS(rateFcn=rateFcn,roms_grid=roms_grid,roms_mo=roms_mo,timeslice=timeslice,vertical_layer=vertical_layer);
  }
  if (is.null(label)) {
    label = rate_col;
  } else if (label!=rate_col){
    sf_dfr[[label]] = sf_dfr[[rate_col]];#--copy column so map label works
  }
  if (is.null(basemap)){
    bbox   <-wtsROMS::getStandardBoundingBox("CGOA shelf");
    basemap<-wtsROMS::tmap_CreateBasemap(bbox=bbox);
  }

  map <- wtsROMS::tmap_CreateMap(sf_dfr,col=label,palette=palette,basemap=basemap);
  return(map);
}

#map = tmap_MapGrowthRate();       print(map);
#map = tmap_MapGrowthRate(sf_dfr); print(map);


