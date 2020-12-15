#' @title Generate Raven HRU Data.frame from Shapefile
#'
#' @description
#' For creating a data.frame of Raven HRU properties from a shapefile with the appropriate columns.
#' Compatible with \code{\link{rvn_rvh_write}}. Generic column names are assumed for shapefile, but can
#' also be passed as arguments.
#'
#' @param hrushp sf object or data.frame with HRU columns. Must have geometry if area_col is NA.
#' @param HRUID_col character, HRU ID column name (optional)
#' @param area_col character, HRU area column name (optional)
#' @param elev_col character, HRU elevation column name
#' @param lat_col character, HRU latitude column name
#' @param long_col character, HRU longitude column name
#' @param SBID_col character, HRU membership subbasin id column name
#' @param landclass_col character, HRU land class column name
#' @param vegclass_col character, HRU vegetation class column name
#' @param soilclass_col character, HRU soil type column name
#' @param terrain_col character, HRU terrain type column name (optional)
#' @param aquifer_col character, HRU aquifer class column name (optional)
#' @param slope_col character, HRU slope column name
#' @param aspect_col character, HRU aspect column name
#'
#' @return data.frame of HRU properties for writing to RVH file
#' @export rvn_rvh_HRUfromSHP
#' @importFrom sf st_area
#'
#' @examples
#TODO
rvn_rvh_HRUfromSHP <- function(hrushp,
                               HRUID_col=NA,
                               area_col=NA,
                               elev_col='Elevation',
                               lat_col='Latitude',
                               long_col='Longitude',
                               SBID_col='SBID',
                               landclass_col='LandUse',
                               vegclass_col='Vegetation',
                               soilclass_col='SoilProfile',
                               terrain_col=NA,
                               aquifer_col=NA,
                               slope_col='Slope',
                               aspect_col='Aspect') {
  #-- Create blank df
  hrudf <- rvn_rvh_blankHRUdf(nHRUs = nrow(hrushp), subbasinIDs = hrushp[,SBID_col])

  #-- Fill columns

  #-- HRUID
  if (!is.na(HRUID)) {
    hrudf$ID <- hrushp[,HRUID_col]
  }

  #-- Area
  if (!is.na(HRUID)) {
    #TODO May need some error handling
    hrudf$Area <- sf::st_area(hrushp)
  }

  # Terrain & Aquifer
  if (!is.na(terrain_col)) {
    hrudf$Terrain     <- hrushp[,terrain_col]
  }
  if (!is.na(aquifer_col)) {
    hrudf$Aquifer     <- hrushp[,aquifer_col]
  }

  #-- Less flexible columns
  hrudf$Elevation   <- hrushp[,area_col]
  hrudf$Latitude    <- hrushp[,lat_col]
  hrudf$Longitude   <- hrushp[,long_col]
  # hrudf$SBID      <- Handled in blank hru df creation
  hrudf$LandUse     <- hrushp[,landclass_col]
  hrudf$Vegetation  <- hrushp[,vegclass_col]
  hrudf$SoilProfile <- hrushp[,soilclass_col]
  hrudf$Slope       <- hrushp[,slope_col]
  hrudf$Aspect      <- hrushp[,aspect_col]

  return(hrudf)
}
