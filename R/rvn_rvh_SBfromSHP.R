#' @title Generate Raven SubBasin Data.frame from Shapefile
#'
#' @description
#' For creating a data.frame of Raven subbasin properties from a shapefile with the appropriate columns.
#' Compatible with \code{\link{rvn_rvh_write}}. Generic column names are assumed for shapefile, but can
#' also be passed as arguments.
#'
#' @param sbshp sf object or data.frame with subbasin columns
#' @param SBID_col character, subbasin ID column name (optional)
#' @param name_col character, subbasin name column name (optional)
#' @param downstream_col character, subbasin downstream subbasin column name
#' @param profile_col character, subbaisn reach profile column name
#' @param length_col character, subbasin reach length column name
#' @param gauged_col character, subbasin boolean gauged flag column name
#'
#' @return data.frame of subbasin properties for writing to RVH file
#' @export rvn_rvh_SBfromSHP
#'
#' @examples
#TODO
rvn_rvh_SBfromSHP <- function(sbshp,
                              SBID_col=NA,
                              name_col=NA,
                              downstream_col="Downstream_ID",
                              profile_col="Profile",
                              length_col="ReachLength",
                              gauged_col="Gauged") {

  #-- Create subbasin DF
  sbdf <- rvn_rvh_blankSBdf(nSubBasins = nrow(shp))

  #-- Fill from shapefile

  #-- SBID
  if (!is.na(SBID_col)) {
    sbdf$SBID        <- sbshp[,SBID_col]
  }

  #-- Name
  if (!is.na(name_col)) {
    sbdf$Name        <- sbshp[,name_col]
  } else {
    sbdf$Name        <- paste0('SBID_',sbshp[,SBID_col])
  }

  #-- Less flexible columns
  sbdf$Downstream_ID <- sbshp[, downstream_col]
  sbdf$Profile       <- sbshp[, profile_col]
  sbdf$ReachLength   <- sbshp[, length_col]
  sbdf$Gauged        <- as.integer(sbshp[, gauged_col])

  #-- Return
  return(sbdf)
}
