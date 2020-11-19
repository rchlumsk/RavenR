#' @title Generate Blank Raven HRU DataFrame
#'
#' @description
#' Used to generate a blank HRU table that can be filled in by the user. Compatible
#' with \code{\link{rvn_rvh_write}}.
#'
#' @details
#' Note that if the length of the subbasinIDs vector is greater than the number of HRUs (nHRUs)
#' specified, this will create a table with HRUs belonging to multiple subbasins, which is not feasible.
#' A warning will be issued that the table will need to be modified for hydrologic consistency.
#'
#' @param nHRUs Number of HRUs, used to determine number of rows in table (default = 1)
#' @param subbasinIDs Subbasins that HRUs belong to (default = all equal 1)
#'
#' @return data.frame of blank HRU properties to be filled in by user
#' @author Leland Scantlebury
#'
#' @seealso \code{\link{rvn_rvh_blankSBdf}} to generate blank subbasin data frame
#'
#' @examples
#' HRUtable <- rvn_rvh_blankHRUdf(nHRUs = 3, subbasinIDs=c(1,1,2))
#' HRUtable
#'
#' # fewer nHRUs than subbasinIDs specified
#' rvn_rvh_blankHRUdf(nHRUs = 1, subbasinIDs=c(1,2))
#'
#' @export rvn_rvh_blankHRUdf
rvn_rvh_blankHRUdf <- function(nHRUs = 1, subbasinIDs=NULL) {
  if (is.null(subbasinIDs)) {
    subbasinIDs <- rep(1, nHRUs)
  }

  if (nHRUs < length(subbasinIDs)) {
    warning("nHRUs is less than the subbasinIDs specified, table will need to be modified for hydrologic consistency.")
  }

  #-- default is zero for numbers, NA for text
  df <- data.frame('ID'         = 1:nHRUs,
                   'Area'       = 0.0,
                   'Elevation'  = 0.0,
                   'Latitude'   = 0.0,
                   'Longitude'  = 0.0,
                   'SBID'       = subbasinIDs,
                   'LandUse'    = NA,
                   'Vegetation' = NA,
                   'SoilProfile'= NA,
                   'Terrain'    = "[NONE]",
                   'Aquifer'    = NA,
                   'Slope'      = 0.0,
                   'Aspect'     = 0.0)
  return(df)
}
