#' Generate Blank Raven HRU DataFrame
#'
#' Used to generate a blank HRU table that can be filled in by the user. Compatible
#' with \code{\link{rvn_rvh_write}}.
#'
#' @param nHRUs Number of HRUs, used to determine number of rows in table (default = 1)
#' @param subbasinIDs Subbasins that HRUs belong to (default = all equal 1)
#'
#' @return data.frame of blank HRU properties to be filled in by user
#' @author Leland Scantlebury
#' @export rvn_rvh_blankHRUdf
#'
#' @examples
#' HRUtable <- rvn_rvh_blankHRUdf(nHRUs = 3, subbasinIDs=c(1,1,2))
rvn_rvh_blankHRUdf <- function(nHRUs = 1, subbasinIDs=NULL) {
  if (is.null(subbasinIDs)) {
    subbasinIDs <- rep(1, nHRUs)
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
