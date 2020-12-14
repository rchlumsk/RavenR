#' @title Generate Blank Raven SubBasin DataFrame
#'
#' @description
#' Generates a blank data frame for Raven subbasin properties.
#'
#' @param nSubBasins Number of SubBasins in model, used to determine number of rows in table (default = 1)
#'
#' @return data.frame of blank SubBasin properties to be filled in by user
#' @author Leland Scantlebury
#'
#' @seealso \code{\link{rvn_rvh_blankHRUdf}} to generate blank HRU data frame
#'
#' @examples
#' SBtable <- rvn_rvh_blankSBdf(nSubBasins = 3)
#' SBtable
#'
#' @export rvn_rvh_blankSBdf
rvn_rvh_blankSBdf <- function(nSubBasins = 1) {
  df <- data.frame('SBID'          = 1:nSubBasins,
                   'Name'          = NA,
                   'Downstream_ID' = -1,
                   'Profile'       = "DEFAULT",
                   'ReachLength'   = 0.0,
                   'Gauged'        = 0)
  return(df)
}
