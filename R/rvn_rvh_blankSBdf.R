#' @title Generate Blank Raven SubBasin DataFrame
#'
#' Generates a blank data frame for Raven subbasin properties.
#'
#' @param nSubBasins Number of SubBasins in model, used to determine number of rows in table (default = 1)
#'
#' @return data.frame of blank SubBasin properties to be filled in by user
#' @author Leland Scantlebury
#' @export rvn_rvh_blankSBdf
#'
#' @examples
#' SBtable <- rvn_rvh_blankSBdf(nSubBasins = 1)
rvn_rvh_blankSBdf <- function(nSubBasins = 1) {
  df <- data.frame('SBID'          = 1:nSubBasins,
                   'Name'          = NA,
                   'Downstream_ID' = -1,
                   'Profile'       = "DEFAULT",
                   'ReachLength'   = 0.0,
                   'Gauged'        = 0)
  return(df)
}
