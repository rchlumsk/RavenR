#' @title Generate Blank Raven SubBasin DataFrame
#'
#' @description
#' Generates a blank data frame for Raven subbasin properties. Compatible with \code{\link{rvn_rvh_write}}.
#'
#' @param nSubBasins Number of SubBasins in model, used to determine number of rows in table (default = 1)
#'
#' @return data.frame of blank SubBasin properties to be filled in by user
#' @author Leland Scantlebury
#'
#' @details
#' The subbasin names are provided as 'sub00x', where x is the basin ID. The padding is determined
#' from the number of subbasins. The downstream IDs are generated to assume a linear downstream progression,
#' with an outlet at the terminal subbasin ID, which can be modified after the data frame is created.
#'
#' @seealso \code{\link{rvn_rvh_blankHRUdf}} to generate blank HRU data frame
#'
#' @examples
#' SBtable <- rvn_rvh_blankSBdf(nSubBasins = 3)
#' SBtable
#'
#' @export rvn_rvh_blankSBdf
rvn_rvh_blankSBdf <- function(nSubBasins = 1) {

  if (nSubBasins <= 0) {
    stop("nSubBasins must be greater than zero.")
  }

  subnames <- sprintf("sub%s",unlist(lapply(X=as.character(1:nSubBasins),
         FUN=rvn_stringpad,
         width = floor(log10(nSubBasins)) + 2,
         padstring='0')) )

  downstreamid <- (1:nSubBasins)+1
  downstreamid[length(downstreamid)] <- -1

  df <- data.frame('SBID'          = 1:nSubBasins,
                   'Name'          = subnames,
                   'Downstream_ID' = downstreamid,
                   'Profile'       = "NONE",
                   'ReachLength'   = 0.0,
                   'Gauged'        = 0)
  return(df)
}
