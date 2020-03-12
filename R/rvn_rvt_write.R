#' Write Raven rvt file from Time Series
#'
#' rvn_rvt_write generates a Raven rvt file from a time series
#'
#' This function writes the rvt file for a given time series dataset. The function will write out
#' the entirety of the columns provided in the given xts object. Please ensure that the
#' parameters supplied in the params and units objects match the xts object supplied.
#'
#' @param ts time series to write in xts format
#' @param prd period to use in writing rvt file, format "YYYY-MM-DD/YYYY-MM-DD"
#' @param tt initial start time to file
#' @param dt time interval to write to file
#' @param params the full string expression for the parameters line to write to file
#' @param units the full string expression for the units line to write to file
#' @param ff filename to write to without .rvt extension (added automatically)
#' @return \item{flag}{returns TRUE if the function executed successfully}
#'
#' @seealso \code{\link{rvn_wsc_rvt}} to create an rvt file from Water Survey Canada (WSC) data
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/Default.html}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven rvt flow file
#' @examples
#'
#' data(hydrograph.data)
#' rvn_rvt_write(hydrograph.data$hyd)
#'
#' @export rvn_rvt_write
rvn_rvt_write <- function(ts=NULL, prd=NULL, tt="00:00:00", dt=1.0, params=":Parameters XX",
                      units=":Units XX", ff="raven_rvt_write.rvt") {

  if(is.null(ts)) {
    stop("ts is required for this function.")
  }

  # determine the period to use
  if (!(is.null(prd))) {

    # period is supplied; check that it makes sense
    firstsplit <- unlist(strsplit(prd,"/"))
    if (length(firstsplit) != 2) {
      stop("Check the format of supplied period; should be two dates separated by '/'.")
    }
    if (length(unlist(strsplit(firstsplit[1],"-"))) != 3 || length(unlist(strsplit(firstsplit[2],"-"))) != 3
        || nchar(firstsplit[1])!= 10 || nchar(firstsplit[2]) != 10) {
      stop("Check the format of supplied period; two dates should be in YYYY-MM-DD format.")
    }
  }

  # begin writing rvt file
  if (!(is.null(prd))) {
    ts <- ts[prd]

    # check that the supplied period works
    if (nrow(ts) == 0) {
      stop("ts has zero points in the supplied period, please adjust period and/or data accordingly.")
    }
  }

  # change all NA values to Raven NA (-1.2345)
  ts[is.na(ts)] = -1.2345

  fc <- file(ff,open='w+')
  writeLines(":MultiData",fc)
  writeLines(sprintf('%s %s %.2f %i',as.character(lubridate::date(ts[1])),tt,dt,nrow(ts)),fc)
  writeLines(params,fc)
  writeLines(units,fc)
  for (j in 1:nrow(ts)) {
    writeLines(sprintf(rep("%g ",ncol(ts)),ts[j,1:ncol(ts)]),fc)
  }
  writeLines(':EndMultiData',fc)
  close(fc)

  return("flag"=TRUE)
}

