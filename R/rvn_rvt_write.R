#' @title Write Raven rvt file from Time Series
#'
#' @description
#' rvn_rvt_write generates a Raven rvt file from a time series
#'
#' @details
#' This function writes the rvt file for a given time series dataset. The function will write out
#' the entirety of the columns provided in the given xts object. Please ensure that the
#' parameters supplied in the params and units objects match the xts object supplied.
#'
#' @param ts time series to write in xts or dataframe format
#' @param dates vector of date objects passed, necessary only if ts is not xts
#' @param prd period to use in writing rvt file, format "YYYY-MM-DD/YYYY-MM-DD"
#' @param tt initial start time to file
#' @param dt time interval to write to file
#' @param params the full string expression for the parameters line to write to file
#' @param units the full string expression for the units line to write to file
#' @param ff filename to write to without .rvt extension (added automatically)
#' @return \item{flag}{returns TRUE if the function executed successfully}
#'
#' @seealso \code{\link{rvn_rvt_wsc}} to create an rvt file from Water Survey Canada (WSC) data
#'
#' @examples
#'
#' # load sample flow data
#' system.file('extdata','run1_Hydrographs.csv', package = "RavenR") %>%
#' rvn_hyd_read() -> mydata
#'
#' # write time series to rvt file using data from subbasin 36
#' rvn_rvt_write(mydata$hyd$Sub36, params = "HYDROGRAPH", units = "m3/s",
#'   ff = file.path(tempdir(), 'raven_rvt_write'))
#'
#' @export rvn_rvt_write
#' @importFrom stats start
#' @importFrom xts is.xts
#' @importFrom lubridate date
#' @importFrom gdata write.fwf
rvn_rvt_write <- function(ts, params, units, dates=NULL, prd=NULL,
                          tt="00:00:00", dt=1.0, ff="raven_rvt_write.rvt")
{
  # Deal with dates
  if(!is.null(dates)) {
    min_date <- start(ts)
  } else {
    # XTS
    if (is.xts(ts)) {
      min_date <- as.character(lubridate::date(ts[1]))
    } else {
      stop("Argument 'dates' must be passed for non-XTS time series (ts) arguments")
    }
  }

  # # determine the period to use
  # if (!(is.null(prd))) {
  #
  #   # period is supplied; check that it makes sense
  #   firstsplit <- unlist(strsplit(prd,"/"))
  #   if (length(firstsplit) != 2) {
  #     stop("Check the format of supplied period; should be two dates separated by '/'.")
  #   }
  #   if (length(unlist(strsplit(firstsplit[1],"-"))) != 3 || length(unlist(strsplit(firstsplit[2],"-"))) != 3
  #       || nchar(firstsplit[1])!= 10 || nchar(firstsplit[2]) != 10) {
  #     stop("Check the format of supplied period; two dates should be in YYYY-MM-DD format.")
  #   }
  # }

  prd <- rvn_get_prd(ts,prd)

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

  fc <- file(paste0(ff,".rvt"),open='wt')
  writeLines(":MultiData",fc)
  writeLines(sprintf('%s %s %.2f %i', min_date, tt, dt, nrow(ts)), fc)
  writeLines(params,fc)
  writeLines(units,fc)
  # write.fwf writes nicely-spaced tables, but perhaps this isn't desired in the
  # RVT file since it often isn't edited by hand (and the spaces take up more disk space!)
  write.fwf(x = ts,
                   file = fc,
                   append = TRUE,
                   justify = 'right',
                   colnames = FALSE,
                   sep = ', ',
                   scientific = TRUE)
  #for (j in 1:nrow(ts)) {
  #  writeLines(sprintf(rep("%g ",ncol(ts)),ts[j,1:ncol(ts)]),fc)
  #}
  rvn_write_Raven_label('EndMultiData',fc)
  close(fc)

  return("flag"=TRUE)
}
