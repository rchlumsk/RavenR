#' @title Create weights time series for calibration/diagnostic evaluation
#'
#' @description
#' Creates an observation data weights time series based upon dates stored in
#' an xts time series and criterion given by the user
#'
#' @param ts xts time series
#' @param criterion criterion used to determine weighted vs. non-weighted dates
#' one of 'BEFORE', 'AFTER', 'BETWEEN', 'BETWEEN_CYCLIC'
#' @param startdate Date indicating start of time period (for 'BETWEEN' or
#' 'AFTER' criterion) or start of annual cyclic period (for 'BETWEEN_CYCLIC'). In
#' the latter case, only the julian day of the startdate matters.
#' @param enddate Date indicating end of time period (for 'BETWEEN' or
#' 'BEFORE' criterion) or end of annual cyclic period (for 'BETWEEN_CYCLIC'). In
#' the latter case, only the julian day of the enddate matters.
#'
#' @details
#' for criterion ='BEFORE', all timestamps prior to the enddate have a
#' weight of 1, 0 otherwise \cr
#' for criterion ='AFTER', all timestamps after the startdate have a
#' weight of 1, 0 otherwise \cr
#' for criterion ='BETWEEN', all timestamps after the startdate and before
#' the enddate have a weight of 1, 0 otherwise \cr
#' for criterion ='BETWEEN_CYCLIC', all julian days after the startdate and before
#' the enddate have a weight of 1, 0 otherwise; if startdate is more than enddate,
#' then the opposite is true, e.g, for startdate="2002-11-01" and enddate "2002-01-31",
#' only November, December and January timestamps have a weight of 1
#'
#' @return {wts}{returns numeric vector of weights}
#'
#' @author James R. Craig, University of Waterloo
#'
#' @examples
#'  # locate hydrograph sample csv data from RavenR package
#'  ff <- system.file("extdata","run1_Hydrographs.csv", package="RavenR")
#'
#'  # read in Raven Hydrographs file, store into mydata
#'  mydata <- rvn_hyd_read(ff, tzone="EST")
#'
#'  # generate rvt file using just observations from Subbasin ID 36
#'  flows <- rvn_ts_infill(mydata$hyd$Sub36_obs)
#'  tf1 <- file.path(tempdir(), "myobservations.rvt")
#'  rvn_rvt_obsfile(tf1, flows, 36,
#'    typestr = "HYDROGRAPH")
#'
#'  # weight March-October flows:
#'  wts <- rvn_gen_obsweights(flows,criterion = "BETWEEN_CYCLIC",
#'    startdate="2000-03-01", enddate="2003-11-01")
#'
#'  # and only after March 2003:
#'  wts2 <- rvn_gen_obsweights(flows,criterion = "AFTER",
#'    startdate="2003-03-01")
#'  wts2 <- wts2*wts # product merges weights
#'
#'  # show weights over time
#'  plot(wts2)
#'
#'  # write observation weights to rvt file
#'  tf2 <- file.path(tempdir(), "myobservations_wts.rvt")
#'  rvn_rvt_obsweights(tf2, wts2, 36, typestr="HYDROGRAPH")
#'
#' @seealso \code{\link{rvn_rvt_obsweights}} to write the weights to an rvt file
#'
#' @export rvn_gen_obsweights
#' @importFrom lubridate yday
#' @importFrom zoo index
rvn_gen_obsweights <- function(ts,criterion="BETWEEN",startdate="1785-10-05",enddate="2500-01-01")
{
  # October 5, 1785 - date of the great pumpkin flood

  # interval
  wts<-0.0*ts
  # startdate<-as.Date(startdate)
  # enddate<-as.Date(enddate)

  # check for valid prd
  prd <- sprintf("%s/%s",startdate,enddate)
  # error check prd input as character
  prd <- rvn_get_prd(prd=prd)

  if ((criterion=="BETWEEN") || (criterion=="AFTER") || (criterion=="BEFORE"))
  {
    # intvl<-interval((startdate),(enddate))
    # wts[index(ts) %within% intvl]<-1
    wts[prd] <- 1
  }
  else if (criterion=="BETWEEN_CYCLIC")
  {
    st<-as.POSIXlt(startdate)$yday
    en<-as.POSIXlt(enddate)$yday
    dates<-as.POSIXlt(index(ts))
    if (st<=en){ wts[dates$yday >= st & dates$yday <= en ]<-1}
    else       { wts[dates$yday >= en & dates$yday <= st ]<-1}
  }

  return(wts)
}
