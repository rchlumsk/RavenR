#' Create weights time series for calibration/diagnostic evaluation
#'
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
#' weight of 1, 0 otherwise
#' for criterion ='AFTER', all timestamps after the startdate have a
#' weight of 1, 0 otherwise
#' for criterion ='BETWEEN', all timestamps after the startdate and before
#' the enddate have a weight of 1, 0 otherwise
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
#' \dontrun{
#'   mydata<-read.csv("JohnCreek.csv")
#'   flows<-timeseries.infill(mydata)
#'   rvn_obsfile_rvt("JohnCreek.rvt", flows, 13,typestr="HYDROGRAPH")
#'
#'   # just weight March-October flows :
#'   wts<-obsweights.gen(flows,criterion="BETWEEN_CYCLIC",startdate="2000-03-01",enddate="2000-11-01")
#'
#'   # and only after March 2003:
#'   wts<-obsweights.gen(flows,criterion="AFTER",startdate="2003-03-01")
#'   wts2=wts2*wts # product merges weights
#'
#'   obsweightsfile.create("JohnCreekWts.rvt",wts2,13,typestr="HYDROGRAPH")
#'  }
#'
#' @keywords Raven observations weights generate
#'
#' @seealso \code{\link{rvn_rvt_obsweights}} to write the weights to an rvt file
#' See also the \href{http://raven.uwaterloo.ca/}{Raven website}
#'
#' @export rvn_gen_obsweights
rvn_gen_obsweights <- function(ts,criterion="BETWEEN",startdate="1785-10-05",enddate="2500-01-01") {
  # October 5, 1785 - date of the great pumpkin flood

  # interval
  wts<-0.0*ts
  startdate<-as.Date(startdate)
  enddate<-as.Date(enddate)

  if ((criterion=="BETWEEN") || (criterion=="AFTER") || (criterion=="BEFORE"))
  {
    interval<-interval((startdate),(enddate))
    wts[index(ts) %within% interval]<-1
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
