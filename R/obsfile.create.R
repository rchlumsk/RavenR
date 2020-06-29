#' Create Raven observation data (rvt) file
#'
#' Creates an observation data file named filename from a continuous (gap-free) xts time series ts
#'
#' @param filename observation data file to be created, with .rvt extension
#' @param ts xts time series with single data column
#' @param SBID Subbasin ID for hydrographs and reservoir stages or HRU ID for observations of state variables (e.g., snow depth)
#' @param typestr Raven-recognized data type string: 'HYDROGRAPH', 'RESERVOIR_STAGE', 'RESERVOIR_INFLOW', 'RESERVOIR_NET_INFLOW',
#' or a state variable name (e.g., SOIL[0] or SNOW)
#' @param units units of the data (should be consistent with Raven units; neither Raven nor this routine checks or corrects
#'
#' @return nothing; produces file only
#'
#' @author James R. Craig, University of Waterloo
#'
#' @examples
#'   mydata<-read.csv("JohnCreek.csv")
#'   flows<-timeseries.infill(mydata)
#'   obsfile.create("JohnCreek.rvt", flows, 13,typestr="HYDROGRAPH")
#'
#'   # just weight March-October flows :
#'   wts<-obsweights.gen(flows,criterion="BETWEEN_CYCLIC",startdate="2000-03-01",enddate="2000-11-01")
#'
#'   # and only after March 2003:
#'   wts<-obsweights.gen(flows,criterion="AFTER",startdate="2003-03-01")
#'   wts2=wts2*wts # product merges weights
#'
#'   obsweightsfile.create("JohnCreekWts.rvt",wts2,13,typestr="HYDROGRAPH")
#'
#' @keywords Raven observations
#'
#' @seealso \code{\link{timeseries.infill}} \code{\link{obsweightsfile.create}}
#' See also the \href{http://raven.uwaterloo.ca/}{Raven website}
#' @export obsfile.create
#'
obsfile.create<-function(filename,ts,SBID,typestr="HYDROGRAPH", units="m3/s")
{
  # assumes time series ts is continuous
  interval<-as.numeric(difftime(index(ts[2]) ,index(ts[1]) , units = c("days")))

  ts[is.na(ts)]<-(-1.2345)

  line1<-paste0(":ObservationData ",typestr," ",SBID, " ",units)
  line2<-paste0(strftime(start(ts), "%Y-%m-%d %H:%M:%S")," ", interval," ",length(ts))

  write(line1,append=FALSE,file=filename)
  write(line2,append=TRUE,file=filename)
  write(ts, file = filename, ncolumns=1,append = TRUE, sep = " ")
  write(":EndObservationData",append=TRUE,file=filename)
}
#' Create Raven observation data weight (rvt) file
#'
#' Creates an observation data weights file named filename from a continuous (gap-free) xts time series ts
#' with values between 0 and 1
#'
#' @param filename observation data file to be created, with .rvt extension
#' @param wts xts time series with single data column of observation weights
#' @param SBID Subbasin ID for hydrographs and reservoir stages or HRU ID for observations of state variables (e.g., snow depth)
#' @param typestr Raven-recognized data type string: 'HYDROGRAPH', 'RESERVOIR_STAGE', 'RESERVOIR_INFLOW', 'RESERVOIR_NET_INFLOW',
#' or a state variable name (e.g., SOIL[0] or SNOW)
#'
#' @return nothing; produces file only
#'
#' @author James R. Craig, University of Waterloo
#'
#' @examples
#'   mydata<-read.csv("JohnCreek.csv")
#'   flows<-timeseries.infill(mydata)
#'   obsfile.create("JohnCreek.rvt", flows, 13,typestr="HYDROGRAPH")
#'
#'   # just weight March-October flows :
#'   wts<-obsweights.gen(flows,criterion="BETWEEN_CYCLIC",startdate="2000-03-01",enddate="2000-11-01")
#'
#'   # and only after March 2003:
#'   wts<-obsweights.gen(flows,criterion="AFTER",startdate="2003-03-01")
#'   wts2=wts2*wts # product merges weights
#'
#'   obsweightsfile.create("JohnCreekWts.rvt",wts2,13,typestr="HYDROGRAPH")
#'
#' @keywords Raven observations
#'
#' @seealso \code{\link{obsfile.create}} \code{\link{obsweights.gen}}
#' See also the \href{http://raven.uwaterloo.ca/}{Raven website}
#' @export obsweightsfile.create
obsweightsfile.create<-function(filename,wts,SBID,typestr="HYDROGRAPH")
{
  interval<-as.numeric(difftime(index(wts[2]) ,index(wts[1]) , units = c("days")))

  wts[wts>1]<-1.0
  wts[wts<0]<-0.0
  wts[is.na(wts)]<-(-1.2345)

  line1<-paste0(":ObservationDataWeights ",typestr," ",SBID, " ",units)
  line2<-paste0(strftime(start(ts), "%Y-%m-%d %H:%M:%S")," ", interval," ",length(ts))

  write(line1,append=FALSE,file=filename)
  write(line2,append=TRUE,file=filename)
  write(ts, file = filename, ncolumns=1,append = TRUE, sep = " ")
  write(":EndObservationDataWeights",append=TRUE,file=filename)

}
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
#' @return nothing; produces file only
#'
#' @author James R. Craig, University of Waterloo
#'
#' @examples
#'   mydata<-read.csv("JohnCreek.csv")
#'   flows<-timeseries.infill(mydata)
#'   obsfile.create("JohnCreek.rvt", flows, 13,typestr="HYDROGRAPH")
#'
#'   # just weight March-October flows :
#'   wts<-obsweights.gen(flows,criterion="BETWEEN_CYCLIC",startdate="2000-03-01",enddate="2000-11-01")
#'
#'   # and only after March 2003:
#'   wts<-obsweights.gen(flows,criterion="AFTER",startdate="2003-03-01")
#'   wts2=wts2*wts # product merges weights
#'
#'   obsweightsfile.create("JohnCreekWts.rvt",wts2,13,typestr="HYDROGRAPH")
#'
#' @keywords Raven observations
#'
#' @seealso \code{\link{obsweightsfile.create}}
#' See also the \href{http://raven.uwaterloo.ca/}{Raven website}
#'
#' @export obsweights.gen
#'
obsweights.gen<-function(ts,criterion="BETWEEN",startdate="1785-10-05",enddate="2500-01-01")
{
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

  return (wts)
}


