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
#' @return {TRUE}{returns TRUE if function runs properly}
#'
#' @author James R. Craig, University of Waterloo
#'
#' @examples
#'  # locate hydrograph sample csv data from RavenR package
#'  ff <- system.file("extdata","run1_Hydrographs.csv", package="RavenR")
#'
#'  # read in Raven Hydrographs file, store into mydata
#'  mydata <- rvn_hyd_read(ff)
#'
#'  # generate rvt file using just observations from Subbasin ID 36
#'  flows <- rvn_ts_infill(mydata$hyd$Sub36_obs)
#'  rvn_rvt_obsfile("run1_Hydrographs.rvt", flows, 36, typestr = "HYDROGRAPH")
#'
#'  # weight March-October flows:
#'  wts <- rvn_gen_obsweights(flows,criterion = "BETWEEN_CYCLIC", startdate="2000-03-01", enddate="2003-11-01")
#'
#'  # and only after March 2003:
#'  wts2 <- rvn_gen_obsweights(flows,criterion = "AFTER", startdate="2003-03-01")
#'  wts2 <- wts2*wts # product merges weights
#'
#'  # write observation weights to rvt file
#'  rvn_rvt_obsweights("run1_Hydrographs_wts.rvt", wts2, 36, typestr="HYDROGRAPH")
#'
#' @keywords Raven observations weights rvt file write
#'
#' @seealso \code{\link{rvn_obsfile_rvt}} \code{\link{obsweights.gen}}
#' See also the \href{http://raven.uwaterloo.ca/}{Raven website}
#' @export rvn_rvt_obsweights
rvn_rvt_obsweights <- function(filename,wts,SBID,typestr="HYDROGRAPH") {
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
  return(TRUE)
}
