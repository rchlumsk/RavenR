#' @title Create Raven observation data (rvt) file
#'
#' @description
#' Creates an observation data file named filename from a continuous (gap-free) xts time series ts
#'
#' @param filename observation data file to be created, with .rvt extension
#' @param ts xts time series with single data column
#' @param SBID Subbasin ID for hydrographs and reservoir stages or HRU ID for observations of state variables (e.g., snow depth)
#' @param typestr Raven-recognized data type string: 'HYDROGRAPH', 'RESERVOIR_STAGE', 'RESERVOIR_INFLOW', 'RESERVOIR_NET_INFLOW',
#' or a state variable name (e.g., SOIL[0] or SNOW)
#' @param units units of the data (should be consistent with Raven units; neither Raven nor this routine checks or corrects
#'
#' @return {TRUE}{ returns TRUE if function runs properly}
#'
#' @author James R. Craig, University of Waterloo
#'
#' @examples
#' # locate hydrograph sample csv data from RavenR package
#' ff <- system.file("extdata","run1_Hydrographs.csv", package="RavenR")
#'
#' # read in Raven Hydrographs file, store into mydata
#' mydata <- rvn_hyd_read(ff, tzone="EST")
#'
#' # generate rvt file using just observations from Subbasin ID 36
#' flows <- rvn_ts_infill(mydata$hyd$Sub36_obs)
#' tf <- file.path(tempdir(), "run1_Hydrographs.rvt")
#' rvn_rvt_obsfile(tf, flows, 36, typestr = "HYDROGRAPH")
#' readLines(tf) %>% head()
#'
#'
#' @seealso \code{\link{rvn_ts_infill}} for infilling time series, and \code{\link{rvn_rvt_obsweights}} to write an rvt observation weights file.
#'
#' @export rvn_rvt_obsfile
#' @importFrom zoo index
rvn_rvt_obsfile <- function(filename,ts,SBID,typestr="HYDROGRAPH", units="m3/s")
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
  return(TRUE)
}
