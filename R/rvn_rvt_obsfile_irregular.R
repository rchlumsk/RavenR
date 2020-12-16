#' @title Create Raven irregular observation data (rvt) file
#'
#' @description
#' Creates an observation data file named filename from a discontinuous xts time series ts
#'
#' @param filename observation data file to be created, with .rvt extension
#' @param ts xts time series with single data column (with discontinuous observations)
#' @param SBID Subbasin ID for hydrographs and reservoir stages or HRU ID for observations of state variables (e.g., snow depth)
#' @param typestr Raven-recognized data type string: 'HYDROGRAPH', 'RESERVOIR_STAGE', 'RESERVOIR_INFLOW', 'RESERVOIR_NET_INFLOW',
#' or a state variable name (e.g., SOIL[0] or SNOW)
#' @param units units of the data (should be consistent with Raven units; neither Raven nor this routine checks or corrects
#'
#' @return {TRUE}{ returns TRUE if function runs properly}
#'
#' @examples
#'
#' # locate hydrograph sample csv data from RavenR package
#' ff <- system.file("extdata","run1_Hydrographs.csv", package="RavenR")
#'
#' # read in Raven Hydrographs file, store into mydata
#' mydata <- rvn_hyd_read(ff, tzone="EST")
#'
#' # generate rvt file using just observations from Subbasin ID 36
#' flows <- rvn_ts_infill(mydata$hyd$Sub36_obs)
#'
#' # generate irregular series by removing some points
#' ind <- sort(sample(seq(2,length(flows)-1), size=300, replace=FALSE))
#'
#' tf <- file.path(tempdir(), "run1_Hydrographs.rvt")
#' rvn_rvt_obsfile_irregular(tf, flows[-ind], 36, typestr = "HYDROGRAPH")
#' readLines(tf) %>% head()
#'
#'
#' @seealso \code{\link{rvn_rvt_obsfile}} for writing continous observation data
#' @export rvn_rvt_obsfile_irregular
#' @importFrom zoo index
#' @importFrom lubridate year month day hour minute second
rvn_rvt_obsfile_irregular <- function(filename,ts,SBID,typestr="HYDROGRAPH", units="m3/s")
{
  # assumes time series ts is continuous
  # interval<-as.numeric(difftime(index(ts[2]) ,index(ts[1]) , units = c("days")))

  ts[is.na(ts)]<-(-1.2345)

  line1<-paste0(":IrregularObservations ",typestr," ",SBID, " ", sprintf("%i",length(ts)), " ",units)
  # line2<-paste0(strftime(start(ts), "%Y-%m-%d %H:%M:%S")," ", interval," ",length(ts))

  write(line1,append=FALSE,file=filename)
  # write(line2,append=TRUE,file=filename)

  sprintf(sprintf("  %i-%02i-%02i %02i:%02i:%02i %.6f",
                  year(ts), month(ts), day(ts),
                  hour(ts), minute(ts), second(ts),
                  ts[,1])) %>%
  write(., file = filename, ncolumns=1,append = TRUE, sep = " ")
  write(":EndIrregularObservations",append=TRUE,file=filename)
  return(TRUE)
}
