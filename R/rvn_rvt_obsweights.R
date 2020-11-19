#' @title Create Raven observation data weight (rvt) file
#'
#' @description
#' Writes observation weights generated from \code{\link{rvn_gen_obsweights}} to
#' a Raven rvt format.
#'
#' @details
#' Any NA values in the weights are converted to the flag -1.2345, used in Raven as NA values.
#'
#' @param filename observation data file to be created, with .rvt extension
#' @param wts xts time series with single data column of observation weights
#' @param SBID Subbasin ID for hydrographs and reservoir stages or HRU ID for observations of state variables (e.g., snow depth)
#' @param typestr Raven-recognized data type string: 'HYDROGRAPH', 'RESERVOIR_STAGE', 'RESERVOIR_INFLOW', 'RESERVOIR_NET_INFLOW',
#' or a state variable name (e.g., SOIL[0] or SNOW)
#'
#' @return TRUE returns TRUE if function runs properly
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
#'  rvn_rvt_obsfile(filename=file.path(tempdir(),"run1_Hydrographs.rvt"),
#'    flows, 36, typestr = "HYDROGRAPH")
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
#'  rvn_rvt_obsweights(filename=file.path(tempdir(), "run1_Hydrographs_obsweights.rvt"),
#'    wts2, 36, typestr="HYDROGRAPH")
#'
#'
#' @seealso \code{\link{rvn_rvt_obsfile}} \code{\link{rvn_gen_obsweights}}
#'
#' @export rvn_rvt_obsweights
#' @importFrom zoo index
rvn_rvt_obsweights <- function(filename="_obsweights.rvt",wts,SBID=1,typestr="HYDROGRAPH")
{

  intvl<-as.numeric(difftime(index(wts[2]), index(wts[1]) , units = c("days")))

  wts[wts>1]<-1.0
  wts[wts<0]<-0.0
  wts[is.na(wts)]<-(-1.2345)

  line1 <- sprintf(":ObservationDataWeights %s %s",typestr,SBID)
  line2 <- sprintf("%s %s %i",strftime(start(wts), "%Y-%m-%d %H:%M:%S"),intvl,length(wts))
  # line1<-paste0(":ObservationDataWeights ",typestr," ",SBID, " ",units)
  # line2<-paste0(strftime(start(wts), "%Y-%m-%d %H:%M:%S")," ", intvl," ",length(wts))

  write(line1,append=FALSE,file=filename)
  write(line2,append=TRUE,file=filename)
  write(wts, file = filename, ncolumns=1,append = TRUE, sep = " ")
  write(":EndObservationDataWeights",append=TRUE,file=filename)
  return(TRUE)
}
