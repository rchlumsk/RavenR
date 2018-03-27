#' Write Raven rvt file from Flow Series
#'
#' write.flow.rvt generates a Raven rvt file from a flow series
#'
#' This function writes the rvt file for a given time series of
#' flows. The supplied flows should be in the xts format. This
#' function operates similarly to the ECflow.rvt function (
#' linked below).
#'
#' prd is used by the xts formatted-data to restrict the data reported in .rvt
#' file to this period. The prd should be defined in
#' "YYYY-MM-DD/YYYY-MM-DD" string format. If the period supplied results in an
#' empty time series (i.e. non-overlapping time periods), an error will be
#' thrown.
#'
#' write.redirect will print out the :RedirectToFile command in a separate
#' file called, "flow_stn_redirect_text.rvt". This command can be copied into
#' the main model's .rvt file to redirect to the produced time series files.
#'
#' flip.number is a useful option to place the subID first in the filename.
#' This is often cleaner for organizing files in a folder, since the
#' alphabeticized order is not dependent on the station name, and the observed
#' files will be in one set.
#'
#' @param flow.series flows to write to file in xts format
#' @param subID subbasin ID corresponding to the flow series
#' @param stnName name of the station or file to write to file (used
#' to build rvt file name)
#' @param rvt.type type of flow-based rvt file to write, default ObservationData
#' @param prd period to use in writing rvt file, format "YYYY-MM-DD/YYYY-MM-DD"
#' @param write.redirect (optional) write the :RedirectToFile commands in a
#' separate .rvt file
#' @param flip.number (optional) put the subID first in the .rvt filename
#' @return \item{flag}{returns TRUE if the function executed successfully}
#' @seealso \code{\link{ECflow.rvt}} to create an rvt file from Water Survey Canada (WSC) data
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/Default.html}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven rvt flow file
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph.data)
#' obs <- hydrograph.data$hyd$Sub36_obs
#'
#' # note that the writing examples are not run
#'\dontrun{
#' # write out observation flow file
#' write.flow.rvt(obs,subID=36,stnName="Nith_Obs1",flip.number=1)
#'
#' # also write out the redirect file
#' write.flow.rvt(obs,subID=36,stnName="Nith_Obs1",write.redirect=T,flip.number=1)
#'}
#'
#' @export write.flow.rvt
write.flow.rvt <- function(flow.series,subID,stnName,rvt.type='ObservationData',prd=NULL,write.redirect=F,flip.number=F) {

  # list of known rvt flow data types to supply to Raven model
  known.rvt.types <- c("ObservationData","BasinInflowHydrograph","ReservoirExtraction")

  # data checks
  if (!(rvt.type %in% known.rvt.types)) {
    stop(sprintf("Unknown rvt.type %s, please check.",rvt.type))
  }

  # determine period ----
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

  # begin writing the support file
  if (write.redirect) {
    fc.redirect <- file('flow_stn_redirect_text.rvt',open='a+')
  }

  # being writing rvt file
  if (!(is.null(prd))) {
    flow.series <- flow.series[prd]

    # check that the supplied period works
    if (nrow(flow.series) == 0) {
      stop("Flow.series has zero points in the supplied period, please adjust period and/or data accordingly.")
    }
  }

  # change all NA values to Raven NA (-1.2345)
  flow.series[is.na(flow.series)] = -1.2345

  # write .rvt file
  if (flip.number) {
    rvt.name <- sprintf('%i_%s.rvt',subID,stnName)
  } else {
    rvt.name <- sprintf('%s_%i.rvt',stnName,subID)
  }

  fc <- file(rvt.name,open='w+')
  writeLines(sprintf(':%s HYDROGRAPH %i m3/s # %s',rvt.type,subID,rvt.name),fc)
  writeLines(sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(flow.series[1])),nrow(flow.series)),fc)
  for (j in 1:nrow(flow.series)) {
    writeLines(sprintf('%g',flow.series[j]),fc)
  }
  writeLines(sprintf(':End%s',rvt.type),fc)
  close(fc)

  # write to support file
  if (write.redirect) {
    writeLines(sprintf(':RedirectToFile %s',rvt.name),fc.redirect)
    close(fc.redirect)
  }

  return("flag"=TRUE)
}

