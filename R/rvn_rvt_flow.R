#' @title Write Raven rvt file from Flow Series
#'
#' @description
#' rvn_rvt_flow generates a Raven rvt file from a flow series
#'
#' @details
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
#' write_redirect will print out the :RedirectToFile command in a separate
#' file called, "flow_stn_redirect_text.rvt". This command can be copied into
#' the main model's .rvt file to redirect to the produced time series files.
#'
#' flip_number is a useful option to place the subID first in the filename.
#' This is often cleaner for organizing files in a folder, since the
#' alphabeticized order is not dependent on the station name, and the observed
#' files will be in one set.
#'
#' The function will write to name generated from the station name, otherwise
#' the .rvt filename may be specified with the filename argument (full path to
#' the filename, including .rvt extension).
#'
#' @param flow.series flows to write to file in xts format
#' @param subID subbasin ID corresponding to the flow series
#' @param stnName name of the station or file to write to file (used
#' to build rvt file name, required if filename not provided)
#' @param rvt_type type of flow-based rvt file to write, default ObservationData
#' @param prd period to use in writing rvt file, format "YYYY-MM-DD/YYYY-MM-DD"
#' @param write_redirect (optional) write the :RedirectToFile commands in a
#' separate .rvt file
#' @param flip_number (optional) put the subID first in the .rvt filename
#' @param filename specified name of file to write to (optional)
#' @return \item{TRUE}{returns TRUE if the function executed successfully}
#' @seealso \code{\link{rvn_rvt_wsc}} to create an rvt file from Water Survey Canada (WSC) data
#'
#' @examples
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#' obs <- rvn_hydrograph_data$hyd$Sub36_obs
#'
#' # write out observation flow file to temporary file
#' rvn_rvt_flow(obs,subID=36,
#'   filename = file.path(tempdir(), "Nith_obs.rvt"))
#'
#'
#' @export rvn_rvt_flow
rvn_rvt_flow <- function(flow.series, subID, stnName=NULL,
                         rvt_type='ObservationData', prd=NULL, write_redirect=FALSE, flip_number=FALSE,
                         filename=NULL)
{

  flow.sim <- NULL

  # list of known rvt flow data types to supply to Raven model
  known.rvt_types <- c("ObservationData","BasinInflowHydrograph","ReservoirExtraction")

  # data checks
  if (!(rvt_type %in% known.rvt_types)) {
    stop(sprintf("Unknown rvt_type %s, please check.",rvt_type))
  }

  prd <- rvn_get_prd(flow.sim, prd)

  # begin writing the support file
  if (write_redirect) {
    fc.redirect <- file('flow_stn_redirect_text.rvt',open='a+')
  }

  if (!(is.null(prd))) {
    flow.series <- flow.series[prd]

    # check that the supplied period works
    if (nrow(flow.series) == 0) {
      stop("Flow.series has zero points in the supplied period, please adjust period and/or data accordingly.")
    }
  }

  # change all NA values to Raven NA (-1.2345)
  flow.series[is.na(flow.series)] = -1.2345

  # determine rvt file name
  if (!is.null(filename)) {
    rvt.name <- filename
  } else {
    if (flip_number) {
      rvt.name <- sprintf('%i_%s.rvt',subID,stnName)
    } else {
      rvt.name <- sprintf('%s_%i.rvt',stnName,subID)
    }
  }

  fc <- file(rvt.name,open='w+')
  writeLines(sprintf(':%s HYDROGRAPH %i m3/s # %s',rvt_type,subID,rvt.name),fc)
  writeLines(sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(flow.series[1])),nrow(flow.series)),fc)
  for (j in 1:nrow(flow.series)) {
    writeLines(sprintf('%g',flow.series[j]),fc)
  }
  writeLines(sprintf(':End%s',rvt_type),fc)
  close(fc)

  # write to support file
  if (write_redirect) {
    writeLines(sprintf(':RedirectToFile %s',rvt.name),fc.redirect)
    close(fc.redirect)
  }

  return(TRUE)
}
