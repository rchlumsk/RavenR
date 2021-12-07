#' @title EC Streamgauge File Conversion from tidyhydat
#'
#' @description
#' rvn_rvt_tidyhydat converts Environment Canada historical streamgauge data,
#' accessed via the tidyhydat package, into .rvt format files usable in
#' Raven.
#'
#' @details
#' Takes a single flow tibble generated from tidyhydat and converts the flow data for
#' each station in the file into .rvt formatted files for a Raven model. If
#' multiple stations exist in indata, multiple observation files are created. This function
#' is a wrapper for \code{rvn_rvt_write}, with the benefit of automatically parsing the
#' tidyhydat download for possibly multiple stations in xts formats before passing
#' to the rvt writing function.
#'
#' rvt_type is the specified rvt file type to write to (see the Raven User's Manual
#' or the \code{rvn_rvt_mappings} function for more rvt types). This should be a flow-based
#' rvt type, such as ObservationData, BasinInflowHydrograph, ReservoirExtraction, etc.
#' Most applications of this will likely write the tidyhydat observations as
#' ObservedData for use in model evaluation to historic records.
#'
#' data_type is the type of Raven input data type, likely 'HYDROGRAPH', for the corresponding
#' flow data. If the flow is used as a reservoir-related flow, the data_type may be
#' RESERVOIR_INFLOW or RESERVOIR_NETINFLOW.
#'
#' subIDs is required and should correspond to the subID to be used in the .rvt
#' file for each station in the ff file, in the order in which it will be read
#' in.
#'
#' prd is used by the xts formatted-data to restrict the data reported in .rvt
#' files, for each station, to this period. The prd should be defined in
#' "YYYY-MM-DD/YYYY-MM-DD" string format. If the period supplied results in an
#' empty time series (i.e. non-overlapping time periods), an error will be
#' thrown.
#'
#' stnNames is an optional character vector to replace the EC station codes
#' found in the HYDAT database. If supplied, the vector must be of the same length
#' as the number of stations supplied and the subIDs vector. If not supplied,
#' the EC station codes will be used. Note that this does not impact model
#' function, only filename readability and station recognition.
#'
#' write_redirect will print out the :RedirectToFile commands in a separate
#' file called, "flow_stn_redirect_text.rvt". These commands can be copied into
#' the main model's .rvt file to redirect to the produced time series files.
#'
#' flip_number is a useful option to place the subID first in the filename.
#' This is often cleaner for organizing files in a folder, since the
#' alphabetized order is not dependent on the station name, and the observed
#' files will be in one set.
#'
#' The function will write to name generated from the station name(s), otherwise
#' the .rvt filename may be specified with the filename argument (full path to
#' the filename, including .rvt extension). If multiple stations are provided,
#' the filename argument may be a vector of filenames.
#'
#' Note that the function uses
#' \code{sort(unique(indata$STATION_NUMBER))} to determine the order of stations,
#' thus the filenames and stnNames should correspond to the sorted vector of station numbers as well.
#'
#' Note that only daily flow data is supported, as tidyhydat only allows for the
#' download of daily (or coarser resolution) flow data. Hourly data that is obtained
#' must be first processed into xts format and then written with \code{\link{rvn_rvt_write}}.
#'
#' If the data is found to have an inconsistent timestep, the function will attempt to correct it
#' by infilling missing time steps with \code{\link{rvn_ts_infill}}. If successful, a warning is issued
#' to the user and the function will proceed, else an error will be raised.
#'
#' @param indata tibble of WSC flow data from tidyhydat's hy_daily_flows() function
#' @param rvt_type type of rvt file to write (e.g. ObservationData, BasinInflowHydrograph, etc.)
#' @param data_type Raven-syntax data type for flow data (default 'HYDROGRAPH')
#' @param subIDs vector of subbasin IDs to correspond to the stations in indata
#' @param prd (optional) data period to use in .rvt file
#' @param stnNames (optional) character vector of alternative station names to use
#' @param write_redirect (optional) write the :RedirectToFile commands in a separate .rvt file
#' @param rd_file (optional) name of the redirect file created (if write_redirect = TRUE)
#' @param flip_number (optional) put the subID first in the .rvt filename
#' @param filename specified name of file(s) to write to (optional)
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#'
#' @examples
#'
#' # note: example modified to avoid using tidyhydat directly, uses saved
#' ## tidyhydat data from RavenR package sample data
#' # library(tidyhydat)
#' stations <- c("05CB004","05CA002")
#'
#' # Gather station data/info using tidyhydat functions
#' # hd <- tidyhydat::hy_daily_flows(station_number = stations,
#' #  start_date = "1996-01-01", end_date = "1997-01-01")
#' data(rvn_tidyhydat_sample)
#' hd <- rvn_tidyhydat_sample
#'
#' # station_info <- hy_stations(stations)
#'
#' tf1 <- file.path(tempdir(), "station1.rvt")
#' tf2 <- file.path(tempdir(), "station2.rvt")
#'
#' # Create RVT files
#' rvn_rvt_tidyhydat(hd, subIDs=c(3,11),
#'   filename=c(tf1,tf2))
#'
#' @export rvn_rvt_tidyhydat
#' @importFrom xts xts
rvn_rvt_tidyhydat <- function(indata, rvt_type="ObservationData", data_type="HYDROGRAPH",
                              subIDs, prd=NULL, stnNames=NULL,
                              write_redirect=FALSE, flip_number=FALSE,
                              rd_file = 'flow_stn_redirect_text.rvt',
                              filename=NULL)
{

  STATION_NUMBER <- Date <- Value <- NULL

  # data checks
  if (!(is.null(stnNames)) & (length(subIDs) != length(stnNames))) {
    stop("Length of subIDs must be the same as stnNames.")
  }

  stns <- sort(unique(indata$STATION_NUMBER))

  # begin writing the support file
  if (write_redirect) {
    fc.redirect <- file(rd_file, open='a+')
  }

  # iterate through for all stations in the file
  for (i in 1:length(stns)) {

    dd.temp <- indata[indata$STATION_NUMBER == stns[i],c("Date","Value")]
    ts.temp <- xts(order.by=as.Date(dd.temp$Date,format="%Y/%m/%d"),x=dd.temp$Value)

    if (!(is.null(prd))) {
      ts.temp <- ts.temp[prd]
    }

    # change all NA values to Raven blank (-1.2345)
    ts.temp[is.na(ts.temp)] = -1.2345

    # check for empty time series
    if (nrow(ts.temp)==0) {
      close(fc.redirect)
      stop(sprintf("Empty time series for station %s, check the supplied period and/or the availability of flow data in the supplied file.",stns[i]))
    }

    # determine file name
    if (!is.null(filename)) {
      rvt.name <- filename[i]
    } else {
      if (flip_number) {
        if (!(is.null(stnNames))) {
          rvt.name <- sprintf('%i_%s.rvt',subIDs[i],stnNames[i])
        } else {
          rvt.name <- sprintf('%i_%s.rvt',subIDs[i],stns[i])
        }
      } else {
        if (!(is.null(stnNames))) {
          rvt.name <- sprintf('%s_%i.rvt',stnNames[i],subIDs[i])
        } else {
          rvt.name <- sprintf('%s_%i.rvt',stns[i],subIDs[i])
        }
      }
    }

    # check for consistency in non-irregular time series; infill if needed
    if (length(grep("Irregular", x=rvt_type)) != 1) {
      difftime_check <- difftime(ts.temp[2:nrow(ts.temp)], ts.temp[1:(nrow(ts.temp)-1)], units="day")
      if (any(difftime_check != difftime_check[1])) { # inconsistent timesteps found

        # check length, is less than expected from time interval and start datetime
        if (length(seq.POSIXt(from=as_datetime(ts.temp[1]),
                              by=difftime_check[1], to=as_datetime(ts.temp[nrow(ts.temp)]))) > nrow(ts.temp)) {
          # length of ts.temp is less than expected, attempt to infill
          ts.temp_infilled <- rvn_ts_infill(ts.temp)

          # recheck difftime
          difftime_check <- difftime(ts.temp_infilled[2:nrow(ts.temp_infilled)], ts.temp_infilled[1:(nrow(ts.temp_infilled)-1)], units="day")
          if (any(difftime_check != difftime_check[1])) {
            # unable to fix with rvn_ts_infill
            stop(sprintf("rvn_rvt_tidyhydat: Inconsistent timesteps found in data for %s, which were not rectified with rvn_ts_infill.\nPlease review and fix time step issues in time series.",
                         rvt.name))
          } else {
            # time series fixed with rvn_ts_infill, replace ts.temp and proceed
            ts.temp <- ts.temp_infilled
            warning(sprintf("rvn_rvt_tidyhydat: Time series for %s adjusted with rvn_ts_infill.", rvt.name))
          }

        } else {
          # length longer than expected, likely that some smaller frequency points exist.
          # print(sprintf("error encountered on iteration %i",i))
          stop(sprintf("rvn_rvt_tidyhydat: Inconsistent timesteps found in data for %s, and series is longer than anticipated.\nConsider reducing temporal resolution to a consistent one with xts::apply* functions and/or infilling with rvn_ts_infill.",
                       rvt.name))
        }
      }
      # time steps consistent (perhaps corrected with rvn_ts_infill) - proceed
    }

    # send to rvn_rvt_write
    rvn_rvt_write(x=ts.temp, filename=rvt.name, rvt_type=rvt_type,
                  data_type=data_type,
                  basin_ID=subIDs[i])

    # write to support file
    if (write_redirect) {
      writeLines(sprintf(':RedirectToFile %s',rvt.name),fc.redirect)
    }
  }

  if (write_redirect) {
    close(fc.redirect)
  }
  return(TRUE)
}
