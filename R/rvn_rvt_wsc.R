#' @title EC Streamgauge File Conversion
#'
#' @description
#' rvn_rvt_wsc converts Environment Canada historical streamgauge data,
#' downloaded from the Water Survey Canada, into .rvt format files usable in
#' Raven.
#'
#' @details
#' This function takes a single WSC flow file and converts the flow data for
#' each station in the file into .rvt formatted files for a Raven model. If
#' multiple stations exist in the .csv file,
#'
#' The file should be downloaded in a .csv Date-Data format with missing days
#' included. The download website is linked below. Any level data will be
#' ignored, although can be included in the file for stations with flow and
#' level data. If no data is found for a given station, an error will be
#' reported.
#'
#' subIDs is required and should correspond to the subID to be used in the .rvt
#' file for each station in the ff file, in the order in which it will be read
#' in. If the subbasin is currently unknown, please supply a placeholder value.
#'
#' prd is used by the xts formatted-data to restrict the data reported in .rvt
#' files, for each station, to this period. The prd should be defined in
#' "YYYY-MM-DD/YYYY-MM-DD" string format. If the period supplied results in an
#' empty time series (i.e. non-overlapping time periods), an error will be
#' thrown.
#'
#' stnNames is an optional character vector to replace the EC station codes
#' found in the .csv file. If supplied, the vector must be of the same length
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
#' alphabeticized order is not dependent on the station name, and the observed
#' files will be in one set.
#'
#' The function will write to name generated from the station name(s), otherwise
#' the .rvt filename may be specified with the filename argument (full path to
#' the filename, including .rvt extension). If multiple stations are provided,
#' the filename argument may be a vector of filenames.
#'
#' @param ff WSC flow file in csv format
#' @param subIDs vector of subbasin IDs to correspond to the stations in ff
#' @param prd (optional) data period to use in .rvt file
#' @param stnNames (optional) character vector of alternative station names to
#' use
#' @param write_redirect (optional) write the :RedirectToFile commands in a
#' separate .rvt file
#' @param flip_number (optional) put the subID first in the .rvt filename
#' @param filename specified name of file(s) to write to (optional)
#'
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{rvn_annual_peak_event}} to consider the timing of peak
#' events
#'
#' Download EC streamgauge data from
#' \href{https://wateroffice.ec.gc.ca/search/historical_e.html}{WSC Historical
#' Data}.
#'
#' @examples
#' ff = system.file("extdata",'Daily__Oct-1-2020_08_20_52PM.csv', package="RavenR")
#' rvn_rvt_wsc(ff,subIDs=c(6), filename=file.path(tempdir(), "stn_02GB001.rvt"))
#'
#' @export rvn_rvt_wsc
#' @importFrom xts xts
#' @importFrom lubridate date
rvn_rvt_wsc <- function(ff,subIDs,prd=NULL,stnNames=NULL,write_redirect=FALSE,flip_number=FALSE, filename=NULL)
{

  # data checks
  if (!(is.null(stnNames)) & (length(subIDs) != length(stnNames))) {
    stop("Length of subIDs must be the same as stnNames.")
  }

  # PARAMETERS
  # param (flow) == 1
  # param (level) == 2

  # SYMBOLS - not currently reported
  #

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

  # check the stations in the supplied file
  dd <- read.table(ff,sep=",",skip=1,header=TRUE)

  # fix to handle multi-byte marker/byte order marker,
  #   appears if there is more than one station per file
  stns <- as.character(unique(iconv(dd$ID,to="ASCII")))
  stns <- stns[!(is.na(stns))]
  if (length(stns) != length(subIDs)) {
    stop(sprintf("Number of stations found in file not equal to the length of subIDs or stnNames, please check the
                 supplied file and function inputs. Found %i stations, %s",length(stns),toString(paste(stns))))
  }

  # begin writing the support file
  if (write_redirect) {
    fc.redirect <- file('flow_stn_redirect_text.rvt',open='a+')
  }

  # iterate through for all stations in the file
  for (i in 1:length(stns)) {
    dd.temp <- dd[(dd$ID == stns[i] & dd$PARAM == 1),]
    # date.temp <- as.Date(dd.temp$Date,format="%Y/%m/%d")
    ts.temp <- xts(order.by=as.Date(dd.temp$Date,format="%Y/%m/%d"),x=dd.temp$Value)
    if (!(is.null(prd))) {
      ts.temp <- ts.temp[prd]
    }
    # change all NA values to Raven NA (-1.2345)
    ts.temp[is.na(ts.temp)] = -1.2345
    # check for empty time series
    if (nrow(ts.temp)==0) {
      close(fc.redirect)
      stop(sprintf("Empty time series for station %s, check the supplied period and/or the availability of flow data in the supplied file.",stns[i]))
    }

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

    fc <- file(rvt.name,open='w+')
    writeLines(sprintf(':ObservationData HYDROGRAPH %i m3/s # %s',subIDs[i],rvt.name),fc)
    writeLines(sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(ts.temp[1])),nrow(ts.temp)),fc)
    for (j in 1:nrow(ts.temp)) {
      writeLines(sprintf('%g',ts.temp[j]),fc)
    }
    writeLines(':EndObservationData',fc)
    close(fc)

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

