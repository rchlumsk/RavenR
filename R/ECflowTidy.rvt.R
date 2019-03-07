#' EC Streamgauge File Conversion from tidyhydat
#'
#' ECflowTidy.rvt converts Environment Canada historical streamgauge data,
#' accessed via the tidyhydat package, into .rvt format files usable in
#' Raven.
#'
#' This function takes a single flow tibble generated from tidyhydat and converts the flow data for
#' each station in the file into .rvt formatted files for a Raven model. If
#' multiple stations exist in the .csv file, multiple observation files are created
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
#' write.redirect will print out the :RedirectToFile commands in a separate
#' file called, "flow_stn_redirect_text.rvt". These commands can be copied into
#' the main model's .rvt file to redirect to the produced time series files.
#'
#' flip.number is a useful option to place the subID first in the filename.
#' This is often cleaner for organizing files in a folder, since the
#' alphabeticized order is not dependent on the station name, and the observed
#' files will be in one set.
#'
#' @param indata tibble of WSC flow data from tidyhydat's hy_daily_flows() 
#' function
#' @param subIDs vector of subbasin IDs to correspond to the stations in ff
#' @param prd (optional) data period to use in .rvt file
#' @param stnNames (optional) character vector of alternative station names to
#' use
#' @param write.redirect (optional) write the :RedirectToFile commands in a
#' separate .rvt file
#' @param flip.number (optional) put the subID first in the .rvt filename
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#'
#' See also the \href{http://www.raven.uwaterloo.ca/}{Raven page}
#' @keywords Raven streamgauge flow rvt conversion tidyhydat
#' @examples
#' # warning: example not run, sample example for associated files only
#' \dontrun{
#' data<-hy_daily_flows(station_number = c("05CB004","05CA002"),start_date = "1996-01-01", end_date = "2000-01-01")
#' 
#' stationnames<-c('Raven River','Lower James River')
#'
#' ECflowTidy.rvt(data,subIDs=c(3,11),stnNames=stationnames,flip.number=T)
#' }
#'
#' @export ECflowTidy.rvt
ECflowTidy.rvt <- function(indata,subIDs,prd=NULL,stnNames=NULL,write.redirect=F,flip.number=F) {

  # data checks
  if (!(is.null(stnNames)) & (length(subIDs) != length(stnNames))) {
    stop("Length of subIDs must be the same as stnNames.")
  }

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

  stns<-pull(distinct(select(indata,STATION_NUMBER)),STATION_NUMBER)

  # begin writing the support file
  if (write.redirect) {
    fc.redirect <- file('flow_stn_redirect_text.rvt',open='a+')
  }

  # iterate through for all stations in the file
  for (i in 1:length(stns)) {
    
    dd.temp <- dd.temp <- filter(indata,STATION_NUMBER == stns[i])
    dd.temp <- select(dd.temp,Date,Value)
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

    # write .rvt file
    if (flip.number) {
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

    fc <- file(rvt.name,open='w+')
    writeLines(sprintf(':ObservationData HYDROGRAPH %i m3/s # %s',subIDs[i],rvt.name),fc)
    writeLines(sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(ts.temp[1])),nrow(ts.temp)),fc)
    for (j in 1:nrow(ts.temp)) {
      writeLines(sprintf('%g',ts.temp[j]),fc)
    }
    writeLines(':EndObservationData',fc)
    close(fc)

    # write to support file
    if (write.redirect) {
      writeLines(sprintf(':RedirectToFile %s',rvt.name),fc.redirect)
    }
  }

  if (write.redirect) {
    close(fc.redirect)
  }
  return(TRUE)
}

