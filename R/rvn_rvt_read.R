#' @title Read .rvt (Raven time series) file
#'
#' @description
#' This routine reads in a valid Raven time series input (.rvt) file and returns the
#' information as an xts time series.
#'
#' @details
#' It supports :MultiData, :Data, :ObservedData,
#' :BasinInflowHydrograph, and most of the other :Data-like time series commands. It does
#' NOT support the master .rvt file with :Gauge or :GriddedForcing commands
#'
#' @param filename the name of the .rvt file (with .rvt extension included ), either relative
#' to the working directory or absolute.
#'
#' @return
#' Returns an xts time series with at least one dataset (multiple for :MultiData files)
#'
#' @author James R. Craig, University of Waterloo
#'
#' @examples
#' # read in rvt file
#' system.file('extdata','GlenAllan.rvt',package="RavenR")%>%
#' rvn_rvt_read(.) -> rvt
#' plot(rvt$TEMP_DAILY_MIN)
#'
#' @export rvn_rvt_read
#' @importFrom xts xts
#' @importFrom lubridate interval
rvn_rvt_read<-function(filename) {
  stopifnot(file.exists(filename))

  if (length(grep(":Gauge", readLines(filename))!=0)){
    print("rvn_rvt_read does not support reading of master model .rvt files, only data files ")
  }

  mult<-grep(":MultiData", readLines(filename))
  is_multidata<-!(length(mult)==0)

  #find location of start and end of data (assumes :Data)
  if (!is_multidata){
    lineno<-2 #default
    startfile<-grep(":", readLines(filename), value = FALSE)
    lineno<-startfile[length(startfile)-1]
    dateline<-startfile[length(startfile)-1]
    cnames<-c(filename)
  } else {
    lineno<-2 #default
    startfile<-grep(":", readLines(filename), value = FALSE)
    lineno<-startfile[length(startfile)-1]
    dateline<-startfile[length(startfile)-3]

    # read parameter names
    pline<-grep(":Parameters", readLines(filename), value = FALSE)
    pline<-unlist(strsplit(readLines(filename)[pline]," "))
    pline<-pline[pline!=""]

    cnames<-pline[2:length(pline)]
  }

  lineend<-grep(":End", readLines(filename), value = FALSE)

  delim=""
  if (length(grep(",", readLines(filename)[(lineno):(lineend-1)], value = FALSE))>0){
    delim="," # comma delimited
  }

  if ((length(lineno)==0) || (length(lineend)==0)){
    print('warning: filename not a valid .rvT file (no :End command)')
  }

  # read tabular data
  ts<-read.table(filename,
                      skip=lineno,
                      nrows=lineend-lineno-1,
                      sep=delim,
                      col.names=cnames,
                      header=FALSE,
                      blank.lines.skip=TRUE,
                      strip.white=TRUE,
                      stringsAsFactors=FALSE,
                      fill=TRUE, #fills empty data columns
                      flush=TRUE, #ignores extra data columns
                      comment.char = "#")
  ts[ts==-1.2345]<-NA #raven NA to NA

  # read date line
  line1<-unlist(strsplit(readLines(filename)[dateline]," "))
  line1<-line1[line1!=""]
  startdate<-as.Date(paste0(line1[1]," ",line1[2])  )

  # handles HH:mm:ss format *or* time in days
  interval<-as.double(as.POSIXct(line1[3],format = "%H:%M:%S")-as.POSIXct("00:00:00",format = "%H:%M:%S"))*60
  if (is.na(interval)){ #not in hh:mm:ss format
    interval<-as.integer(as.double(line1[3])*60*24) #rounds to nearest minute
  }
  dates<-seq(startdate, by = as.difftime(interval, units = "mins"), length.out = nrow(ts))

  timeser<-xts(ts,order.by=dates)
  return(timeser)
}
