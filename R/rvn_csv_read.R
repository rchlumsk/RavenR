#' @title Read in generic Raven output csv files
#'
#' @description
#' Reads in output csv files produced by Raven.
#'
#' @details
#' Expects a full file path to the Raven output file.
#'
#' The timezone is provided by the tzone argument as "UTC" by default, and should be adjusted by
#' the user to the local time zone as needed, based on the model run.
#'
#' @param ff full file path to the csv file
#' @param tzone string indicating the timezone of the data in ff
#' @param xtsformat boolean whether to return in xts format (if date and/or hour found)
#' @return data frame (as xts if set with \code{xtsformat}) read from the file
#' @seealso \code{\link{rvn_hyd_read}} for reading Hydrographs output files
#'
#' @examples
#' # create full file path
#' ff <- system.file("extdata","ReservoirStages.csv", package="RavenR")
#'
#' # read in the Reservoir file with the generic call
#' myres <- rvn_csv_read(ff)
#'
#' # view contents
#' head(myres)
#'
#' @export rvn_csv_read
#' @importFrom xts xts
#' @importFrom utils read.csv
rvn_csv_read <- function(ff=NA, tzone="UTC", xtsformat=TRUE) {

  if (missing(ff)) {
    stop("Requires the full file path to the Raven csv output file.")
  }

  #read output
  dd <- read.csv(ff,header=TRUE,nrows=5)
  # careful in date-time formats; excel can screw it up if csv is saved over. This works for
  # an untouched Raven output file

  cols <- colnames(dd)
  classes <- rep(NA,length(cols))
  if ("time" %in% cols) {
    classes[which(cols=="time")] <- "integer"
  }
  if ("date" %in% cols) {
    classes[which(cols=="date")] <- "Date"
  }
  if ("hour" %in% cols) {
    classes[which(cols=="hour")] <- "character"
  }

  # re-read with specified colClasses
  dd <- read.csv(ff,header=TRUE,colClasses = classes,na.strings=c("---",'NA'))

  dt <- NULL
  if ("date" %in% cols) {
    if ("hour" %in% cols) {
      dt <- as.POSIXct(paste(dd$date,dd$hour), format="%Y-%m-%d %H:%M:%S", tz=tzone)
    } else {
      dt <- as.Date(dd$date)
    }
    # check if dt converted properly
    if (any(is.na(dt))) {
      warning("date and time failed to convert from date and hour columns, please check file format.")
      dt <- NULL
    }
  }

  # change all dots to underscore
  newcols <-
    gsub("\\.","_",x=gsub("\\.\\.","\\.",x=gsub("\\.\\.\\.","\\.\\.",x=cols)))

  # trim all last underscores
  for (i in 1:length(newcols)) {
    if (rvn_substrRight(newcols[i],1) == "_") {
      newcols[i] <- rvn_substrMRight(newcols[i],1)
    }
  }
  colnames(dd) <- newcols

  # change to xtsformat if desired
  if (xtsformat & !is.null(dt)) {
    dd <- xts(order.by=dt,x=dd)
  }

  return(dd)
}
