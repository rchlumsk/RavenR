#' @title Read in Raven Hydrograph file
#'
#' @description
#' rvn_hyd_read is used to read in the Hydrographs.csv file produced by the
#' modelling Framework Raven.
#'
#' @details
#' This function expects a full file path to the Hydrographs.csv file, then
#' reads in the file using fread. The main advantage of this function is
#' renaming the columns to nicer names and extracting the units into something
#' much easier to read.
#'
#' This function is also built to support the rvn_hyd_extract function, which uses
#' the object created here for extracting by reference to the columns named
#' here, for example sub24.
#'
#' ff is the full file path of the Hydrographs.csv file. If the file is located
#' in the current working directory, then simply the name of the file is
#' sufficient.
#'
#' tzone is a string indicating the timezone of the supplied Hydrographs file. The
#' timezone provided is coded into the resulting hyd data frame using the as.POSIXct
#' function. If no timezone is provided, this is left as an empty string, and is
#' determined by the function as the current time zone.
#'
#' @param ff full file path to the Hydrographs.csv file
#' @param tzone string indicating the timezone of the data in ff
#' @return \item{hyd}{data frame from the file with standardized names}
#' @seealso \code{\link{rvn_hyd_extract}} for extraction tools related to the
#' rvn_hyd_read output file
#'
#' @examples
#' # read in hydrograph sample csv data from RavenR package
#' ff <- system.file("extdata","run1_Hydrographs.csv", package="RavenR")
#'
#' # read in Raven Hydrographs file, store into myhyd
#' myhyd <- rvn_hyd_read(ff)
#'
#' # view contents
#' head(myhyd$hyd)
#' myhyd$units
#'
#' @export rvn_hyd_read
#' @importFrom xts xts
#' @importFrom utils read.csv
rvn_hyd_read <- function(ff=NA, tzone=NULL) {

  if (missing(ff)) {
    stop("Requires the full file path to the Hydrographs.csv file.")
  }

  #read hydrograph output
  hydrographs <- read.csv(ff,header=TRUE,nrows=5)

  # assumed colClasses structure - mostly numeric except date and hour columns
  classes <- c(c('numeric','character','character'),rep('numeric',ncol(hydrographs)-3))

  # re-read with specified colClasses
  hydrographs <- read.csv(ff,header=TRUE,colClasses = classes,na.strings=c("---",'NA'))

  # need to fix the hourly model
  if (is.null(tzone)) {
    date.time <- as.POSIXct(paste(hydrographs$date,hydrographs$hour), format="%Y-%m-%d %H:%M:%S")
  } else {
    date.time <- as.POSIXct(paste(hydrographs$date,hydrographs$hour), format="%Y-%m-%d %H:%M:%S",tz=tzone)
  }

  if (length(which(is.na(date.time)))>0){
    print("rvn_hyd_read: Error in mapping day/time to POSIXct. Must be timezone without daylight savings")
    return(FALSE)
  }
  cols <- colnames(hydrographs)

  # temporary fix while precip column leaves no space between precip and units
  if ("precip.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "precip.mm.day."),"precip..mm.day.")
  }

  # change all "..." to ".." in cols
  newcols <- gsub("\\.\\.\\.","\\.\\.",cols)

  # setup units and obs_flag
  units <- matrix(data=NA,nrow=length(cols))
  obs_flag <- matrix(data=NA,nrow=length(cols))

  # find index where precip column starts
  pcol <- grep("precip*",cols)
  Ncol <- length(cols)

  # split the col names into units, observed flags
  for (i in pcol:Ncol) {
    mysplit <- unlist(strsplit(newcols[i],"\\.\\."))

    if (length(mysplit) == 2) {
      units[i] = mysplit[2]
      obs_flag[i] = FALSE
      newcols[i] = mysplit[1]
    } else if (length(mysplit) >= 3) {
      if (mysplit[2] == "observed") {
        units[i] = mysplit[3]
        obs_flag[i] = TRUE
        newcols[i] = sprintf("%s_obs",mysplit[1])
      } else if (mysplit[2] == "inflow") {
        units[i] = mysplit[3]
        obs_flag[i] = FALSE
        newcols[i] = sprintf("%s_resinflow",mysplit[1])
      } else if (mysplit[3] == "inflow") {
        units[i] = mysplit[4]
        obs_flag[i] = FALSE
        newcols[i] = sprintf("%s_resinflow",mysplit[1])
      }
    }
  }

  # add the date time object, replace time date hour bits
  hydrographs <- hydrographs[,pcol:Ncol]
  newcols <- newcols[pcol:Ncol]
  units <- units[pcol:Ncol]
  obs_flag <- obs_flag[pcol:Ncol]
  hydrographs <- xts(order.by=date.time,x=hydrographs)

  # assign new column names
  colnames(hydrographs) <- newcols

  # manual correct for units
  # remove trailing "." in unit labels
  # for (i in 1:length(units)) {
  #   if (substr(units[i], nchar(units[i]), nchar(units[i])) == ".") {
  #     units[i] = substr(units[i],1,nchar(units[i])-1)
  #   }
  # }

  # temporary correction
  units <- replace(units,which(units == "m3.s."),"m3/s")
  units <- replace(units,which(units == "mm.day."),"mm/day")

  return(list("hyd" = hydrographs, "units" = units, "obs.flag" = obs_flag))
}
