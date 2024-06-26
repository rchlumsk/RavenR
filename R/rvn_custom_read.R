#' @title Read Raven Custom Output files
#'
#' @description
#' rvn_custom_read is used to read any Raven custom output file
#'
#' @details
#' rvn_custom_read parses the filename and predicts the file format accordingly, so
#' it is important to use the unmodified file names for this function. The use
#' (or not) of a runname is accounted for.
#'
#' The returned object is a time series object (xts format), which can be used
#' to easily plot the time series data. The options of the custom output are
#' included in the rav.obj attributes.
#'
#' The timezone is provided by the tzone argument as "UTC" by default, and should be adjusted by
#' the user to the local time zone as needed, based on the model run.
#'
#' @param ff full file path to the custom output file
#' @param no_runname boolean for whether a runName is supplied, important for
#' parsing the filename
#' @param tzone string indicating the timezone of the data in ff
#' @return \item{custom_out}{data frame with the custom output data stored as xts
#' object}
#' @seealso \code{\link{rvn_custom_output_plot}} for plotting custom output
#'
#' @examples
#'
#' # find sample rvh file for Nith subwatershed
#' ff <- system.file("extdata","run1_SNOW_Daily_Average_ByHRU.csv", package="RavenR")
#'
#' # extract and plot custom data
#' mycustomdata <- rvn_custom_read(ff)
#' summary(mycustomdata[,1:5])
#' plot(mycustomdata[,5],main='Daily Average SNOW - HRU 5')
#'
#' @export rvn_custom_read
#' @importFrom xts xts
#' @importFrom utils read.csv
rvn_custom_read <- function(ff=NA, no_runname=FALSE, tzone="UTC") {

  if (missing(ff)) {
    stop("Requires the full file path to the Raven custom output file")
  }

  if(!file.exists(ff)){ stop("specified file name/path does not exist") }

  fname <- unlist(strsplit(unlist(strsplit(ff,".csv"))[1],"/"))[length(unlist(strsplit(ff,"/")))]
  namelist <- unlist(strsplit(fname,"_"))

  # JRC: to fix: currently won't be able to handle long variable name with no runname prefix unless
  #              no_runname is specified (i.e., it won't figure it out on its own)

  # determine properties from file name
  if (length(namelist) >= 5) {
    if (no_runname==FALSE){
      runname <- namelist[1]
      vv <- paste(namelist[2:(length(namelist)-3)],collapse="_")
    } else {
      runname <-''
      vv <- paste(namelist[1:(length(namelist)-3)],collapse="_")
    }
    time.type <- namelist[length(namelist)-2]
    stat.type <- namelist[length(namelist)-1]
    space.type <- namelist[length(namelist)]
  } else {
	  runname=''
    vv <- namelist[1]
    time.type <- namelist[2]
    stat.type <- namelist[3]
    space.type <- namelist[4]
  }

  cust.data    <- read.csv(ff,header=F,skip=2,stringsAsFactors=F)
  cust.data    <- cust.data[,1:(ncol(cust.data)-1)] # remove NA column
  cust.headers <- read.csv(ff,header=F,nrows=2,skip=0,stringsAsFactors = F)
  cust.headers <- cust.headers[,1:(ncol(cust.headers)-1)]

  ## time property handling
  # obtain time object, trim columns
  # need to update to handle hourly and yearly data *********
  if (time.type == "Continuous") {
    if (is.null(tzone)) {
      #LS - Continuous has an hours column
      tt <- as.POSIXct(paste(cust.data[,2], cust.data[,3]), format="%Y-%m-%d %H:%M:%S")
    } else {
      tt <- as.POSIXct(paste(cust.data[,2], cust.data[,3]), format="%Y-%m-%d %H:%M:%S", tz=tzone)
    }

    # date.time <- as.POSIXct(paste(hydrographs$date,hydrographs$hour), format="%Y-%m-%d %H:%M:%S")
    cust.data <- cust.data[,-(1:3)]
    cust.headers <- cust.headers[1,-(1:3)]

  } else if (time.type == "Daily") {

    if (is.null(tzone)) {
      tt <- as.POSIXct(paste(cust.data[,2]), format="%Y-%m-%d")
    } else {
      tt <- as.POSIXct(paste(cust.data[,2]), format="%Y-%m-%d", tz=tzone)
    }

    # date.time <- as.POSIXct(paste(hydrographs$date,hydrographs$hour), format="%Y-%m-%d %H:%M:%S")
    cust.data <- cust.data[,-(1:2)]
    cust.headers <- cust.headers[1,-(1:2)]

  } else if (time.type == "Monthly") {
    if (is.null(tzone)) {
      tt <- as.POSIXct(paste0(cust.data[,2],rep("-1",nrow(cust.data)) ), format="%Y-%m-%d")
    } else {
      tt <- as.POSIXct(paste0(cust.data[,2],rep("-1",nrow(cust.data)) ), format="%Y-%m-%d", tz=tzone)
    }
    # tt <- as.yearmon(as.character(cust.data[,2]), "%Y-%m")
    cust.data <- cust.data[,-(1:2)]
    cust.headers <- cust.headers[1,-(1:2)]

  } else if (time.type == "Yearly") {
    if (is.null(tzone)) {
      tt <- as.POSIXct(paste0(cust.data[,2],rep("-12",nrow(cust.data)),rep("-1",nrow(cust.data))), format="%Y-%m-%d")
    } else {
      tt <- as.POSIXct(paste0(cust.data[,2],rep("-12",nrow(cust.data)),rep("-1",nrow(cust.data))), format="%Y-%m-%d")
    }
    cust.data <- cust.data[,-(1:2)]
    cust.headers <- cust.headers[1,-(1:2)]

  } else if (time.type == "WYearly") {
    # graps the first year in the wyear column, attaches month=12 and day=1 to it
    #   adds that y-m-d as the timestamp in the time series
    # can adjust to the second year or any month/day desired
    # to change the year in the split, change the [[1]] to [[2]] in the line below
    yrs <- t(sapply(cust.data[,2],FUN=function(x) strsplit(x,"-")[[1]],USE.NAMES=F)) # split into matrix
    if (is.null(tzone)) {
      tt <- as.POSIXct(paste0(yrs[,1],rep("-12",nrow(cust.data)),rep("-1",nrow(cust.data))), format="%Y-%m-%d")
    } else {
      tt <- as.POSIXct(paste0(yrs[,1],rep("-12",nrow(cust.data)),rep("-1",nrow(cust.data))), format="%Y-%m-%d", tz=tzone)
    }
     cust.data <- cust.data[,-(1:2)]
     cust.headers <- cust.headers[1,-(1:2)]
  }

  if (NCOL(cust.data) != 1) {
    colnames(cust.data) <- as.matrix(cust.headers)
  }

  # create xts custom data object
  dd <- xts(cust.data, order.by=tt)
  names(dd) <- as.character(names(dd)) # convert to character
  # should consider adding SUB or HRU prefix based on spatial aggregation

  # adds attributes to xts custom data object
    # replace with xtsAttribute ??
  attr(dd,'runname')<-runname
  attr(dd,'datatype')<-vv
  attr(dd,'time_agg')<-time.type
  attr(dd,'stat_agg')<-stat.type
  attr(dd,'space_agg')<-space.type

  # Used by rvn_rvc_from_custom_output
  if(space.type == 'ByHRU') {
    attr(dd,'HRUs') <- ncol(cust.data)
  } else if (space.type == 'BySubbasin') {
    attr(dd, 'SBs') <- ncol(cust.data)
  } #TODO HRUGroup, EntireWatershed

  return("custom_out"=dd)
}
