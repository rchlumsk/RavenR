#' Read Raven Custom Output files
#'
#' custom.read is used to read any Raven custom output file
#'
#' custom.read parses the filename and predicts the file format accordingly, so
#' it is important to use the unmodified file names for this function. The use
#' (or not) of a runname is accounted for.
#'
#' The returned object is a time series object (xts format), which can be used
#' to easily plot the time series data. The otpions of the custom output are
#' included in the rav.obj attributes.
#'
#' @param ff full file path to the custom output file
#' @param no.runname boolean for whether a runName is supplied, important for
#' parsing the filename
#' @return \item{custom.out}{data frame with the custom output data stored as xts
#' object}
#' @seealso \code{\link{customoutput.plot}} for plotting custom output
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven read custom output
#' @examples
#'
#'# find sample rvh file for Nith subwatershed
#'ff <- system.file("extdata","run1_SNOW_Daily_Average_ByHRU.csv", package="RavenR")
#'
#'# extract and plot custom data
#'mycustomdata <- custom.read(ff)
#'plot(mycustomdata[,5],main='Daily Average SNOW - HRU 5')
#'
#' @export custom.read
custom.read <- function(ff=NA, no.runname=F) {

  if (missing(ff)) {
    stop("Requires the full file path to the Raven custom output file")
  }

  if(!file.exists(ff)){ stop("specified file name/path does not exist") }

  fname <- unlist(strsplit(unlist(strsplit(ff,".csv"))[1],"/"))[length(unlist(strsplit(ff,"/")))]
  namelist <- unlist(strsplit(fname,"_"))

  # JRC: to fix: currently won't be able to handle long variable name with no runname prefix unless
  #              no.runname is specified (i.e., it won't figure it out on its own)

  # determine properties from file name
  if (length(namelist) >= 5) {
    if (no.runname==F){
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

  cust.data    <- read.csv(ff,header=F,skip=2,stringsAsFactors=F);
  cust.data    <- cust.data[,1:(ncol(cust.data)-1)] # remove NA column
  cust.headers <- read.csv(ff,header=F,nrows=2,skip=0,stringsAsFactors = F);
  cust.headers <- cust.headers[,1:(ncol(cust.headers)-1)]

  ## time property handling
  # obtain time object, trim columns
  # need to update to handle hourly and yearly data *********
  if (time.type == "Continuous" | time.type == "Daily") {
    tt <- as.POSIXct(paste(cust.data[,2]), format="%Y-%m-%d")
    # date.time <- as.POSIXct(paste(hydrographs$date,hydrographs$hour), format="%Y-%m-%d %H:%M:%S")
    cust.data <- cust.data[,-(1:2)]
    cust.headers <- cust.headers[1,-(1:2)]

  } else if (time.type == "Monthly") {
    tt <- as.POSIXct(paste0(cust.data[,2],rep("-1",nrow(cust.data)) ), format="%Y-%m-%d")
    # tt <- as.yearmon(as.character(cust.data[,2]), "%Y-%m")
    cust.data <- cust.data[,-(1:2)]
    cust.headers <- cust.headers[1,-(1:2)]

  } else if (time.type == "Yearly") {
    tt <- as.POSIXct(paste0(cust.data[,2],rep("-12",nrow(cust.data)),rep("-1",nrow(cust.data))), format="%Y-%m-%d")
    cust.data <- cust.data[,-(1:2)]
    cust.headers <- cust.headers[1,-(1:2)]

  } else if (time.type == "WYearly") {
    # graps the first year in the wyear column, attaches month=12 and day=1 to it
    #   adds that y-m-d as the timestamp in the time series
    # can adjust to the second year or any month/day desired
    # to change the year in the split, change the [[1]] to [[2]] in the line below
    yrs <- t(sapply(cust.data[,2],FUN=function(x) strsplit(x,"-")[[1]],USE.NAMES=F)) # split into matrix
   tt <- as.POSIXct(paste0(yrs[,1],rep("-12",nrow(cust.data)),rep("-1",nrow(cust.data))), format="%Y-%m-%d")
   cust.data <- cust.data[,-(1:2)]
   cust.headers <- cust.headers[1,-(1:2)]
  }

  if (NCOL(cust.data) != 1) {
    colnames(cust.data) <- as.matrix(cust.headers)
  }

  # create xts custom data object
  dd <- xts(cust.data, order.by=tt)

  # adds attributes to xts custom data object
    # replace with xtsAttribute ??
  attr(dd,'runname')<-runname
  attr(dd,'datatype')<-vv
  attr(dd,'time.agg')<-time.type
  attr(dd,'stat.agg')<-stat.type
  attr(dd,'space.agg')<-space.type

  return("custom.out"=dd)
}
