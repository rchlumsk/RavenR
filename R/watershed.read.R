#' Read in Raven WatershedStorage file
#'
#' watershed.read is used to read in the WatershedStorage.csv file produced by
#' the modelling Framework Raven.
#'
#' This function expects a full file path to the WatershedStorage.csv file,
#' then reads in the file using read.csv. The main advantage of this functon is
#' renaming the columns to nicer names and extracting the units into something
#' much easier to read.
#'
#' This function is also built to support the wshd.animate function, which uses
#' the object created here for creating an animation of the watershed storage
#' containers.
#'
#' ff is the full file path of the WatershedStorage.csv file. If the file is
#' located in the current working directory, then simply the name of the file
#' is sufficient.
#'
#' @param ff full file path to the WatershedStorage.csv file
#' @return \item{watershed.storage}{data frame from the file with standardized
#' names} \item{units}{vector corresponding to units of each column}
#' @seealso \code{\link{hyd.read}} for reading in the Hydrographs.csv file
#' \code{\link{watershedmeb.read}} for reading in the
#' WatershedMassEnergyBalance.csv file
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven read.csv watershed storage
#' @examples
#'
#' # locate in RavenR Watershed storage file
#' ff <- system.file("extdata","run1_WatershedStorage.csv", package="RavenR")
#'
#' # create full file path and read in file
#' mywshd <- watershed.read(ff)
#'
#' # check data
#' head(mywshd$watershed.storage)
#'
#' @export watershed.read
watershed.read <- function(ff=NA) {

  if (missing(ff)) {
    stop("Requires the full file path to the WatershedStorage.csv file")
  }

  # test reading and get format, number of columns
  watersheds <- read.csv(ff,header=T,nrows=5)
  classes <- c(c('numeric','character','character'),rep('numeric',ncol(watersheds)-3))

  # re-read with specified colClasses
  watersheds <- read.csv(ff,header=T,colClasses = classes,na.strings=c("---",'NA','1.#INF'))

  # careful in date-time formats; excel can screw it up if csv is saved over. This works for
  # un untouched Raven output file
  date.time <- as.POSIXct(paste(watersheds$date,watersheds$hour), format="%Y-%m-%d %H:%M:%S")
  # head(date.time)
  cols <- colnames(watersheds)

  # temporary fix while precip column leaves no space between precip and units
  if ("precip.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "precip.mm.day."),"precip..mm.day.")
  }
  if ("rainfall.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "rainfall.mm.day."),"rainfall..mm.day.")
  }
  if ("snowfall.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "snowfall.mm.day."),"snowfall..mm.day.")
  }
    if ("rain.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "rain.mm.day."),"rain..mm.day.")
  }
  if ("snowfall.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "snow.mm.day."),"snow..mm.day.")
  }

  # change all "..." to ".." in cols
  newcols <- gsub("\\.\\.\\.","\\.\\.",cols)

  # setup units
  units <- matrix(data=NA,nrow=length(cols))

  # split the col names into units
  for (i in 4:length(cols)) {
    mysplit <- unlist(strsplit(newcols[i],"\\.\\."))

    if (length(mysplit) == 2) {
      units[i] = mysplit[2]
      newcols[i] = mysplit[1]
    }
    #       else if (length(mysplit) == 3) {
    #       units[i] = mysplit[3]
    #       newcols[i] = sprintf("%s_obs",mysplit[1])
    #     }
  }

  # add the date time object, replace time date hour bits
  watersheds <- watersheds[,4:ncol(watersheds)]
  newcols <- newcols[4:length(newcols)]
  units <- units[4:nrow(units)]
  watersheds <- xts(order.by=date.time,x=watersheds)

  # assign new column names
  colnames(watersheds) <- newcols

  # manual correct for units
  # remove trailing "." in unit labels
  #   for (i in 1:length(units)) {
  #     if (substr(units[i], nchar(units[i]), nchar(units[i])) == ".") {
  #       units[i] = substr(units[i],1,nchar(units[i])-1)
  #     }
  #   }
  # temporary correction
  units <- replace(units,which(units == "m3.s."),"m3/s")
  units <- replace(units,which(units == "mm.day."),"mm/day")

  return(list("watershed.storage" = watersheds, "units" = units))
}

