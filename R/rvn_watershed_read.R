#' @title Read in Raven WatershedStorage file
#'
#' @description
#' rvn_watershed_read is used to read in the WatershedStorage.csv file produced by
#' Raven.
#'
#' @details
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
#' @param tzone string indicating the timezone of the data in ff
#' @return \item{watershed_storage}{data frame from the file with standardized
#' names} \item{units}{vector corresponding to units of each column}
#' @seealso \code{\link{rvn_hyd_read}} for reading in the Hydrographs.csv file
#' \code{\link{rvn_watershedmeb_read}} for reading in the
#' WatershedMassEnergyBalance.csv file
#'
#' @examples
#'
#' # locate in RavenR Watershed storage file
#' ff <- system.file("extdata","run1_WatershedStorage.csv", package="RavenR")
#'
#' # create full file path and read in file
#' mywshd <- rvn_watershed_read(ff)
#'
#' # check data
#' head(mywshd$watershed_storage)
#'
#' @export rvn_watershed_read
#' @importFrom utils read.csv
rvn_watershed_read <- function(ff=NA, tzone=NULL) {

  if (missing(ff)) {
    stop("Requires the full file path to the WatershedStorage.csv file")
  }

  # test reading and get format, number of columns
  watersheds <- read.csv(ff,header=TRUE,nrows=5)
  classes <- c(c('numeric','character','character'),rep('numeric',ncol(watersheds)-3))

  # re-read with specified colClasses
  watersheds <- read.csv(ff,header=TRUE,colClasses = classes,na.strings=c("---",'NA','1.#INF'))

  if (is.null(tzone)) {
    date.time <- as.POSIXct(paste(watersheds$date,watersheds$hour), format="%Y-%m-%d %H:%M:%S")
  } else {
    date.time <- as.POSIXct(paste(watersheds$date,watersheds$hour), format="%Y-%m-%d %H:%M:%S", tz=tzone)
  }

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

  return(list("watershed_storage" = watersheds, "units" = units))
}

