#' @title Read in Raven WatershedMassEnergyBalance file
#'
#' @description
#' rvn_watershedmeb_read is used to read in the WatershedMassEnergyBalance.csv file
#' produced by the modelling Framework Raven.
#'
#' @details
#' This function expects a full file path to the WatershedMassEnergyBalance.csv
#' file, then reads in the file using read.csv. The main advantage of this
#' functon is renaming the columns to nicer names and extracting the units into
#' something much easier to read. The from and to rows are also properly
#' handled, which is not as straightforward as some of the other Raven files.
#'
#' ff is the full file path of the WatershedMassEnergyBalance.csv file. If the
#' file is located in the current working directory, then simply the name of
#' the file is sufficient.
#'
#' @param ff full file path to the WatershedMassEnergyBalance.csv file
#' @param tzone string indicating the timezone of the data in ff
#' @return
#'  \item{watershedmeb}{data frame from the file with standardized names}
#'  \item{units}{vector corresponding to units of each column}
#'  \item{from}{vector of the 'from' compartments in file}
#'  \item{to}{vector of the 'to' compartments in file}
#'
#' @seealso \code{\link{rvn_watershed_read}} for reading in the
#' WatershedStorage.csv file
#'
#' @examples
#' # locate RavenR Watershed Mass Energy Balance storage file
#' ff <- system.file("extdata","run1_WatershedMassEnergyBalance.csv", package="RavenR")
#'
#' # read in file
#' mywshdmeb <- rvn_watershedmeb_read(ff)
#'
#' # view mass energy balance time series
#' head(mywshdmeb$watershedmeb)
#'
#' # view 'from' dataframe
#' mywshdmeb$from
#'
#' @export rvn_watershedmeb_read
#' @importFrom xts xts
#' @importFrom utils read.csv
rvn_watershedmeb_read <- function(ff=NA, tzone=NULL)
{
  if (missing(ff)) {
    stop("Requires the full file path to the WatershedMassEnergyBalance.csv file")
  }

  # test reading and get format, number of columns
  watersheds <- read.csv(ff,header=TRUE,nrows=5)
  classes <- c(c('numeric','character','character'),rep('numeric',ncol(watersheds)-3))
  cols <- colnames(watersheds)

  # read in and store the to and from rows
  from_row <- read.csv(ff,header=FALSE,nrows=1,skip=1)[4:length(cols)]
  to_row   <- read.csv(ff,header=FALSE,nrows=1,skip=2)[4:length(cols)]
  colnames(from_row) <- cols[4:length(cols)]
  colnames(to_row) <- cols[4:length(cols)]

  # re-read with specified colClasses
  watersheds <- read.csv(ff,header=FALSE,skip=3,colClasses = classes,na.strings=c("---",'NA','1.#INF'))
  colnames(watersheds) <- cols # assigning headers back

  if (is.null(tzone)) {
    date.time <- as.POSIXct(paste(watersheds$date,watersheds$hour), format="%Y-%m-%d %H:%M:%S")
  } else {
    date.time <- as.POSIXct(paste(watersheds$date,watersheds$hour), format="%Y-%m-%d %H:%M:%S", tz=tzone)
  }
  # head(date.time)
  # cols <- colnames(watersheds)

  # # temporary fix while precip column leaves no space between precip and units
  # if ("precip.mm.day." %in% cols) {
  #   cols <- replace(cols,which(cols == "precip.mm.day."),"precip..mm.day.")
  # }
  # if ("rainfall.mm.day." %in% cols) {
  #   cols <- replace(cols,which(cols == "rainfall.mm.day."),"rainfall..mm.day.")
  # }
  # if ("snowfall.mm.day." %in% cols) {
  #   cols <- replace(cols,which(cols == "snowfall.mm.day."),"snowfall..mm.day.")
  # }

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

  return(list("watershedmeb" = watersheds, "units" = units,"from"=from_row,"to"=to_row))
}
