#' @title Read in Raven ReservoirStages file
#'
#' @description
#' rvn_res_read is used to read in the ReservoirStages.csv file produced by the
#' modelling Framework Raven.
#'
#' @details
#' This function expects a full file path to the ReservoirStages.csv file, then
#' reads in the file using read.csv. The main advantage of this function is
#' renaming the columns to nicer names and extracting the units into something
#' much easier to read.
#'
#' This function is also built to support the rvn_res_extract function, which uses
#' the object created here for extracting by reference to the columns named
#' here, for example sub24.
#'
#' ff is the full file path of the ReservoirStages.csv file. If the file is
#' located in the current working directory, then simply the name of the file
#' is sufficient.
#'
#' @param ff full file path to the ReservoirStages.csv file
#' @param tzone string indicating the timezone of the data in ff
#' @return \item{res}{data frame from the file with standardized names}
#' @seealso \code{\link{rvn_res_extract}} for extraction tools related to the
#' rvn_res_read output file
#'
#' @examples
#' # create full file path
#' ff <- system.file("extdata","ReservoirStages.csv", package="RavenR")
#'
#' # read in the Reservoir file
#' myres <- rvn_res_read(ff)
#'
#' # view contents
#' head(myres$res)
#' myres$units
#'
#' @export rvn_res_read
#' @importFrom xts xts
#' @importFrom utils read.csv
rvn_res_read <- function(ff=NA, tzone=NULL)
{

  if (missing(ff)) {
    stop("Requires the full file path to the ReservoirStages.csv file.")
  }

  #read reservoir output
  reservoirs <- read.csv(ff,header=TRUE,nrows=5)
  # careful in date-time formats; excel can screw it up if csv is saved over. This works for
  # an untouched Raven output file

  # assumed colClasses structure - mostly numeric except date and hour columns
  classes <- c(c('numeric','character','character'),rep('numeric',ncol(reservoirs)-3))

  # re-read with specified colClasses
  reservoirs <- read.csv(ff,header=TRUE,colClasses = classes,na.strings=c("---",'NA'))

  # need to fix the hourly model
  if (is.null(tzone)) {
    date.time <- as.POSIXct(paste(reservoirs$date,reservoirs$hour), format="%Y-%m-%d %H:%M:%S")
  } else {
    date.time <- as.POSIXct(paste(reservoirs$date,reservoirs$hour), format="%Y-%m-%d %H:%M:%S", tz=tzone)
  }
  # head(date.time)
  cols <- colnames(reservoirs)

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
    } else if (length(mysplit) == 3) {
      if (mysplit[2] == "observed") {
        units[i] = mysplit[3]
        obs_flag[i] = TRUE
        newcols[i] = sprintf("%s_obs",mysplit[1])
      } else if (mysplit[2] == "res.inflow") {
        units[i] = mysplit[3]
        obs_flag[i] = FALSE
        newcols[i] = sprintf("%s_inflow",mysplit[1])
      }
    }
  }

  # add the date time object, replace time date hour bits
  reservoirs <- reservoirs[,pcol:Ncol]
  newcols <- newcols[pcol:Ncol]
  units <- units[pcol:Ncol]
  obs_flag <- obs_flag[pcol:Ncol]
  reservoirs <- xts(order.by=date.time,x=reservoirs)

  # assign new column names
  colnames(reservoirs) <- newcols

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

  return(list("res" = reservoirs, "units" = units, "obs.flag" = obs_flag))
}
