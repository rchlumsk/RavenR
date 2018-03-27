#' Read in Raven ReservoirStages file
#'
#' res.read is used to read in the ReservoirStages.csv file produced by the
#' modelling Framework Raven.
#'
#' This function expects a full file path to the ReservoirStages.csv file, then
#' reads in the file using read.csv. The main advantage of this function is
#' renaming the columns to nicer names and extracting the units into something
#' much easier to read.
#'
#' This function is also built to support the res.extract function, which uses
#' the object created here for extracting by reference to the columns named
#' here, for example sub24.
#'
#' ff is the full file path of the ReservoirStages.csv file. If the file is
#' located in the current working directory, then simply the name of the file
#' is sufficient.
#'
#' @param ff full file path to the ReservoirStages.csv file
#' @return \item{res}{data frame from the file with standardized names}
#' @seealso \code{\link{res.extract}} for extraction tools related to the
#' res.read output file
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven read.csv reservoir
#' @examples
#' # warning: example not run, sample example for associated files only
#' \dontrun{
#' # create full file path
#' dir <- "C:/temp/model/output.csv")
#' ff <- paste0(dir,"/","run4_ReservoirStages.csv")
#'
#' # read in the Reservoir file
#' myres <- res.read(ff)
#'
#' # view contents
#' head(myres$res)
#' res$units
#' }
#'
#' @export res.read
res.read <- function(ff=NA) {

  if (missing(ff)) {
    stop("Requires the full file path to the ReservoirStages.csv file.")
  }

  #read reservoir output
  reservoirs <- read.csv(ff,header=T,nrows=5)
  # careful in date-time formats; excel can screw it up if csv is saved over. This works for
  # an untouched Raven output file

  # assumed colClasses structure - mostly numeric except date and hour columns
  classes <- c(c('numeric','character','character'),rep('numeric',ncol(reservoirs)-3))

  # re-read with specified colClasses
  reservoirs <- read.csv(ff,header=T,colClasses = classes,na.strings=c("---",'NA'))


  # need to fix the hourly model
  date.time <- as.POSIXct(paste(reservoirs$date,reservoirs$hour), format="%Y-%m-%d %H:%M:%S")
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
      obs_flag[i] = F
      newcols[i] = mysplit[1]
    } else if (length(mysplit) == 3) {
      if (mysplit[2] == "observed") {
        units[i] = mysplit[3]
        obs_flag[i] = T
        newcols[i] = sprintf("%s_obs",mysplit[1])
      } else if (mysplit[2] == "res.inflow") {
        units[i] = mysplit[3]
        obs_flag[i] = F
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
