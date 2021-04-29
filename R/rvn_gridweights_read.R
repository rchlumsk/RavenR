#' @title Read in Raven GridWeights file
#'
#' @description
#' rvn_gridweights_read is used to read in a Raven grid weights file.
#'
#' @details
#' This function expects a full file path to the grid weights file, then
#' reads in the file. The output is provided as a list containing the number of HRUs,
#' number of grid cells, and the grid weights data frame.
#'
#' @param ff full file path to the grid weights file
#' @return \item{gridweights}{list with the number of HRUs, number of grid cells, and gridweights data frame}
#' @seealso \code{\link{rvn_gen_gridweights}} for generating a grid weights file from an HRU shapefile
#' and netcdf file.
#'
#' @examples
#'
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
#' @export rvn_gridweights_read
#' @importFrom xts xts
#' @importFrom utils read.csv
rvn_gridweights_read <- function(ff=NA) {

  if (missing(ff)) {
    stop("Requires the full file path to the grid weights .rvt file.")
  }

  # #read hydrograph output
  # hydrographs <- read.csv(ff,header=TRUE,nrows=5)
  #
  # # assumed colClasses structure - mostly numeric except date and hour columns
  # classes <- c(c('numeric','character','character'),rep('numeric',ncol(hydrographs)-3))
  #
  # # re-read with specified colClasses
  # hydrographs <- read.csv(ff,header=TRUE,colClasses = classes,na.strings=c("---",'NA'))
  #
  # # need to fix the hourly model
  # if (is.null(tzone)) {
  #   date.time <- as.POSIXct(paste(hydrographs$date,hydrographs$hour), format="%Y-%m-%d %H:%M:%S")
  # } else {
  #   date.time <- as.POSIXct(paste(hydrographs$date,hydrographs$hour), format="%Y-%m-%d %H:%M:%S",tz=tzone)
  # }
  #
  # if (length(which(is.na(date.time)))>0){
  #   print("rvn_hyd_read: Error in mapping day/time to POSIXct. Must be timezone without daylight savings")
  #   return(FALSE)
  # }
  # cols <- colnames(hydrographs)
  #
  # # temporary fix while precip column leaves no space between precip and units
  # if ("precip.mm.day." %in% cols) {
  #   cols <- replace(cols,which(cols == "precip.mm.day."),"precip..mm.day.")
  # }
  #
  # # change all "..." to ".." in cols
  # newcols <- gsub("\\.\\.\\.","\\.\\.",cols)
  #
  # # setup units and obs_flag
  # units <- matrix(data=NA,nrow=length(cols))
  # obs_flag <- matrix(data=NA,nrow=length(cols))
  #
  # # find index where precip column starts
  # pcol <- grep("precip*",cols)
  # Ncol <- length(cols)
  #
  # # split the col names into units, observed flags
  # for (i in pcol:Ncol) {
  #   mysplit <- unlist(strsplit(newcols[i],"\\.\\."))
  #
  #   if (length(mysplit) == 2) {
  #     units[i] = mysplit[2]
  #     obs_flag[i] = FALSE
  #     newcols[i] = mysplit[1]
  #   } else if (length(mysplit) >= 3) {
  #     if (mysplit[2] == "observed") {
  #       units[i] = mysplit[3]
  #       obs_flag[i] = TRUE
  #       newcols[i] = sprintf("%s_obs",mysplit[1])
  #     } else if (mysplit[2] == "inflow") {
  #       units[i] = mysplit[3]
  #       obs_flag[i] = FALSE
  #       newcols[i] = sprintf("%s_resinflow",mysplit[1])
  #     } else if (mysplit[3] == "inflow") {
  #       units[i] = mysplit[4]
  #       obs_flag[i] = FALSE
  #       newcols[i] = sprintf("%s_resinflow",mysplit[1])
  #     }
  #   }
  # }
  #
  # # add the date time object, replace time date hour bits
  # hydrographs <- hydrographs[,pcol:Ncol]
  # newcols <- newcols[pcol:Ncol]
  # units <- units[pcol:Ncol]
  # obs_flag <- obs_flag[pcol:Ncol]
  # hydrographs <- xts(order.by=date.time,x=hydrographs)
  #
  # # assign new column names
  # colnames(hydrographs) <- newcols
  #
  # # manual correct for units
  # # remove trailing "." in unit labels
  # # for (i in 1:length(units)) {
  # #   if (substr(units[i], nchar(units[i]), nchar(units[i])) == ".") {
  # #     units[i] = substr(units[i],1,nchar(units[i])-1)
  # #   }
  # # }
  #
  # # temporary correction
  # units <- replace(units,which(units == "m3.s."),"m3/s")
  # units <- replace(units,which(units == "mm.day."),"mm/day")

  list("NumberHRUs"=length(ValidHRUIDs),
       "NumberGridCells"=length(GRDshp),
       "GridWeights"=dfgrid) %>%
    return(.)
}
