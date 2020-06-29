#' Read in Raven Exhaustive Mass Balance file
#'
#' watershed.read is used to read in the ExhaustiveMassBalance.csv file produced by
#' the modelling Framework Raven.
#'
#' This function expects a full file path to the ExhaustiveMassBalance.csv file,
#' then reads in the file using read.csv. The main advantage of this functon is
#' renaming the columns to nicer names and extracting the units into something
#' much easier to read.
#'
#' ff is the full file path of the ExhaustiveMassBalance.csv file. If the file is
#' located in the current working directory, then simply the name of the file
#' is sufficient.
#'
#' @param ff full file path to the ExhaustiveMassBalance.csv file
#' @param join.categories boolean whether add to the category tag as a column
#' name prefix in exhaustivemb output (default TRUE)
#' @return \item{exhaustivemb}{data frame from the file with standardized
#' names} \item{units}{vector corresponding to units of each column}
#' \item{categories}{vector corresponding to the storage category of each column}
#' @seealso \code{\link{hyd.read}} for reading in the Hydrographs.csv file
#' \code{\link{watershedmeb.read}} for reading in the
#' WatershedMassEnergyBalance.csv file
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven read.csv exhaustive mass balance
#' @examples
#' read in file from package (RavenR/inst/extdata)
#' ff <- system.file("extdata","ExhaustiveMassBalance_1.csv", package="RavenR")
#'
#' # read in exhaustive MB file, create plot
#' exhaustiveMB.data <- RavenR::exhaustivemb.read(ff)
#' head(exhaustiveMB.data$exhaustivemb)
#'plot(exhaustiveMB.data$exhaustivemb$SURFACE_WATER.Infiltration,
#' ylab="Cumulative Surface Water Infiltration"
#' @export exhaustivemb.read
exhaustivemb.read <- function(ff=NA,join.categories=T) {

  if (missing(ff)) {
    stop("Requires the full file path to the ExhaustiveMassBalance.csv file")
  }

  # test reading and get format, number of columns
  emb <- read.csv(ff,header=F,nrows=5,stringsAsFactors = F)
  classes <- c(c('numeric','character','character'),rep('numeric',ncol(emb)-3))
  categories <- as.character(emb[1,4:ncol(emb)])
  ss1 <- categories[1]
  for (i in 2:length(categories)) {
    if (categories[i] == "") {
      categories[i] <- ss1
    } else {
      ss1 <- categories[i]
    }
  }

  # re-read with specified colClasses
  emb <- read.csv(ff,header=T,skip=1,colClasses = classes,na.strings=c("---",'NA','1.#INF'))

  # careful in date-time formats; excel can screw it up if csv is saved over. This works for
  # un untouched Raven output file
  # date.time <- as.POSIXct(paste(emb$date,emb$hour), format="%Y-%m-%d %H:%M:%S")
  date.time <- as.POSIXct(paste(emb[,2],emb[,3]), format="%Y-%m-%d %H:%M:%S") # assuming order of dt columns

  # head(date.time)
  cols <- colnames(emb)

  # temporary fix while precip column leaves no space between precip and units
  # if ("precip.mm.day." %in% cols) {
  #   cols <- replace(cols,which(cols == "precip.mm.day."),"precip..mm.day.")
  # }
  # if ("rainfall.mm.day." %in% cols) {
  #   cols <- replace(cols,which(cols == "rainfall.mm.day."),"rainfall..mm.day.")
  # }
  # if ("snowfall.mm.day." %in% cols) {
  #   cols <- replace(cols,which(cols == "snowfall.mm.day."),"snowfall..mm.day.")
  # }
  #   if ("rain.mm.day." %in% cols) {
  #   cols <- replace(cols,which(cols == "rain.mm.day."),"rain..mm.day.")
  # }
  # if ("snowfall.mm.day." %in% cols) {
  #   cols <- replace(cols,which(cols == "snow.mm.day."),"snow..mm.day.")
  # }

  # change all "..." to ".." in cols
  newcols <- gsub("\\.\\.\\.","\\.\\.",cols)


  ##### UNITS CURRENTLY DISABLED AS THE FILE HAS NO UNITS

  # setup units
  units <- matrix(data=NA,nrow=length(cols))

  # # split the col names into units
  # for (i in 4:length(cols)) {
  #   mysplit <- unlist(strsplit(newcols[i],"\\.\\."))
  #
  #   if (length(mysplit) == 2) {
  #     units[i] = mysplit[2]
  #     newcols[i] = mysplit[1]
  #   }
  # }

  # add the date time object, replace time date hour bits
  emb <- emb[,4:ncol(emb)]
  newcols <- newcols[4:length(newcols)]
  units <- units[4:nrow(units)]
  # categories <- categories[4:length(categories)] # already done above
  emb <- xts(order.by=date.time,x=emb)

  # join the categories as a prefix to column names
  if (join.categories) {
    newcols <- paste0(categories,rep(".",length(categories)),newcols)
  }

  # assign new column names
  colnames(emb) <- newcols

  # manual correct for units
  # remove trailing "." in unit labels
  #   for (i in 1:length(units)) {
  #     if (substr(units[i], nchar(units[i]), nchar(units[i])) == ".") {
  #       units[i] = substr(units[i],1,nchar(units[i])-1)
  #     }
  #   }

  # temporary correction
  # units <- replace(units,which(units == "m3.s."),"m3/s")
  # units <- replace(units,which(units == "mm.day."),"mm/day")

  return(list("exhaustivemb" = emb, "units" = units,"categories"=categories))
}

