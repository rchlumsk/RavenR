#' @title Read in Raven Exhaustive Mass Balance file
#'
#' @description
#' rvn_exhaustive_mb_read is used to read in the ExhaustiveMassBalance.csv file produced by
#' the modelling Framework Raven.
#'
#' @details
#' This function expects a full file path to the ExhaustiveMassBalance.csv file,
#' then reads in the file using read.csv. The main advantage of this functon is
#' renaming the columns to nicer names and extracting the units into something
#' much easier to read.
#'
#' ff is the full file path of the ExhaustiveMassBalance.csv file. If the file is
#' located in the current working directory, then simply the name of the file
#' is sufficient.
#'
#' tzone is a string indicating the timezone of the supplied file. The
#' timezone provided is coded into the resulting data frame using the as.POSIXct
#' function. If no timezone is provided, this is left as an empty string, and is
#' determined by the function as the current time zone.
#'
#' @param ff full file path to the ExhaustiveMassBalance.csv file
#' @param join_categories boolean whether add to the category tag as a column
#' name prefix in exhaustivemb output (default TRUE)
#' @param tzone string indicating the timezone of the data in ff
#' @return \item{exhaustivemb}{data frame from the file with standardized
#' names} \item{units}{vector corresponding to units of each column}
#' \item{categories}{vector corresponding to the storage category of each column}
#'
#' @seealso \code{\link{rvn_hyd_read}} for reading in the Hydrographs.csv file, and
#' \code{\link{rvn_exhaustive_mb_read}} for reading in the
#' WatershedMassEnergyBalance.csv file
#'
#' @examples
#' # Read in exhaustive MB file, create plot
#' ff <- system.file("extdata","run1_ExhaustiveMassBalance.csv", package="RavenR")
#' embd <- rvn_exhaustive_mb_read(ff)
#'
#' # Preview data
#' head(embd$exhaustive_mb)
#'
#' # Plot data
#' plot(embd$exhaustive_mb$SURFACE_WATER.Infiltration,
#'      main="Cumulative Surface Water Infiltration")
#' @export rvn_exhaustive_mb_read
#' @importFrom xts xts
rvn_exhaustive_mb_read <- function(ff=NA, join_categories=TRUE, tzone=NULL)
{

  if (missing(ff)) {
    stop("Requires the full file path to the ExhaustiveMassBalance.csv file")
  }

  # test reading and get format, number of columns
  emb <- read.csv(ff,header=FALSE,nrows=5,stringsAsFactors = FALSE)
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
  emb <- read.csv(ff,header=TRUE,skip=1,colClasses = classes,na.strings=c("---",'NA','1.#INF'))

  if (is.null(tzone)) {
    # date.time <- as.POSIXct(paste(hydrographs$date,hydrographs$hour), format="%Y-%m-%d %H:%M:%S")
    date.time <- as.POSIXct(paste(emb[,2],emb[,3]), format="%Y-%m-%d %H:%M:%S")
  } else {
    date.time <- as.POSIXct(paste(emb[,2],emb[,3]), format="%Y-%m-%d %H:%M:%S", tz=tzone)
  }

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
  if (join_categories) {
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

  return(list("exhaustive_mb" = emb, "units" = units,"categories"=categories))
}

