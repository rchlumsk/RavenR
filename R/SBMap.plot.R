#' Create Subbasin Map
#'
#' # description to be added
#'
#' Creates single static subbasin plot from custom data
#'
#' see informal parameter description here
#'
#'shpfilename - file path to shapefile.shp file
# subIDcol - string of subbasin ID column in shapefile
# plot.date - string of date to plot in custom.data
# cust.data - custom data set as read in by custom.read, for daily by_subbasin data
# let.title - text for legend title
# leg.pos - position of legend
# normalize.data - whether to normalize data by all cust.data (TRUE) or just the data for the given date (FALSE)
# colour.scheme - colour scheme to use. Currently just 'White-Blue' or 'Blue-White'. Will be easy to add more later
# num.classes - number of classes to use in legend. Does not change the actual display colours
# invalid.stop - whether to stop if invalid basins are found (TRUE) or just continue with a warning (FALSE)
# basins.label - label to put on basins, one of c('None,'subID','value') to show nothing, subbasinIDs, or actual plotted values
# plot.title - title across top of plot
#'
#' @param TBD params to be added
#'
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#'
#' @seealso \code{\link{subbasinNetwork.plot}} to create network plots
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords subbasin map plot
#' @examples
#'
#' # Warning: example not run
#' # Example to be cleaned up
#'
#' \dontrun{
#' shpfilename <- 'shp/subbasins.shp'
#' subIDcol <- 'subID'
#' plot.date <- "2010-01-02"
#' cust.data <- custom.read('PRECIP_Daily_Average_BySubbasin.csv')
#' leg.title <- 'Legend - Precip (mm)'
#' leg.pos <- 'topright'
#' normalize.data <- TRUE
#' colour.scheme <- 'White-Blue'
#' num.classes <- 5
#' invalid.stop <- TRUE
#' basins.label <- 'subID'
#' # 'None', 'subID', 'value'
#' plot.title <- 'Precipitation (mm/d)'
#'
#'
#' # create singe plot
#' SBMap.plot(shpfilename,subIDcol,plot.date,cust.data,plot.title=plot.title)
#'
#' cust.data <- custom.read('SNOW_Daily_Average_BySubbasin.csv')
#' plot.title <- 'Snow (mm)'
#'
#' # create singe plot
#' SBMap.plot(shpfilename,subIDcol,plot.date,cust.data,plot.title=plot.title)
#' }
#'
#' @export SBMap.plot
SBMap.plot <- function(shpfilename,subIDcol,plot.date,cust.data,leg.title='Legend',leg.pos='bottomleft',
                       normalize.data=TRUE,colour.scheme='White-Blue',
                       num.classes=5,invalid.stop=TRUE,basins.label='subID',plot.title='')
{
  basinshp<-readShapeSpatial(shpfilename)
  shp.subs<-basinshp@data[[subIDcol]] # all SBIDs in the GIS File
  if (is.null(shp.subs)){
    print("Invalid Subbasin ID column identifier")
  }
  data.subs <- as.numeric(colnames(cust.data))

  # check if any invalid data columns in data
  if (any(is.na(data.subs))) {
    stop("Invalid data columns found in custom data. Ensure all columns names are subbasin IDs, and data was generated with RavenR::custom.read from a subbasin custom data file.")
  }

  # check if any valid subbasins in data
  if (any(data.subs[!(data.subs %in% shp.subs)]) | any(shp.subs[!(shp.subs %in% data.subs)])) {
    if (invalid.stop) {
      stop("One or more subbasins does not correspond between shapefile subbasin IDs and data subbasin IDs.\nCheck the supplied data or provide the invalid.stop argument as FALSE to continue")
    }
    warning("One or more subbasins does not correspond between shapefile subbasin IDs and data subbasin IDs.\nContinuing with available data.")
  }

  # get IDs of subbasin to plot
  subs <- sort(shp.subs[(shp.subs %in% data.subs)])

  if (length(subs) == 0) {
    stop("No valid subbasins found. Check shapefile and custom data supplied to function.")
  }

  # proceed with plotting

  # shapefile with valid basins
  validbasins <- basinshp[ (basinshp@data[[subIDcol]] %in% subs),]

  # shapefile with invalid basins
  invalidbasins<-basinshp[!(basinshp@data[[subIDcol]] %in% subs),]

  # get data to plot
  dd <- cust.data[plot.date,subs]

  if (normalize.data) {
    dmin <- min(cust.data[,subs])
    dmax <- max(cust.data[,subs])
  } else {
    dmin <- min(dd)
    dmax <- max(dd)
  }
  drange <- ((dd-dmin)/(dmax-dmin))

  # JRC todo: if data is string-based (e.g., landcover), should plot differently

  cc <- seq(0.0,num.classes,1)/num.classes
  if (colour.scheme == "White-Blue") {
    hc <- rgb(colorRamp(c("azure", "blue"))(drange)/255)
    legcolor<-rgb(colorRamp(c("azure", "blue"))(cc)/255)
  } else if (colour.scheme == "Blue-White") {
    hc <- rgb(colorRamp(c("blue","azure"))(drange)/255)
    legcolor<-rgb(colorRamp(c("blue", "azure"))(cc)/255)
  } else {
    stop("Other colour schemes coming soon")
  }

  # create plot
  plot(validbasins,col=hc)

  # add title
  title(plot.title)

  # add subbasin labels
  if (basins.label == 'None') {
    # no labels
  } else if (basins.label == 'subID') {
    invisible(text(getSpPPolygonsLabptSlots(validbasins),labels=as.character(subs), cex=0.5))
  } else if (basins.label == 'value') {
    invisible(text(getSpPPolygonsLabptSlots(validbasins),labels=as.character(round(dd,1)), cex=0.5))
  }

  # add date to plot
  mtext(as.character(plot.date),side=1)

  # Create Legend
  nos<-as.character(round(cc*(dmax-dmin)+dmin,1))
  legend(x=leg.pos,y=leg.pos, legend=nos, legcolor, cex=0.8, bty="n",title=leg.title,
         border='black',title.col = 'black')

  # Plot invalid basins as gray
  plot(invalidbasins,add=TRUE,col=rep("gray40",length(invalidbasins@data)))
  return(TRUE)
}
