#' Plot Continuous Data Using Subbasin Shapefile
#'
#' Plots Raven custom output into a subbasin map
#'
#' @param shpfilename filename of shapefile containing HRU polygons, with one column inidicating Raven HRU ID
#' @param subIDcol string of subbasin ID column in shapefile
#' @param plot.date string of date to plot in custom.data
#' @param cust.data custom data set as read in by custom.read, for daily by_subbasin data
#' @param leg.title text for legend title
#' @param leg.pos position of legend
#' @param normalize.data whether to normalize data by all cust.data (TRUE) or just the data for the given date (FALSE)
#' @param colour1 string indicating which colour (text or code) to use on lower bound of range
#' @param colour2 string indicating which colour (text or code) to use on upper bound of range
#' @param num.classes number of classes to use in legend. Does not change the actual display colours
#' @param invalid.stop whether to stop if invalid basins are found (TRUE) or just continue with a warning (FALSE)
#' @param basins.label label to put on basins, one of c('None,'subID','value') to show nothing, subbasinIDs, or actual plotted values
#' @param plot.title title across top of plot
#' @param plot.invalid boolean indicating whether to plot invalid basins in grey (currently disabled)
#'
#' @details Does not currently support discrete data such as land use. Ability to include invalid basins in grey is currently disabled.
#'
#' @author James R. Craig, University of Waterloo
#' @author Robert Chlumsky
#'
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#'
#' @importFrom sf read_sf st_centroid st_coordinates
#'
#' @seealso \code{\link{subbasinNetwork.plot}} to create network plots
#'
#' See also the \href{http://raven.uwaterloo.ca/}{Raven web site}
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords subbasin map plot
#' @examples
#'
#' # Raw sample data
#' shpfilename <- system.file("extdata","Nith_shapefile_sample.shp",package="RavenR")
#'
#' # Custom Output data from Raven for Nith basin
#' cust.data <- custom.read(system.file("extdata","run1_PRECIP_Daily_Average_BySubbasin.csv",
#'                                   package="RavenR"))
#'
#' subIDcol <- 'subID' # attriute in shapefile with subbasin IDs
#' plot.date <- "2003-03-30" # date for which to plot custom data
#'
#' # function call
#' SBMap.plot(shpfilename,subIDcol,plot.date,cust.data)
#'
#' @export SBMap.plot
SBMap.plot <- function(shpfilename,subIDcol,plot.date,cust.data,leg.title='Legend',leg.pos='bottomleft',
                       normalize.data=FALSE,colour1="green",colour2="blue",
                       num.classes=5,invalid.stop=TRUE,basins.label='subID',plot.title='', plot.invalid=F)
{

  basinshp <- sf::read_sf(shpfilename)

  if ( !(subIDcol %in% colnames(basinshp)) ) {
    stop(sprintf("Provided argument subIDcol value '%s' must be a column attribute in shapefile.",subIDcol))
  }

  shp.subs <- basinshp[[subIDcol]] # all SBIDs in the GIS File

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
  subs <- as.character(sort(shp.subs[(shp.subs %in% data.subs)]))

  if (length(subs) == 0) {
    stop("No valid subbasins found. Check shapefile and custom data supplied to function.")
  }

  # proceed with plotting

  # shapefile with valid basins
  validbasins <- basinshp[ (basinshp[[subIDcol]] %in% subs),]

  # shapefile with invalid basins
  invalidbasins<-basinshp[!(basinshp[[subIDcol]] %in% subs),]

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

  # check validity of colour inputs
  if (!(iscolour(colour1))) {
    warning(sprintf("Specified colour1 %s is not a valid colour, defaulting to green",colour1))
    colour1 <- "green"
  }
  if (!(iscolour(colour2))) {
    warning(sprintf("Specified colour2 %s is not a valid colour, defaulting to blue",colour2))
    colour2 <- "blue"
  }
  # create colour scheme
  hc <- rgb(colorRamp(c(colour1,colour2))(drange)/255)
  legcolor<-rgb(colorRamp(c(colour1, colour2))(cc)/255)

  # create plot
  plot(validbasins$geometry,col=hc)

  # add title
  title(plot.title)

  # get the polygon coordinates, suppress the current warning message with this function
  suppressWarnings(crds <- st_coordinates(st_centroid(validbasins)))

  # add subbasin labels
  if (basins.label == 'None') {
    # no labels
  } else if (basins.label == 'subID') {
    invisible(text(crds, labels=as.character(subs), cex=0.5))
  } else if (basins.label == 'value') {
    invisible(text(crds,labels=as.character(round(dd,1)), cex=0.5))
  }

  # add date to plot
  mtext(as.character(plot.date),side=1)

  # Create Legend
  nos<-as.character(round(cc*(dmax-dmin)+dmin,1))
  legend(x=leg.pos,y=leg.pos, legend=nos, legcolor, cex=0.8, bty="n",title=leg.title,
         border='black',title.col = 'black')

  # Plot invalid basins as gray
  # plot(invalidbasins,add=TRUE,col=rep("gray40",nrow(invalidbasins))) # disabled feature, need to get extents of
  # valid and invalid basins prior to including this feature
  return(TRUE)
}
