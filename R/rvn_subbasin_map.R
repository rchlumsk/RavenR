#' @title Plot Continuous Data Using Subbasin Shapefile
#'
#' @description
#' Plots Raven custom output into a subbasin map
#'
#' @param shpfilename filename of shapefile containing HRU polygons, with one column inidicating Raven HRU ID
#' @param subIDcol string of subbasin ID column in shapefile
#' @param plot_date string of date to plot in custom.data
#' @param cust_data custom data set as read in by custom.read, for daily by_subbasin data
#' @param normalize_data whether to normalize data by all cust_data (TRUE) or just the data for the given date (FALSE)
#' @param invalid_stop whether to stop if invalid basins are found (TRUE) or just continue with a warning (FALSE)
#' @param basins_label label to put on basins, one of c('None,'subID','value') to show nothing, subbasinIDs, or actual plotted values
#' @param plot_invalid boolean indicating whether to plot invalid basins in grey (currently disabled)
#'
#' @details Does not currently support discrete data such as land use. Ability to include invalid basins in grey is currently disabled.
#'
#' @author James R. Craig, University of Waterloo
#' @author Robert Chlumsky
#' @author Genevieve Brown
#'
#' @return \item{p1}{ggplot object of subbasin map}
#'
#' @seealso \code{\link{rvn_subbasin_network_plot}} to create network plots
#'
#' @examples
#'
#' # Raw shapefile sample data
#' shpfilename <- system.file("extdata","Nith_shapefile_sample.shp",package="RavenR")
#'
#' # Custom Output data from Raven for Nith basin
#' cust_file <- system.file("extdata","run1_PRECIP_Daily_Average_BySubbasin.csv",
#'                          package="RavenR")
#' cust_data <- rvn_custom_read(cust_file)
#'
#' subIDcol <- 'subID'        # attribute in shapefile with subbasin IDs
#' plot_date <- "2003-03-30"  # date for which to plot custom data
#'
#' # Generate plot object
#' p1 <- rvn_subbasin_map(shpfilename,subIDcol,plot_date,cust_data, normalize=TRUE)
#' p1
#'
#'
#' @export rvn_subbasin_map
#' @importFrom sf read_sf st_centroid st_coordinates
#' @importFrom ggplot2 ggplot aes geom_sf theme geom_text
rvn_subbasin_map <- function(shpfilename, subIDcol, plot_date, cust_data=NULL, normalize_data=FALSE,
                       invalid_stop=TRUE, basins_label='subID', plot_invalid=FALSE)
{

  X <- Y <- Value <- text <- NULL

  basinshp <- read_sf(shpfilename)

  if ( !(subIDcol %in% colnames(basinshp)) ) {
    stop(sprintf("Provided argument subIDcol value '%s' must be a column attribute in shapefile.",subIDcol))
  }

  shp.subs <- basinshp[[subIDcol]] # all SBIDs in the GIS File

  if (is.null(shp.subs)){
    print("Invalid Subbasin ID column identifier")
  }

  if (is.null(cust_data)) {
    stop("cust_data must be provided")
  }

  data.subs <- as.numeric(colnames(cust_data))

  # check if any invalid data columns in data
  if (any(is.na(data.subs))) {
    stop("Invalid data columns found in custom data. Ensure all columns names are subbasin IDs, and data was generated with RavenR::custom.read from a subbasin custom data file.")
  }

  # check if any valid subbasins in data
  if (any(data.subs[!(data.subs %in% shp.subs)]) | any(shp.subs[!(shp.subs %in% data.subs)])) {
    if (invalid_stop) {
      stop("One or more subbasins does not correspond between shapefile subbasin IDs and data subbasin IDs.\nCheck the supplied data or provide the invalid_stop argument as FALSE to continue")
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
  dd <- cust_data[plot_date,subs]

  if (normalize_data) {
    dmin <- min(cust_data[,subs])
    dmax <- max(cust_data[,subs])
    drange <- ((dd-dmin)/(dmax-dmin))
    drange <- t(as.data.frame(drange))
    validbasins$Value <- drange
  } else {
    dd <- t(as.data.frame(dd))
    validbasins$Value <- dd
  }

  #create plot
  p1 <- ggplot()+
    geom_sf(data=validbasins, aes(fill=Value))+
    rvn_theme_RavenR()+
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())

  # get the polygon coordinates, suppress the current warning message with this function
  suppressWarnings(crds <- st_coordinates(st_centroid(validbasins)))

  # add subbasin labels
  if (basins_label == 'None') {
    # no labels
  } else if (basins_label == 'subID') {
    sub_labels <- as.data.frame(crds)
    sub_labels$text <- as.character(subs)

    p1 <- p1 +
      geom_text(data = sub_labels, aes(X,Y,label=text), size=0.5)

  } else if (basins_label == 'value') {
    sub_labels <- as.data.frame(crds)
    sub_labels$text <- as.character(round(dd,1))

    p1 <- p1 +
      geom_text(data = sub_labels, aes(X,Y,label=text), size=0.5)
  }

  # add date to plot
  p1 <- p1 +
    xlab(plot_date)+
    ylab("")

  p1 +
    geom_sf(data = invalidbasins, fill = "grey")

  #plot invalid basins as gray
  invalidbasins$Value <- NA

  p1 +
    geom_sf(data = invalidbasins, fill = "grey")
  ## check why the invalidbasins are added a second time wiht line 145, should be able to get effects in 1 line?

  return(p1)
}
