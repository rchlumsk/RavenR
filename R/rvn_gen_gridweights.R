#' @title Generate Raven grid weights from shapefile
#'
#' @description
#' Generates a Raven grid weights file given an HRU shapefile and a grid shapefile.
#'
#' @details
#' Generate grid weights file GaugeWeights.rvt given an HRU shapefile with HRU ID column
#' HRUIDcol (default 'HRU_ID') and a grid shapefile with ID column gridIDcol (default: 'cellID')
#' weights are determined by the areal overlap of grid cell g and HRU k, i.e.,
#'  wt[k][g]=area[k][g]/area[k]
#' where wt[k][g] is the weight of cell g in HRU k, area[k] is the total area of HRU k,
#' and area[k][g] is the area of HRU k that is within cell g.
#'
#' By definition, the grid domain has to completely cover the HRU domain such that the sum of
#' wt[k][g] for any k, over all g, is 1.0. However, the script currently normalizes weights such that
#' all weights will sum to 1 for all HRUs where any portion of the HRU is overlapping with one or more
#' grid cells.
#'
#' @details
#' Not a tonne of QA/QC is currently included - can fail due to bad netCDF file or inappropriate UTM zone;
#' uses rgdal and rgeos libraries, and the accuracy of the gIntersection() routine
#' leaves something to be desired. The shapefiles should be in the same projection, which is
#' checked for in this function.
#'
#' Note that the grid weights file (e.g. GridWeights.txt) is only written if outfile is not NULL. If
#' this is left as NULL, just the gridweights list object is returned. This object may be used to to
#' write the grid weights file using the \code{\link{rvn_gridweights_write}} function. If outfile is provided,
#' the \code{\link{rvn_gridweights_write}} function is called internally to write the gridweights file.
#'
#' @param HRUshpfile polygon shapefile (as sf or file path) of HRUs with data column containing HRU IDs (.shp extension expected)
#' @param Gridshpfile polygon shapefile (as sf or file path) of grid cells with data column containing cell IDs (.shp extension expected)
#' @param ValidHRUIDs a vector of valid HRU IDs in the model
#' @param HRUIDcol the name of the HRUshpfile polygon which contains the HRU IDs
#' @param gridIDcol the name of the Gridshpfile polygon which contains the cell IDs
#' @param outfile optional name of output Raven gaugeweights file
#'
#' @return \item{gridweights}{list with the number of HRUs, number of grid cells, and gridweights data frame.
#' Also writes the grid weights rvt file if outfile is supplied.}
#'
#' @author James R. Craig, University of Waterloo, 2019
#'
#' @seealso \code{\link{rvn_netcdf_to_gridshp}} for converting netcdf files to grid shapefile format
#'
#' @examples
#'
#' # load example rvh file
#' nith <- system.file("extdata",'Nith.rvh', package = "RavenR")
#' rvh <- rvn_rvh_read(nith)
#'
#' # adjust HRU shapefile to one per subbasin for demonstration
#' rvh$HRUtable <- rvh$HRUtable[c(1,6,15,25),]
#' rvh$HRUtable$Area <- rvh$SBtable$Area
#' rvh$HRUtable$ID <- rvh$HRUtable$SBID
#'
#' # define HRU shapefile path (use subbasin shapefile for example)
#' hrushpfile <- system.file("extdata","Nith_shapefile_sample.shp",package = "RavenR")
#'
#' # get grid shapefile from netcdf file
#' nithnc <- system.file("extdata/Nith_era5_sample.nc", package="RavenR")
#' gridshp <- rvn_netcdf_to_gridshp(ncfile=nithnc, projID=26917)
#'
#' # calculate gridweights
#' gw <- rvn_gen_gridweights(hrushpfile, gridshp,
#' gridIDcol = 'GridIDs', HRUIDcol = "subID")
#'
#'
#' @export rvn_gen_gridweights
#' @importFrom raster crs
#' @importFrom sf st_crs read_sf st_buffer st_intersection st_area
#' @importFrom dplyr group_by summarise
rvn_gen_gridweights <- function(HRUshpfile, Gridshpfile, ValidHRUIDs=NULL, HRUIDcol="HRU_ID",
                                gridIDcol="GridIDs", outfile=NULL)
{

  # read in HRU file
  #----------------------------------------------------------
  if ("sf" %in% class(HRUshpfile)) {
    HRUshp <- HRUshpfile

    # file path provided, read in shapefile
  } else if (class(HRUshpfile) == "character") {

    if (file.exists(HRUshpfile)) {
      HRUshp <- read_sf(HRUshpfile)
    } else {
      stop(sprintf("HRUshpfile '%s' not found.",HRUshpfile))
    }
  }

  # dsn=normalizePath(dirname(HRUshpfile))
  # lay=substr(basename(HRUshpfile),1,nchar(basename(HRUshpfile))-4)
  # HRUshp <- readOGR(dsn = dsn,layer = lay) # returns SpatialPolygonsDataFrame
  # HRUshp <- read_sf(HRUshpfile)

  if (!(HRUIDcol %in% colnames(HRUshp))){
    print(paste0("HRU ID column name ",HRUIDcol,"is not in shapefile ",HRUshpfile))
    return()
  }

  # check validHRUs input
  #----------------------------------------------------------
  if (is.null(ValidHRUIDs)) {
    ValidHRUIDs <- as.numeric(HRUshp[[HRUIDcol]])
  } else if (any(ValidHRUIDs %notin% HRUshp[[HRUIDcol]])) {
    badhruids <- ValidHRUIDs[which(ValidHRUIDs %notin% HRUshp[[HRUIDcol]])]
    stop(sprintf("%i ValidHRUIDs are not found in the HRU shapefile HRUIDcol attribute, including: %s",
                 length(badhruids), paste(  badhruids[c(1:min(5,length(badhruids)))]  )))
  }

  # read in Grid file
  #---------------------------------------------------------
  if ("sf" %in% class(Gridshpfile)) {
    GRDshp <- Gridshpfile

    # file path provided, read in shapefile
  } else if (class(Gridshpfile) == "character") {

    if (file.exists(HRUshpfile)) {
      GRDshp <- read_sf(Gridshpfile)
    } else {
      stop(sprintf("Gridshpfile '%s' not found.",Gridshpfile))
    }
  }

  # dsn=normalizePath(dirname(Gridshpfile))
  # lay=substr(basename(Gridshpfile),1,nchar(basename(Gridshpfile))-4)
  # GRDshp<-readOGR(dsn = dsn,layer = lay) # returns SpatialPolygonsDataFrame
  # GRDshp <- read_sf(Gridshpfile)

  if (!(gridIDcol %in% colnames(GRDshp))){
    print(paste0("Grid cell column name ",gridIDcol,"is not in shapefile ",Gridshpfile))
    return()
  }

  # check projections of both shapefiles
  #----------------------------------------------------------
  if (as.character(crs(GRDshp)) != as.character(crs(HRUshp))) {
    stop("The projections of the HRU and grid shapefiles need to be the same (checked with raster::crs() ), please reproject the shapefiles accordingly.")
  }

  # calculate areas of shapes (presumes non-geographic coordinates)
  #----------------------------------------------------------
  # HRUshp$HRUarea <-sapply(slot(HRUshp, "polygons"), slot, "area")
  # GRDshp$Gridarea<-sapply(slot(GRDshp, "polygons"), slot, "area")
  HRUshp$HRUarea <- as.numeric(st_area(HRUshp))
  # GRDshp$Gridarea <- st_area(GRDshp)

  # intersect HRUs and Grids after repairing HRU file structure
  #----------------------------------------------------------
  print("fixing")
  # HRUshp <- gBuffer(HRUshp, byid=TRUE, width=0) # returns SpatialPolygonsDataFrame
  # GRDshp <- gBuffer(GRDshp, byid=TRUE, width=0) # returns SpatialPolygonsDataFrame
  HRUshp <- st_buffer(HRUshp, dist=0)
  GRDshp <- st_buffer(GRDshp, dist=0)
  print("...done fixing")

  print("intersecting...")
  # isect<-gIntersection(HRUshp, GRDshp, byid=TRUE)
  isect <- sf::st_intersection(HRUshp, GRDshp)
  if (is.null(isect)){
    print("Cannot create gaugeweights file - HRU and grid shapefiles don't overlap ")
    return()
  }
  print("...done intersecting")

  # plots for debugging
  #----------------------------------------------------------
  #plot(HRUshp)
  #plot(GRDshp,add=TRUE)
  #plot(isect,add=TRUE,col=rep(c("red","blue","green","yellow","purple"),length(isect)))
  #invisible(text(getSpPPolygonsLabptSlots(GRDshp),labels=as.character(GRDshp$gridcode), cex=0.4)) # label with HRU ID
  #print("...done plotting")

  # calculate new area of shapes
  #----------------------------------------------------------
  # isect$iarea<-sapply(slot(isect, "polygons"), slot, "area")
  isect$iarea <- as.numeric(st_area(isect))

  # extract HRU and grid indices from gIntersection-generated 'ID' field
  #----------------------------------------------------------
  # isect$id   <-sapply(slot(isect, "polygons"), slot, "ID")
  # for (k in 1:nrow(isect)) {
  #   isect$hindex[k]<-as.numeric(strsplit(isect$id[k], " ")[[1]][1])+1
  #   isect$gindex[k]<-as.numeric(strsplit(isect$id[k], " ")[[1]][2])+1
  # }

  # compute Gridweights data frame
  #----------------------------------------------------------
  # wtsum <-rep(0.0,nrow(HRUshp))
  # dfgrid <- data.frame(matrix(NA,nrow=nrow(isect),ncol=3))
  # colnames(dfgrid) <- c("HRUID","GRIDID","WEIGHT")

  # dfgrid <- data.frame("HRUID"=NA, "GRIDID"=NA, "WEIGHT"=NA,
                       # nrow=0)

  ## simplifying script - just get all rows, calculate initial weights, aggregate and normalize afterwards


  dfgrid <- data.frame(isect[[HRUIDcol]],
                       isect[[gridIDcol]],
                       isect$iarea / isect$HRUarea)

  # dfgrid <- data.frame(matrix(NA,nrow=0,ncol=3))
  colnames(dfgrid) <- c("HRUID","GRIDID","WEIGHT")

  ## to do - add check for non-overlapping areas, option/flag on whteher to normalize


  # # get list of hrus in the intersection and cross-ref with ValidHRUIDs
  # hrus <- unique(isect[[HRUIDcol]])
  # hrus <- hrus[which(hrus %in% ValidHRUIDs)]

  # for each hrus, get all gridweights and bind with dfgrid

  # for (hru in hrus) {
  #   dfgrid_temp <- data.frame(matrix(NA,nrow=0,ncol=3))
  #   colnames(dfgrid_temp) <- c("HRUID","GRIDID","WEIGHT")
  #
  #   temp <- isect[isect[[HRUIDcol]] == hru,]
  #   temp$WEIGHT <- temp
  #
  #
  # }


  # for (i in 1:nrow(isect)) {
  #   wt<-0
  #   k<-isect$hindex[i] # HRU index
  #   g<-isect$gindex[i] # grid cell index
  #   if ((k>=1) & (k<=nrow(HRUshp))) {
  #     # for each HRU, wt[k][g]=area[k][g]/area[k]
  #
  #     if (HRUshp$HRUarea[k]>0){
  #       wt<-isect$iarea[i] / HRUshp$HRUarea[k]
  #     }
  #     if (wt>0){
  #       HRUID <- as.numeric(HRUshp[[HRUIDcol]][k])
  #       GridID <- as.numeric(GRDshp[[gridIDcol]][g])
  #       # if (HRUID %in% ValidHRUIDs){
  #         # write(paste("  ",HRUID," ",GridID ," ",wt),append=TRUE,file=outfile)
  #       dfgrid[i,] <- c(HRUID, GridID, wt)
  #       # }
  #       # wtsum[k]<-wtsum[k]+wt
  #     }
  #   }
  # }

  # merge/update/rescale Gridweights data frame
  #----------------------------------------------------------

  # subset only ValidHRUIDs
  dfgrid <- dfgrid[dfgrid$HRUID %in% ValidHRUIDs,]

  # # remove possible duplicates
  # dfgrid <- unique(dfgrid)

  # merge possible duplicate HRUID/GRIDID pairs, sum weights
  HRUID <- GRIDID <- WEIGHT <-  NULL  # declaration for CRAN passing
  dfgrid %>%
    group_by(HRUID, GRIDID) %>%
    summarise(WEIGHT = sum(WEIGHT), .groups = 'drop') %>%
    as.data.frame() -> dfgrid

  # rescale weights to ensure weights sum to 1
  for (hh in ValidHRUIDs) {
    dfgrid[dfgrid$HRUID == hh,]$WEIGHT <-
      dfgrid[dfgrid$HRUID == hh,]$WEIGHT / sum(dfgrid[dfgrid$HRUID == hh,]$WEIGHT)
    # if (sum(temp$WEIGHT) != 1) {print(sprintf("hh=%s not equal to 1", hh))}
  }

  gridweights <- list("NumberHRUs"=length(ValidHRUIDs),
       "NumberGridCells"=length(GRDshp),
       "GridWeights"=dfgrid)


  # write Gridweights file (if provided)
  #----------------------------------------------------------

  if (!is.null(outfile)) {
    rvn_gridweights_write(gridweights, outfile)
  }

  #for debugging only:
  # return (list(area=isect$area,id=isect$id,id1=isect$hindex,id2=isect$gindex,HRU=HRUshp, weightsum=wtsum))

  return(gridweights)
}
