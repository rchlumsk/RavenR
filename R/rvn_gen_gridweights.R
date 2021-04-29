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
#' wt[k][g] for any k, over all g, is 1.0
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
#' @param HRUshpfile polygon shapefile of HRUs with data column containing HRU IDs (.shp extension expected)
#' @param Gridshpfile polygon shapefile of grid cells with data column containing cell IDs (.shp extension expected)
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
#' # define HRU shapefile (use subbasin shapefile for example)
#' HRUshpfile <- system.file("extdata","Nith_shapefile_sample.shp",package = "RavenR")
#'
#' # write grid shapefile from netcdf file
#' nithnc <- system.file("extdata/Nith_era5_sample.nc", package="RavenR")
#' Gridshpfile <- file.path(tempdir(), "Nith_gridcells.shp")
#' myshp <- rvn_netcdf_to_gridshp(ncfile=nithnc, projID=26917, outshp=Gridshpfile)
#'
#' # generate .rvt file of grid weights
#' ValidHRUIDs <- rvh$HRUtable$ID
#' tfout <- file.path(tempdir(), "Nith_GridWeights.rvt")
#' rvn_gen_gridweights(HRUshpfile, Gridshpfile, ValidHRUIDs,
#' gridIDcol = 'GridIDs', HRUIDcol = "subID", outfile = tfout)
#'
#'
#' @export rvn_gen_gridweights
#' @importFrom rgdal readOGR
#' @importFrom rgeos gBuffer gIntersection
#' @importFrom methods slot
#' @importFrom raster crs
#' @importFrom sf st_crs
#' @importFrom dplyr group_by summarise
rvn_gen_gridweights <- function(HRUshpfile, Gridshpfile, ValidHRUIDs=NULL, HRUIDcol="HRU_ID",
                                gridIDcol="GridIDs", outfile=NULL)
{

  # read in HRU file
  #----------------------------------------------------------
  dsn=normalizePath(dirname(HRUshpfile))
  lay=substr(basename(HRUshpfile),1,nchar(basename(HRUshpfile))-4)
  HRUshp<-readOGR(dsn = dsn,layer = lay) # returns SpatialPolygonsDataFrame
  # HRUshp <- read_sf(HRUshpfile)
  if (!(HRUIDcol %in% colnames(HRUshp@data))){
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
  dsn=normalizePath(dirname(Gridshpfile))
  lay=substr(basename(Gridshpfile),1,nchar(basename(Gridshpfile))-4)
  GRDshp<-readOGR(dsn = dsn,layer = lay) # returns SpatialPolygonsDataFrame
  # GRDshp <- read_sf(Gridshpfile)
  if (!(gridIDcol %in% colnames(GRDshp@data))){
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
  HRUshp$HRUarea <-sapply(slot(HRUshp, "polygons"), slot, "area")
  GRDshp$Gridarea<-sapply(slot(GRDshp, "polygons"), slot, "area")
  #HRUshp$polygons is a SpatialPolygons datatype [sp]

  # intersect HRUs and Grids after repairing HRU file structure
  #----------------------------------------------------------
  print("fixing")
  HRUshp <- gBuffer(HRUshp, byid=TRUE, width=0) # returns SpatialPolygonsDataFrame
  GRDshp <- gBuffer(GRDshp, byid=TRUE, width=0) # returns SpatialPolygonsDataFrame
  print("...done fixing")

  print("intersecting...")
  isect<-gIntersection(HRUshp, GRDshp, byid=TRUE)
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
  isect$iarea<-sapply(slot(isect, "polygons"), slot, "area")

  # extract HRU and grid indices from gIntersection-generated 'ID' field
  #----------------------------------------------------------
  isect$id   <-sapply(slot(isect, "polygons"), slot, "ID")
  for (k in 1:nrow(isect)) {
    isect$hindex[k]<-as.numeric(strsplit(isect$id[k], " ")[[1]][1])+1
    isect$gindex[k]<-as.numeric(strsplit(isect$id[k], " ")[[1]][2])+1
  }

  # compute Gridweights data frame
  #----------------------------------------------------------
  # wtsum <-rep(0.0,nrow(HRUshp))
  dfgrid <- data.frame(matrix(NA,nrow=nrow(isect),ncol=3))
  colnames(dfgrid) <- c("HRUID","GRIDID","WEIGHT")
  for (i in 1:nrow(isect)) {
    wt<-0
    k<-isect$hindex[i] # HRU index
    g<-isect$gindex[i] # grid cell index
    if ((k>=1) & (k<=nrow(HRUshp))) {
      # for each HRU, wt[k][g]=area[k][g]/area[k]

      if (HRUshp$HRUarea[k]>0){
        wt<-isect$iarea[i] / HRUshp$HRUarea[k]
      }
      if (wt>0){
        HRUID <- as.numeric(HRUshp[[HRUIDcol]][k])
        GridID <- as.numeric(GRDshp[[gridIDcol]][g])
        # if (HRUID %in% ValidHRUIDs){
          # write(paste("  ",HRUID," ",GridID ," ",wt),append=TRUE,file=outfile)
        dfgrid[i,] <- c(HRUID, GridID, wt)
        # }
        # wtsum[k]<-wtsum[k]+wt
      }
    }
  }

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


  # write Gridweights file
  #----------------------------------------------------------

  if (!is.null(outfile)) {
    rvn_gridweights_write(gridweights, outfile)
  }

  #for debugging only:
  # return (list(area=isect$area,id=isect$id,id1=isect$hindex,id2=isect$gindex,HRU=HRUshp, weightsum=wtsum))

  return(gridweights)
}
