#' Generates a Raven grid weights file given an HRU shapefile and a grid shapefile
#'
#' Generate grid weights file GaugeWeights.rvi given an HRU shapefile with HRU ID column
#' HRUIDcol (default 'HRU_ID') and a grid shapefile with ID column gridIDcol (default: 'cellID')
#' weights are determined by the areal overlap of grid cell g and HRU k, i.e.,
#'  wt[k][g]=area[k][g]/area[k]
#' where wt[k][g] is the weight of cell g in HRU k, area[k] is the total area of HRU k,
#' and area[k][g] is the area of HRU k that is within cell g.
#'
#' By definition, the grid domain has to completely cover the HRU domain such that the sum of
#' wt[k][g] for any k, over all g, is 1.0
#'
#'  @param HRUshpfile polygon shapefile of HRUs with data column containing HRU IDs (.shp extension expected)
#'  @param Gridshpfile polygon shapefile of grid cells with data column containing cell IDs (.shp extension expected)
#'  @param ValidHRUIDs a vector of valid HRU IDs in the model
#'  @param HRUIDcol the name of the HRUshpfile polugon which contains the HRU IDs
#'  @param gridIDcol the name of the Gridshpfile polugon which contains the cell IDs
#'  @param outfile optional name of output Raven gaugeweights file
#'
#'
#'  @return \item{TRUE}{returns TRUE if executed properly. Also output GaugeWeights.rvi file}
#'
#'  @details not alot of QA/Qc included - can fail due to bad netCDF file or inappropriate UTM zone;
#'  uses rgdal and rgeos libraries. *Both shapefiles should be in same projection!* Unfortunately, the
#'  accuracy of the gIntersection() routine leaves a bit to be desired...
#'
#'  @author James R. Craig, University of Waterloo, 2019
#'
#'  @seealso \code{\link{rvn_netcdf_to_gridshp}} for converting netcdf files to grid shapefile format
#'
#'  @seealso \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R. Craig's research page} for software downloads,
#'  including the \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#'
#' @keywords netcdf grid shapefile conversion
#'
#' @examples
#'
#'  \dontrun{
#'   # netcdf.to.gridshp(2017052200.nc,11,GridCells)
#'   rvn_gen_gridweights(ModelHRUs.shp,GridCells.shp,ValidHRUIDs,HRUIDcol="ID")
#'
#'   # example workflow
#'   #--------------------------------------------
#'   setwd("C:/temp/RedDeer/")
#'
#'   HRUshpfile<-"./maps/RedDeerHRUs.shp" # needs to have single column referring to HRU ID (here presumed 'HRU_ID')
#'   GRDshpfile<-"./maps/RDPS_GridRedDeer2.shp"
#'   rvh<-rvh.read("reddeer.rvh")
#'
#'   validHRUIDs<-rvh$HRUtable$ID
#'
#'   tmp<-gridweights.gen(HRUshpfile,GRDshpfile, validHRUIDs, gridIDcol="GridIDs",HRUIDcol="HRU_ID",outfile="RDPSGridWeights.rvt")
#'
#'   #dsn=normalizePath(dirname(HRUshpfile))
#'   #lay=substr(basename(HRUshpfile),1,nchar(basename(HRUshpfile))-4)
#'   #HRUshp<-readOGR(dsn = dsn,layer = lay) # returns SpatialPolygonsDataFrame
#'
#'  }
#' @export rvn_gen_gridweights
rvn_gen_gridweights <- function(HRUshpfile,Gridshpfile,ValidHRUIDs,HRUIDcol="HRU_ID",gridIDcol="cellID",outfile="GridWeights.rvt")
{

  require(rgdal)
  require(rgeos)

  # read in HRU file
  #----------------------------------------------------------
  dsn=normalizePath(dirname(HRUshpfile))
  lay=substr(basename(HRUshpfile),1,nchar(basename(HRUshpfile))-4)
  HRUshp<-readOGR(dsn = dsn,layer = lay) # returns SpatialPolygonsDataFrame
  if (!(HRUIDcol %in% colnames(HRUshp@data))){
    print(paste0("HRU ID column name ",HRUIDcol,"is not in shapefile ",HRUshpfile))
    return()
  }

  # read in Grid file
  #---------------------------------------------------------
  dsn=normalizePath(dirname(Gridshpfile))
  lay=substr(basename(Gridshpfile),1,nchar(basename(Gridshpfile))-4)
  GRDshp<-readOGR(dsn = dsn,layer = lay) # returns SpatialPolygonsDataFrame
  if (!(gridIDcol %in% colnames(GRDshp@data))){
    print(paste0("Grid cell column name ",gridIDcol,"is not in shapefile ",Gridshpfile))
    return()
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
  for (k in 1:nrow(isect)){
    isect$hindex[k]<-as.numeric(strsplit(isect$id[k], " ")[[1]][1])+1
    isect$gindex[k]<-as.numeric(strsplit(isect$id[k], " ")[[1]][2])+1
  }

  # write Gridweights file
  #----------------------------------------------------------
  write("# -------------------------------------------------",append=FALSE,file=outfile)
  write("# grid weights file generated by RavenR gridweights.gen.R utility",append=TRUE,file=outfile)
  write("# -------------------------------------------------",append=TRUE,file=outfile)
  write(":GridWeights",append=TRUE,file=outfile)
  write(paste("  :NumberHRUs ",length(ValidHRUIDs)),append=TRUE,file=outfile)
  write(paste("  :NumberGridCells ",length(GRDshp)),append=TRUE,file=outfile)

  #asum1=0.0
  #asum2=0.0
  wtsum <-rep(0.0,nrow(HRUshp))
  for (i in 1:nrow(isect))
  {
    wt<-0
    k<-isect$hindex[i] # HRU index
    g<-isect$gindex[i] # grid cell index
    if ((k>=1) & (k<=nrow(HRUshp)))
    {
      # for each HRU, wt[k][g]=area[k][g]/area[k]

      if (HRUshp$HRUarea[k]>0){
        wt<-isect$iarea[i] / HRUshp$HRUarea[k]
      }
      if (wt>0){
        HRUID<-HRUshp$HRU_ID[k] #!! HARDCODED: only works because we know name of HRU ID column
        # must get field from name HRUIDcol
        GridID<-GRDshp$GridIDs[g] #!! only works because we know name of Grid ID column
        # must get field from name gridIDcol

        if (HRUID %in% validHRUIDs){
          write(paste("  ",HRUID," ",GridID ," ",wt),append=TRUE,file=outfile)
        }
        wtsum[k]<-wtsum[k]+wt
      }
    }
  }
  write(":EndGridWeights",append=TRUE,file=outfile)



  #for debugging only:
  return (list(area=isect$area,id=isect$id,id1=isect$hindex,id2=isect$gindex,HRU=HRUshp, weightsum=wtsum))
}
