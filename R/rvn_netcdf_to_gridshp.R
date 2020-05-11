#' Generate grid overlay from netCDF file
#'
#' Takes the latitude-longitude cell coordinates from a netCDF file (assumed to be named 'lat' and 'long')
#' generates an estimate of the grid polygons associated with each netCDF cell and exports this to a shapefile (outshp)
#'
#' @param ncfile netCDF file with latitude and longitude variables
#' @param UTMzone UTM zone for exported shapefile (integer)
#' @param outshp name of output shapefile prefix (i.e., no .shp extension)
#'
#' @return \item{TRUE}{returns TRUE if executed properly. Also generates shapefile in folder}
#'
#' @details not alot of QA/Qc included - can fail due to bad netCDF file or inappropriate UTM zone;
#' uses ncdf.tools, sp, rgdal, deldir, and sna libraries
#'
#' @seealso \code{\link{rvn_gen_gridweights}} for generating a shapefile of gridweights
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#'
#' @author James R. Craig, University of Waterloo, 2019
#' @keywords netcdf grid shapefile conversion
#' @examples
#' \dontrun{
#' rvn_netcdf_to_gridshp(2017052200.nc,11,voronoi_out)
#'
#' # Example usage:
#' setwd("C:/temp/")
#' ncfile<-"force/2017052200.nc"
#' UTMzone<-11
#' outshp<-"maps/RDPS_Grid"
#' netcdf.to.gridshp(ncfile,UTMzone,outshp)
#'
#' }
#'
#'
#' @export rvn_netcdf_to_gridshp
rvn_netcdf_to_gridshp <- function(ncfile,UTMzone,outshp)
{

  require(ncdf.tools)
  require(sp)
  require(rgdal)
  require(deldir)
  require(sna)

  # extract lat-long from netCDF file
  #-----------------------------------------------------------------------
  mydata<-ncdf.tools::readNcdf(ncfile,var.name=c("lat"))
  lat<-as.vector(t(mydata))
  mydata<-ncdf.tools::readNcdf(ncfile,var.name=c("lon"))
  long<-as.vector(t(mydata))

  ncols=length(mydata[1,])
  nrows<-length(mydata[,1])
  c<-rep(1:ncols,nrows)
  r<-rep(1:nrows,each=ncols)
  #IDs<-(r-1)*ncols+(c-1)
  IDs<-(c-1)*nrows+(r-1)
  #print(c)


  # transform lat-long to UTM zone
  #-----------------------------------------------------------------------
  latlong <- data.frame(ID = 1:2, X = long, Y = lat)
  coordinates(latlong) <- c("X", "Y")
  proj4string(latlong) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(latlong, CRS(paste0("+proj=utm +zone=",UTMzone," ellps=WGS84")))
  xx<-res@coords[1:length(long)]
  yy<-res@coords[length(long)+(1:length(long))]

  # generate voronoi diagram
  #-----------------------------------------------------------------------
  voronoi <- deldir::deldir(xx,yy)

  #plot(xx, yy, type="n", asp=1)
  #points(xx,yy,  pch=20, col="red", cex=0.5)
  #plot(voronoi, wlines="tess", wpoints="none", number=FALSE, add=TRUE, lty=1)

  polys = vector(mode='list', length=length(tile.list(voronoi)))
  w<-tile.list(voronoi)
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }

  SP = SpatialPolygons(polys)

  rnames<-sapply(slot(SP, 'polygons'), function(x) slot(x, 'ID'))

  voronoishp = SpatialPolygonsDataFrame(SP, data=data.frame(x=xx,y=yy, row.names=rnames,GridIDs=IDs,Latit=lat,Longit=long))

  # write to file
  #-----------------------------------------------------------------------
  unlink(paste0(outshp,".*")) # deletes old file if it exists
  rgdal::writeOGR(voronoishp,dsn=getwd(),layer=outshp, driver="ESRI Shapefile")

  #plot(voronoishp,col="blue")

  return (TRUE)
}

