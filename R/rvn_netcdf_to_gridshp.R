#' @title Generate grid overlay from netCDF file
#'
#' @description
#' Takes the latitude-longitude cell coordinates from a netCDF file (assumed to be named 'lat' and 'long')
#' generates an estimate of the grid polygons associated with each netCDF cell and exports this to a shapefile (outshp)
#'
#' @details
#' Note that the function can fail due to bad netCDF file or inappropriate UTM zone.
#'
#' @param ncfile netCDF file with latitude and longitude variables
#' @param UTMzone UTM zone for exported shapefile (integer)
#' @param outshp name of output shapefile prefix (i.e., no .shp extension, DEFAULT 'rvn_output_shapefile')
#'
#' @return \item{TRUE}{returns TRUE if executed properly. Also generates shapefile in folder}
#'
#' @seealso \code{\link{rvn_gen_gridweights}} for generating a shapefile of gridweights
#'
#' @author James R. Craig, University of Waterloo, 2019
#'
#' @examples
#'
#' ncfile <- system.file("extdata/Nith_era5_sample.nc", package="RavenR")
#' UTMzone <- 17
#' outshp <- 'Nith_output_shapefile'
#' rvn_netcdf_to_gridshp(ncfile, UTMzone, outshp)
#'
#'
#' @export rvn_netcdf_to_gridshp
#' @importFrom sp coordinates proj4string spTransform Polygons Polygon SpatialPolygonsDataFrame SpatialPolygons CRS
#' @importFrom rgdal writeOGR
#' @importFrom deldir deldir tile.list
#' @importFrom methods slot
#' @importFrom ncdf4 nc_open ncvar_get
rvn_netcdf_to_gridshp <- function(ncfile,UTMzone,outshp='rvn_output_shapefile')
{

  # extract lat-long from netCDF file
  #-----------------------------------------------------------------------
  nc_data <- nc_open(ncfile)
  mydata <- ncvar_get(nc_data, "lat")
  lat <- as.vector(t(mydata))
  mydata <- ncvar_get(nc_data, "lon")
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
  # latlong <- data.frame(ID = 1:2, X = long, Y = lat)
  latlong <- data.frame(ID = IDs, X = long, Y = lat)
  sp::coordinates(latlong) <- c("X", "Y")
  sp::proj4string(latlong) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(latlong, CRS(paste0("+proj=utm +zone=",UTMzone," ellps=WGS84")))
  xx<-res@coords[1:length(long)]
  yy<-res@coords[length(long)+(1:length(long))]

  # generate voronoi diagram
  #-----------------------------------------------------------------------
  voronoi <- deldir(xx,yy)

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
  writeOGR(voronoishp,dsn=getwd(),layer=outshp, driver="ESRI Shapefile")

  #plot(voronoishp,col="blue")

  return(TRUE)
}

