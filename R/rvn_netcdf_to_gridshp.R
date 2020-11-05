#' @title Generate grid overlay from netCDF file
#'
#' @description
#' Takes the latitude-longitude cell coordinates from a netCDF file (assumed to be named 'lat' and 'long')
#' generates an estimate of the grid polygons associated with each netCDF cell and exports this to a shapefile (outshp)
#'
#' @details
#' projID should be provided as an integer value referring to a valid EPSG coordinate system. If projID is left NULL, the
#' output shapefile will be left in lat/long coordinates and WGS84 projection. Note that the function can fail due to bad
#' netCDF file or inappropriate coordinate system ID. A list of IDs for
#' projected coordinate system may be found on the \href{https://spatialreference.org/ref/epsg/}{Spatial Reference webpage}
#' or on the \href{http://resources.esri.com/help/9.3/arcgisserver/apis/rest/pcs.html}{ESRI webpage}.
#'
#' If the outshp is NULL, the shapefile object is returned by the function and nothing is written to file. If outshp is provided,
#' the shapefile is also written to file. The outshp should be supplied as the file name (with or without .shp extension).
#'
#' This function uses the sf::st_write function to write the shapefile, if outshp is provided.
#'
#' Additional metadata on the sample netcdf file may be found with `?Nith_era5_sample`.
#'
#' @param ncfile netCDF file with latitude and longitude variables
#' @param projID projected coordinate system ID (EPSG numeric code) to project shapefile to (default NULL, optional)
#' @param outshp name of output shapefile (DEFAULT NULL, optional)
#'
#' @return \item{shapefile}{returns sf shapefile object; will also write a shapefile to the outshp if provided}
#'
#' @seealso \code{\link{rvn_gen_gridweights}} for generating a shapefile of gridweights
#'
#' @author James R. Craig, University of Waterloo, 2019
#'
#' @examples
#'
#' # get location for sample netcdf file
#' ?Nith_era5_sample
#' ncfile <- system.file("extdata/Nith_era5_sample.nc", package="RavenR")
#'
#' # produce shapefile in lat/long
#' myshp <- rvn_netcdf_to_gridshp(ncfile)
#' class(myshp)
#' sf::st_crs(myshp)$input
#' plot(myshp$geometry)
#'
#' # write shapefile to file in UTM coordinates
#' projID <- 26917 # NAD83 UTM Zone 17N, appropriate UTM zone for Nith watershed
#' outshp <- file.path(tempdir(), "Nith_gridcells.shp")
#' myshp <- rvn_netcdf_to_gridshp(ncfile, projID, outshp)
#' sf::st_crs(myshp)$input
#'
#'
#' @export rvn_netcdf_to_gridshp
#' @importFrom sp coordinates proj4string spTransform Polygons Polygon SpatialPolygonsDataFrame SpatialPolygons CRS
#' @importFrom sf st_write
#' @importFrom deldir deldir tile.list
#' @importFrom methods slot
#' @importFrom ncdf4 nc_open ncvar_get
rvn_netcdf_to_gridshp <- function(ncfile,projID=NULL,outshp=NULL)
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

  # build SpatialPointsDataFrame, transform lat-long to UTM zone (if warranted)
  #-----------------------------------------------------------------------
  # latlong <- data.frame(ID = 1:2, X = long, Y = lat)
  res <- data.frame(ID = IDs, X = long, Y = lat)
  sp::coordinates(res) <- c("X", "Y")
  sp::proj4string(res) <- sp::CRS("+proj=longlat +datum=WGS84")
  if (!is.null(projID)) {
    # res <- spTransform(latlong, sp::CRS(paste0("+proj=utm +zone=",UTMzone," ellps=WGS84")))
    res <- spTransform(res, sp::CRS(sprintf("+init=epsg:%i",projID)))
    myprojstring = sp::CRS(sprintf("+init=epsg:%i",projID))
  } else {
    myprojstring = sp::CRS("+proj=longlat +datum=WGS84")
  }
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
  sp::proj4string(voronoishp) <- myprojstring # assign projection to new shapefile accordingly
  voronoishp <- sf::st_as_sf(voronoishp)

  # write to file (if path given)
  #-----------------------------------------------------------------------
  if (!is.null(outshp)) {

    # # remove .shp if exists
    # if (rvn_substrRight(outshp,4) == ".shp") {
    #   outshp <- rvn_substrMRight(outshp,4)
    # }

    if (!endsWith(outshp, ".shp")) {
      outshp <- paste0(outshp, ".shp")
    }

    # unlink(paste0(outshp,".*")) # deletes old file if it exists
    # writeOGR(voronoishp,dsn=getwd(),layer=outshp, driver="ESRI Shapefile")
    # writeOGR(voronoishp, dsn=outshp, )
    st_write(voronoishp, dsn=outshp, driver="ESRI Shapefile", delete_dsn=TRUE)
    print(sprintf("%s written to file", outshp))
  }

  return(voronoishp)
}

