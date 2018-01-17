##' @title Decimal degree grid from polar stereographic spatial objects
##' @description Outputs a list of grid lines that can be used to plot a grid on polar stereographic maps. Requires a spatial object projected using UTM coordinates
##' @param dat Spatial object projected using UTM coordinates, such as \code{\link{readShapeSpatial}} OR a numeric vector of length 4 where first element defines the minimum longitude, second element the maximum longitude, third element the minimum latitude and fourth element the maximum latitude of the bounding box. In case of a numeric vector, the coordinates have to be given as decimal degrees.
##' @param lat.interval the interval of latitude grids for polar stereographic maps (\code{type = "arctic50"} or \code{"arctic60"})
##' @param lon.interval the interval of longitude grids for polar stereographic maps (\code{type = "arctic50"} or \code{"arctic60"})
##' @param n.points number of points used in creating the grid lines. The more points, the smoother the lines.
##' @param proj4.utm projection in \code{dat}. If TRUE (default), fetches the projection from \code{dat}. If \code{dat} is a numeric vector and \code{proj4.utm = TRUE}, \code{CRS("+init=epsg:32633")} is used as UTM projection (the same projection than in Svalbard shapefiles).
##' @param proj4.deg desired decimal degree projection. Uses a reasonable default projection. Do not change.
##' @return Returns a UTM coodrinate grid.
##' @seealso \code{\link{basemap}}
##' @author Mikko Vihtakari
##' @import maptools sp rgdal
##' @export
# dat = Land; lat.interval = 10; lon.interval = 45; n.points = 1000; proj4.utm = TRUE; proj4.deg = "+proj=longlat +datum=WGS84"
deg_grid_polar <- function(dat, lat.interval = 10, lon.interval = 45, n.points = 1000, proj4.utm = TRUE, proj4.deg = "+proj=longlat +datum=WGS84") {

if(is.numeric(dat)) {

  dat <- Polygon(matrix(c(dat[1], dat[3], dat[1], dat[4], dat[2], dat[4], dat[2], dat[3], dat[1], dat[3]), ncol = 2, byrow = TRUE))
  dat <- SpatialPolygons(list(Polygons(list(dat), ID = "a")), proj4string=CRS(proj4.deg))
  dat.deg <- dat
  if(proj4.utm) proj4.utm <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  dat <- spTransform(dat, CRS(proj4.utm))

} else {

  if(proj4.utm) proj4.utm <- proj4string(dat)
  dat.deg <- spTransform(dat, CRS("+proj=longlat +datum=WGS84"))

  }

x <- sp::bbox(dat.deg)


### Latitude grid lines

lat.breaks <- seq(x[2], 90, by = lat.interval) #lims[3], lims[4]

lats <- latgrid(latitude = lat.breaks, projection = proj4.utm, n.points. = n.points)
y <- transform_coord(lats, lon = "lon.utm", lat = "lat.utm", proj.og = proj4.utm, proj.out = proj4.deg, new.names = c("lon", "lat"), verbose = FALSE)

out <- list()
out$lat <- cbind(lats, y)

### Latitude (y-axis) breaks for maps

lat.breaks <- lat.breaks[c(-1, -length(lat.breaks))]
tmp <- data.frame(label =  lat.breaks, lat = lat.breaks, lon = 180)
tmp <- cbind(tmp, transform_coord(x = tmp, lon = "lon", lat = "lat", proj.og = proj4.deg, proj.out = proj4.utm, verbose = FALSE))

out$lat.breaks <- tmp

### Longitude grid lines

lon.breaks <- sort(seq(0, 360, by = lon.interval) - 180)[-1]

lons <- longrid(lon.breaks, projection = proj4.utm, base.lat = x[2])

out$lon <- lons

### Longitude (x-axis) breaks for maps

#tmp <- data.frame(latitude = min(x[2,]), longitude = unique(out$lon$lon))
#tmp <- cbind(tmp, transform_coord(x = tmp, proj.og = proj4.deg, proj.out = proj4.utm, verbose = FALSE))

out$lon.breaks <- NA #data.frame(deg = tmp$longitude, utm = tmp$lon.utm)

### Projections and map boundaries

out$utm.proj <- proj4.utm
out$deg.proj <- proj4.deg

out$boundaries <- data.frame(lon.deg = x[1,], lat.deg = x[2,], lon.utm = bbox(dat)[1,], lat.utm = bbox(dat)[2,])

class(out) <- "degGridPolar"

out

}


latgrid <- function(latitude = NULL, projection = proj4.utm, n.points. = n.points){
  tmp <- lapply(1:length(latitude), function(i) {
    X <- latitude[i]
    r <- project(matrix(c(0,X), ncol = 2), proj = projection)[2]
    tt <- seq(0,2*pi,length.out = n.points.)
    xx <- r * cos(tt)
    yy <- r * sin(tt)
    return(data.frame(lon.utm = xx, lat.utm = yy, ID = paste0("lat", round(X,0))))
    })
  return(do.call(rbind, tmp))
}

## Longitude grid lines to maps
longrid <- function(longitude = NULL, base.lat = NULL, projection = proj4.utm){
  tmp <- lapply(1:length(longitude), function(i){
    X <- longitude[i]
    tp <- project(matrix(c(X, base.lat), ncol = 2), proj = projection)
    label <- ifelse(X >= 0, X, 360+X)
    angle <- ifelse(X >= 0, label-90, label-270)
    return(data.frame(lon.start = 0, lon.end = tp[1], lat.start = 0, lat.end = tp[2], ID = paste0("lon", X), label = label, angle = angle))
  })
  return(do.call(rbind, tmp))
}
