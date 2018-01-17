##' @title Decimal degree grid from UTM projected spatial objects
##' @description Outputs a list of grid lines that can be used to plot a grid on maps. Requires a spatial object projected using UTM coordinates
##' @param dat Spatial object projected using UTM coordinates, such as \code{\link{readShapeSpatial}} OR a numeric vector of length 4 where first element defines the minimum longitude, second element the maximum longitude, third element the minimum latitude and fourth element the maximum latitude of the bounding box. In case of a numeric vector, the coordinates have to be given as decimal degrees.
##' @param round.lat specifying the level of rounding to be used to plot latitude grid lines. Overrides \code{n.lat.grid}
##' @param round.lon specifying the level of rounding to be used to plot longitude grid lines. Overrides \code{n.lon.grid}
##' @param n.lat.grid number of latitude grid lines. Alternatively use \code{round.lat}
##' @param n.lon.grid number of longitude grid lines. Alternatively use \code{round.lon}
##' @param expar number specifying the fraction by which range of grid lines should be expanded to make them cover the entire map. See argument \code{f} for \code{\link[grDevices]{extendrange}} function. Set \code{expar = 0}, if you want exact grid lines.
##' @param n.points number of points used in creating the grid lines. The more points, the smoother the lines.
##' @param proj4.utm projection in \code{dat}. If TRUE (default), fetches the projection from \code{dat}. If \code{dat} is a numeric vector and \code{proj4.utm = TRUE}, \code{CRS("+init=epsg:32633")} is used as UTM projection (the same projection than in Svalbard shapefiles).
##' @param proj4.deg desired decimal degree projection. Uses a reasonable default projection. Do not change.
##' @return Returns a UTM coodrinate grid.
##' @seealso \code{\link{basemap}}
##' @author Mikko Vihtakari
##' @import maptools sp rgdal
##' @export

## Test parameters. Delete when ready
# dat = krone.cr
# dat = c(10,28,76,81)
# n.lat.grid = 3
# n.lon.grid = 3
# n.points = 10
# round.lat = 0.015
# round.lon = 0.1
# proj4.deg = "+proj=longlat +datum=WGS84"
# proj4.utm = TRUE
# expar = 0.1

deg_grid <- function(dat, round.lat = FALSE, n.lat.grid = 3, round.lon = FALSE, n.lon.grid = 3, expar = 0.3, n.points = 10, proj4.utm = TRUE, proj4.deg = "+proj=longlat +datum=WGS84") {

if(is.numeric(dat)) {

  dat <- Polygon(matrix(c(dat[1], dat[3], dat[1], dat[4], dat[2], dat[4], dat[2], dat[3], dat[1], dat[3]), ncol = 2, byrow = TRUE))
  dat <- SpatialPolygons(list(Polygons(list(dat), ID = "a")), proj4string=CRS(proj4.deg))
  dat.deg <- dat
  if(proj4.utm) proj4.utm <- "+init=epsg:32633"
  dat <- spTransform(dat, CRS(proj4.utm))

} else {

  if(proj4.utm) proj4.utm <- proj4string(dat)
  dat.deg <- spTransform(dat, CRS("+proj=longlat +datum=WGS84"))

  }

x <- bbox(dat.deg)

### Latitude grid lines

if(is.numeric(round.lat)) {
  lats <- extendrange(c(unname(x[2,1]), unname(x[2,2])), f = expar)
  lats <- seq(floor(lats[1] / round.lat) * round.lat, ceiling(lats[2] / round.lat) * round.lat, by = round.lat)
  #lats <- lats[lats > x[2,1] & lats < x[2,2]]
  lons <- extendrange(c(unname(x[1,1]), unname(x[1,2])), f = expar)
  lons <- seq(lons[1], lons[2], length.out = n.points)
  if(any(duplicated(lats))) warning("Duplicate values in latitude grid. Adjust round.lat")
} else {
lats <- extendrange(c(unname(x[2,1]), unname(x[2,2])), f = expar)
lats <- seq(lats[1], lats[2], length.out = n.lat.grid +2)[2:(n.lat.grid+1)]
lons <- extendrange(c(unname(x[1,1]), unname(x[1,2])), f = expar)
lons <- seq(lons[1], lons[2], length.out = n.points)
}

deg.df <- data.frame(lon = rep(lons, length(lats)), lat = rep(lats, each = n.points), ID = rep(1:length(lats), each = n.points))

y <- transform_coord(deg.df, lon = "lon", lat = "lat", proj.og = proj4.deg, proj.out = proj4.utm, verbose = FALSE)

out <- list()
out$lat <- cbind(deg.df, y)

### Latitude (y-axis) breaks for maps

tmp <- data.frame(latitude = unique(out$lat$lat), longitude = min(x[1,]))
tmp <- cbind(tmp, transform_coord(x = tmp, proj.og = proj4.deg, proj.out = proj4.utm, verbose = FALSE))

out$lat.breaks <- data.frame(deg = tmp$latitude, utm = tmp$lat.utm)

### Longitude grid lines

if(is.numeric(round.lon)) {
  lons <- extendrange(c(unname(x[1,1]), unname(x[1,2])), f = expar)
  lons <- seq(floor(lons[1] / round.lon) * round.lon, ceiling(lons[2] / round.lon) * round.lon, by = round.lon)
  #lons <- lons[lons > x[1,1] & lons < x[2,2]]
  lats <- extendrange(c(unname(x[2,1]), unname(x[2,2])), f = expar)
  lats <- seq(lats[1], lats[2], length.out = n.points)
  if(any(duplicated(lons))) warning("Duplicate values in latitude grid. Adjust round.lon")
} else {
  lons <- extendrange(c(unname(x[1,1]), unname(x[1,2])), f = expar)
  lons <- seq(lons[1], lons[2], length.out = n.lon.grid +2)[2:(n.lon.grid+1)]
  lats <- extendrange(c(unname(x[2,1]), unname(x[2,2])), f = expar)
  lats <- seq(lats[1], lats[2], length.out = n.points)
}

deg.df <- data.frame(lon = rep(lons, each = n.points), lat = rep(lats, length(lons)), ID = rep(1:length(lons), each = n.points))

y <- transform_coord(deg.df, lon = "lon", lat = "lat", proj.og = proj4.deg, proj.out = proj4.utm, verbose = FALSE)

out$lon <- cbind(deg.df, y)

### Longitude (x-axis) breaks for maps

tmp <- data.frame(latitude = min(x[2,]), longitude = unique(out$lon$lon))
tmp <- cbind(tmp, transform_coord(x = tmp, proj.og = proj4.deg, proj.out = proj4.utm, verbose = FALSE))

out$lon.breaks <- data.frame(deg = tmp$longitude, utm = tmp$lon.utm)

### Projections and map boundaries

out$utm.proj <- proj4.utm
out$deg.proj <- proj4.deg

out$boundaries <- data.frame(lon.deg = x[1,], lat.deg = x[2,], lon.utm = bbox(dat)[1,], lat.utm = bbox(dat)[2,])

class(out) <- "degGrid"

out

}
