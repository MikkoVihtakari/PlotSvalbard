##' @title Decimal degree grid from UTM projected spatial objects
##' @description Outputs a list of grid lines that can be used to plot a grid on maps. Requires a spatial object projected using UTM coordinates
##' @param dat Spatial object projected using UTM coordinates, such as \code{\link{readShapeSpatial}} OR a numeric vector of length 4 where first element defines the minimum longitude, second element the maximum longitude, third element the minimum latitude and fourth element the maximum latitude of the bounding box. In case of a numeric vector, the coordinates have to be given as decimal degrees.
##' @param round.lat specifying the level of rounding to be used to plot latitude grid lines. Overrides \code{n.lat.grid}
##' @param round.lon specifying the level of rounding to be used to plot longitude grid lines. Overrides \code{n.lon.grid}
##' @param n.lat.grid number of latitude grid lines. Alternatively use \code{round.lat}
##' @param n.lon.grid number of longitude grid lines. Alternatively use \code{round.lon}
##' @param n.points number of points used in creating the grid lines. The more points, the smoother the lines.
##' @param proj4.utm projection in \code{dat}. If TRUE (default), fetches the projection from \code{dat}. If \code{dat} is a numeric vector and \code{proj4.utm = TRUE}, \code{CRS("+init=epsg:32633")} is used as UTM projection (the same projection than Kongsfjorden shapefiles).
##' @param proj4.deg desired decimal degree projection. Uses a reasonable default projection. Do not change.
##' @return Returns a UTM coodrinate grid.
##' @author Mikko Vihtakari
##' @import maptools sp rgdal
##' @export

## Test parameters. Delete when ready
# dat = krone.cr
# dat = c(12,15,78,79)
# n.lat.grid = 3
# n.lon.grid = 3
# n.points = 10
# round.lat = 0.015
# round.lon = 0.1
# proj4.deg = "+proj=longlat +datum=WGS84"
# proj4.utm = TRUE

deg_grid <- function(dat, round.lat = FALSE, n.lat.grid = 3, round.lon = FALSE, n.lon.grid = 3, n.points = 10, proj4.utm = TRUE, proj4.deg = "+proj=longlat +datum=WGS84") {

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

### Latitude grid linedats

if(is.numeric(round.lat)) {
  lats <- seq(plyr::round_any(unname(x[2,1]), round.lat, floor), plyr::round_any(unname(x[2,2]), round.lat, ceiling), by = round.lat)
  lats <- lats[lats > x[2,1] & lats < x[2,2]]
  lons <- seq(unname(x[1,1]), unname(x[1,2]), length.out = n.points)
  if(any(duplicated(lats))) warning("Duplicate values in latitude grid. Adjust round.lat")
} else {
lats <- seq(unname(x[2,1]), unname(x[2,2]), length.out = n.lat.grid +2)[2:(n.lat.grid+1)]
lons <- seq(unname(x[1,1]), unname(x[1,2]), length.out = n.points)
}

deg.df <- data.frame(long = rep(lons, length(lats)), lat = rep(lats, each = n.points), ID = rep(1:length(lats), each = n.points))

y <- deg.df

coordinates(y) <- c("long", "lat")
proj4string(y) <- CRS(proj4.deg)

y <- spTransform(y, CRS(proj4.utm))
y <- data.frame(coordinates(y))
colnames(y) <- paste(colnames(y), "utm", sep = ".")

out <- list()
out$lat <- cbind(deg.df, y)

tmp.utm <- lapply(unique(out$lat$ID), function(k) {
  tmp <- subset(out$lat, ID == k)
  tmp[which.min(tmp$long.utm),"lat.utm"]
})

out$lat.breaks <- data.frame(deg = unique(out$lat$lat), utm = unlist(tmp.utm))

### Longitude grid lines

if(is.numeric(round.lon)) {
lons <- seq(plyr::round_any(unname(x[1,1]), round.lat, floor), plyr::round_any(unname(x[1,2]), round.lat, ceiling), by = round.lon)
  lons <- lons[lons > x[1,1] & lons < x[2,2]]
  lats <- seq(unname(x[2,1]), unname(x[2,2]), length.out = n.points)
  if(any(duplicated(lons))) warning("Duplicate values in latitude grid. Adjust round.lon")
} else {
lons <- seq(unname(x[1,1]), unname(x[1,2]), length.out = n.lon.grid +2)[2:(n.lon.grid+1)]
lats <- seq(unname(x[2,1]), unname(x[2,2]), length.out = n.points)
}

deg.df <- data.frame(long = rep(lons, each = n.points), lat = rep(lats, length(lons)), ID = rep(1:length(lons), each = n.points))

y <- deg.df

coordinates(y) <- c("long", "lat")
proj4string(y) <- CRS(proj4.deg)

y <- spTransform(y, CRS(proj4.utm))
y <- data.frame(coordinates(y))
colnames(y) <- paste(colnames(y), "utm", sep = ".")

out$lon <- cbind(deg.df, y)

tmp.utm <- lapply(unique(out$lon$ID), function(k) {
  tmp <- subset(out$lon, ID == k)
  tmp[which.min(tmp$lat.utm), "long.utm"]
})

out$lon.breaks <- data.frame(deg = unique(out$lon$long), utm = unlist(tmp.utm))
out$boundaries <- data.frame(long.deg = x[1,], lat.deg = x[2,], long.utm = bbox(dat)[1,], lat.utm = bbox(dat)[2,])

out
}
