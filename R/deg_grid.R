##' @title Decimal degree grid from UTM projected spatial objects
##' @description Outputs a list of grid lines that can be used to plot a grid on maps. Requires a spatial object projected using UTM coordinates
##' @param dat \code{\link[basemap_limits]{basemapLimits}} object
##' @param round.lat specifying the level of rounding to be used to plot latitude grid lines. Overrides \code{n.lat.grid}
##' @param round.lon specifying the level of rounding to be used to plot longitude grid lines. Overrides \code{n.lon.grid}
##' @param n.lat.grid number of latitude grid lines. Alternatively use \code{round.lat}
##' @param n.lon.grid number of longitude grid lines. Alternatively use \code{round.lon}
##' @param n.points number of points used in creating the grid lines. The more points, the smoother the lines.
##' @return Returns a UTM coodrinate grid.
##' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function. Common users do not need to worry about these details.
##' @keywords internal
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
# round.lat = 0.1
# round.lon = 0.1
# proj4.deg = "+proj=longlat +datum=WGS84"
# proj4.utm = TRUE
# expar = 0.1
# dat <- lims
deg_grid <- function(dat, round.lat = FALSE, n.lat.grid = 3, round.lon = FALSE, n.lon.grid = 3, n.points = 30) {

if(class(dat) != "basemapLimits") stop("deg_grid function requires a basemapLimits object")
  
## Definitions ####
  
calc_lat <- TRUE
calc_lon <- TRUE
out <- list()

############################################
### Latitude grid lines and axis labels ####
  
if(is.numeric(round.lat)) {
  lats <- range(dat$bound_dd$lat)
  lats <- seq(floor(lats[1] / round.lat) * round.lat, ceiling(lats[2] / round.lat) * round.lat, by = round.lat)
  lons <- range(dat$bound_dd$lon)
  lons <- seq(lons[1], lons[2], length.out = n.points)
  
  lat_rounding <- ifelse(round.lat >= 1, 0, ifelse(round.lat >= 0.1, 1, ifelse(round.lat >= 0.01, 2, ifelse(round.lat >= 0.001, 3, 4))))
    
  if(!any(lats >= dat$limits_dd[3] & lats <= dat$limits_dd[4] )) {
      calc_lat <- FALSE
      warning("Too large round.lat value. No latitude grid plotted.")
    }
  } else {
   lats <- range(dat$bound_dd$lat)
   lat_diff <- diff(lats)
   lat_rounding <- ifelse(lat_diff > 1, 1, ifelse(lat_diff > 0.1, 2, ifelse(lat_diff > 0.01, 3, 4)))
   
   lats <- round(seq(lats[1], lats[2], length.out = n.lat.grid +2)[2:(n.lat.grid+1)], lat_rounding)
 
   lons <- range(dat$bound_dd$lon)
   lons <- seq(lons[1], lons[2], length.out = n.points)
  } 

### Make to grid ##
  
  if(calc_lat) {
    deg.df <- data.frame(lon = rep(lons, length(lats)), lat = rep(lats, each = n.points), ID = rep(1:length(lats), each = n.points))

deg.df <- transform_coord(deg.df, lon = "lon", lat = "lat", proj.og = dat$proj_deg, proj.out = dat$proj_utm, verbose = FALSE, bind = TRUE)

### Remove lines are entirely outside the map limits

x <- split(deg.df, deg.df$ID)

x <- lapply(x, function(k) {
  if(any(k$lat.utm >= dat$limits_utm[3] & k$lat.utm <= dat$limits_utm[4])) {
    k
  } else {
    NULL
  }
})

x <- Filter(Negate(is.null), x)

### Assign to a list

out$lat <- do.call(rbind, x)

### Latitude (y-axis) breaks for maps

#k <- x[[1]]
lat_breaks <- lapply(x, function(k) {
  
  if(unique(k$lat) >= dat$limits_dd[3] & unique(k$lat) <= dat$limits_dd[4]) {
  
    ga <- sp::Lines(list(sp::Line(as.matrix(k[c("lon.utm", "lat.utm")]))), ID = unique(k$lat))
    ga <- sp::SpatialLines(list(ga), proj4string = sp::CRS(dat$proj_utm))
  
    ba <-rgeos::gIntersection(dat$bound_utm_shp, ga, byid = TRUE)
  
    da <- sp::coordinates(ba)[[1]][[1]]
    da <- da[which.min(da[,1]),]
  
    out <- data.frame(label = sprintf(paste0("%.", lat_rounding, "f"), unique(k$lat)), lon_utm = unname(da[1]), lat_utm = unname(da[2]))
    
    transform_coord(out, lon = "lon_utm", lat = "lat_utm", new.names = c("lon_dd", "lat_dd"), proj.og = dat$proj_utm, proj.out = dat$proj_deg, bind = TRUE, verbose = FALSE)  
    
  } else NULL
  
})


out$lat.breaks <- do.call(rbind, lat_breaks)

  } else {
    out$lat <- NULL
    out$lat.breaks <- NULL
  }

#############################################
### Longitude grid lines and axis labels ####

if(is.numeric(round.lon)) {
  lons <- range(dat$bound_dd$lon)
  lon_rounding <- ifelse(round.lon >= 1, 0, ifelse(round.lon >= 0.1, 1, ifelse(round.lon >= 0.01, 2, ifelse(round.lon >= 0.001, 3, 4))))
  lons <- seq(floor(lons[1] / round.lon) * round.lon, ceiling(lons[2] / round.lon) * round.lon, by = round.lon)
  lats <- range(c(dat$bound_dd$lat, 90))
  #lats <- seq(lats[1], lats[2], length.out = n.points)
  
  if(!any(lons >= dat$limits_dd[1] & lons <= dat$limits_dd[2])) {
      calc_lon <- FALSE
      warning("Too large round.lon value. No latitude grid plotted.")
  }
  
  if(any(duplicated(lons))) warning("Duplicate values in latitude grid. Adjust round.lon")
} else {
  lons <- range(dat$bound_dd$lon)
  lon_diff <- diff(lons)
  lon_rounding <- ifelse(lon_diff > 10, 0, ifelse(lon_diff > 1, 1, ifelse(lon_diff > 0.1, 2, ifelse(lon_diff > 0.01, 3, 4))))
   
  lons <- round(seq(lons[1], lons[2], length.out = n.lon.grid +2)[2:(n.lon.grid+1)], lon_rounding)
 
  lats <- range(c(dat$bound_dd$lat, 90))
}

### Make to grid ##
  
  if(calc_lon) {

deg.df <- data.frame(lon = rep(lons, each = length(lats)), lat = rep(lats, length(lons)), ID = rep(1:length(lons), each = length(lats)))

deg.df <- transform_coord(deg.df, lon = "lon", lat = "lat", proj.og = dat$proj_deg, proj.out = dat$proj_utm, verbose = FALSE, bind = TRUE)

### Remove lines are entirely outside the map limits

x <- split(deg.df, deg.df$ID)

x <- lapply(x, function(k) {
  if(any(k$lon.utm >= dat$limits_utm[1] & k$lon.utm <= dat$limits_utm[2])) {
    k
  } else {
    NULL
  }
})

x <- Filter(Negate(is.null), x)

### Assign to a list

out$lon <- do.call(rbind, x)

### Longitude (x-axis) breaks for maps

#k <- x[[2]]
lon_breaks <- lapply(x, function(k) {
  
  if(unique(k$lon) >= dat$limits_dd[1] & unique(k$lon) <= dat$limits_dd[2]) {
  ga <- sp::Lines(list(sp::Line(as.matrix(k[c("lon.utm", "lat.utm")]))), ID = unique(k$lon))
  ga <- sp::SpatialLines(list(ga), proj4string = sp::CRS(dat$proj_utm))
  
  ba <- rgeos::gIntersection(dat$bound_utm_shp, ga, byid = TRUE)
  
  da <- coordinates(ba)[[1]][[1]]
  da <- da[which.min(da[,2]),]
  
  out <- data.frame(label = sprintf(paste0("%.", lon_rounding, "f"), unique(k$lon)), lon_utm = unname(da[1]), lat_utm = unname(da[2]))
  transform_coord(out, lon = "lon_utm", lat = "lat_utm", new.names = c("lon_dd", "lat_dd"), proj.og = dat$proj_utm, proj.out = dat$proj_deg, bind = TRUE, verbose = FALSE)

  } else NULL  

})

out$lon.breaks <- do.call(rbind, lon_breaks)

  } else {
    out$lon <- NULL
    out$lon.breaks <- NULL
  }

### Projections and map boundaries

out$utm.proj <- dat$proj_utm
out$deg.proj <- dat$proj_deg

out$limits <- data.frame(lon.deg = dat$limits_dd[1:2], lat.deg = dat$limits_dd[3:4], lon.utm = dat$limits_utm[1:2], lat.utm = dat$limits_utm[3:4])

out$limits_shp_utm <- dat$bound_utm_shp

class(out) <- "degGrid"

out

}
