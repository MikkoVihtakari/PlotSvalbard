#' @title Clip ocean current data frame to fit a basemap
#' @description An internal utility function to clip ocean current data frames to fit the boundaries of a basemap
#' @param dt data frame containing ocean currents. Must be run through the \code{\link[broom]{tidy}} function to contain \code{long}, \code{lat} and \code{group} columns.
#' @param X A basemapData object
#' @param shrink numeric between 0 and 100 defining the percentage of clipping ocean current arrows to be done vertically and horizontally. 
#' @author Mikko Vihtakari
#' @keywords internal
#' @export


clip_current <- function(dt, X, shrink = 0.5) {
  
  #limits <- c(X$Grid$limits$lon.deg, X$Grid$limits$lat.deg)
  
  limits <- c(X$Grid$limits$lon.utm, X$Grid$limits$lat.utm)
  
  xdist <- diff(limits[1:2])
  ydist <- diff(limits[3:4])
  
  xshr <- xdist*(shrink/100)
  yshr <- ydist*(shrink/100)
  
  lims <- c(limits[1]+xshr, limits[2]-xshr, limits[3]+yshr, limits[4]-yshr)
  
  #lims <- basemap_limits(c(limits[1]+xshr, limits[2]-xshr, limits[3]+yshr, limits[4]-yshr), type = type)$bound_utm
  
  lon_min <- lims[1]#min(lims$lon)
  lat_min <- lims[3]#min(lims$lat)
  lon_max <- lims[2]#max(lims$lon)
  lat_max <- lims[4]#max(lims$lat)
  
tp <- if(!is.null(limits)) {
  
  #j <- unique(dt$group)[15]
  lapply(unique(dt$group), function(j) {
    
      tmp <- dt[dt$group == j,]
    
      if(!all(tmp$long > lon_min & tmp$long < lon_max & tmp$lat > lat_min & tmp$lat < lat_max)) {
        tmp <- tmp[tmp$long > lon_min & tmp$long < lon_max & tmp$lat > lat_min & tmp$lat < lat_max,]
      }
      
      tmp
    })
  }

  do.call(rbind, tp)
}
