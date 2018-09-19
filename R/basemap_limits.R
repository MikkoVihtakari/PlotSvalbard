#' @title Define limits for a basemap
#' @description An internal function to define limits for a basemap
#' @param limits See \code{\link{basemap}}
#' @param proj_deg Projection for \code{limits}. Defaults to decimal degrees
#' @param type Map type for output. Defines the used UTM projection (see \code{\link{basemap}} and \code{\link{map_type}})
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function. Common users do not need to worry about these details.
#' @keywords internal
#' @export
#' @import sp
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}} \code{\link{basemap_data}} \code{\link{deg_grid}} \code{\link{deg_grid_polar}}

basemap_limits <- function(limits, proj_deg = "+proj=longlat +datum=WGS84", type = MapType$map.type) {
  
  ## Projections  
  proj_utm <- map_projection(type)
  
  
  if(is.character(limits)) { ## Limits as a shapefile
    limits_utm_shp <- get(limits)
    limits_utm <- unname(c(limits_utm_shp@bbox[1,], limits_utm_shp@bbox[2,]))
    
    limits_dd_shp <- sp::spTransform(limits_utm_shp, sp::CRS(proj_deg))
    limits_dd <- unname(c(limits_dd_shp@bbox[1,], limits_dd_shp@bbox[2,]))
    
    bound_utm_shp <- limits_utm_shp
    
    ## Boundaries (data.frames specifying each corner)
    
    bla <- limits_utm_shp@polygons[[1]]@Polygons[[1]]@coords
    bla <- as.data.frame(bla)
    bla <- bla[-nrow(bla),]
    colnames(bla) <- c("lon", "lat")
    row.names(bla) <- c("SE", "NE", "NW", "SW")
    bound_utm <- bla
    
    bla <- limits_dd_shp@polygons[[1]]@Polygons[[1]]@coords
    bla <- as.data.frame(bla)
    bla <- bla[-nrow(bla),]
    colnames(bla) <- c("lon", "lat")
    row.names(bla) <- c("SE", "NE", "NW", "SW")
    bound_dd <- bla
    
  } else { ## Limits for numeric vectors
   if(is.numeric(limits)) {
    limits_dd <- limits
    limits_dd_shp <- sp::Polygon(matrix(c(limits_dd[1], limits_dd[3], limits_dd[1], limits_dd[4], limits_dd[2], limits_dd[4], limits_dd[2], limits_dd[3], limits_dd[1], limits_dd[3]), ncol = 2, byrow = TRUE))
    limits_dd_shp <- sp::SpatialPolygons(list(sp::Polygons(list(limits_dd_shp), ID = "clip_boundary")), proj4string= sp::CRS(proj_deg))
    limits_utm_shp <- sp::spTransform(limits_dd_shp, sp::CRS(proj_utm))
    limits_utm <- unname(c(limits_utm_shp@bbox[1,], limits_utm_shp@bbox[2,]))
    
    bound_utm_shp <- sp::Polygon(matrix(c(limits_utm[1], limits_utm[3], limits_utm[1], limits_utm[4], limits_utm[2], limits_utm[4], limits_utm[2], limits_utm[3], limits_utm[1], limits_utm[3]), ncol = 2, byrow = TRUE))
    bound_utm_shp <- sp::SpatialPolygons(list(sp::Polygons(list(bound_utm_shp), ID = "clip_boundary")), proj4string = sp::CRS(proj_utm))
    bound_dd_shp <- sp::spTransform(bound_utm_shp, sp::CRS(proj_deg))
    
    ## Boundaries (data.frames specifying each corner)
    
    bla <- bound_utm_shp@polygons[[1]]@Polygons[[1]]@coords
    bla <- as.data.frame(bla)
    bla <- bla[-nrow(bla),]
    colnames(bla) <- c("lon", "lat")
    row.names(bla) <- c("SE", "NE", "NW", "SW")
    bound_utm <- bla
    
    bla <- bound_dd_shp@polygons[[1]]@Polygons[[1]]@coords
    bla <- as.data.frame(bla)
    bla <- bla[-nrow(bla),]
    colnames(bla) <- c("lon", "lat")
    row.names(bla) <- c("SE", "NE", "NW", "SW")
    bound_dd <- bla
    
   } else {
     stop("basemap_limits requires either a shapefile as character or a numeric vector.")
   }
  }
    
    lims <- list(limits_og = limits, limits_dd = limits_dd, limits_utm = limits_utm, bound_dd = bound_dd, bound_utm = bound_utm, bound_utm_shp = bound_utm_shp, proj_deg = proj_deg, proj_utm = proj_utm)
    class(lims) <- "basemapLimits"
    
    lims
}