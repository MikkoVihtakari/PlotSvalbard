#' @title Create basemapData object for basemap plotting
#' @description Internal function to create a \code{basemapData} object for \code{\link{basemap}}
#' @param type See \code{\link{basemap}}
#' @param limits See \code{\link{basemap}}
#' @param round.lat. See \code{\link{basemap}}
#' @param n.lat.grid. See \code{\link{basemap}}
#' @param round.lon. See \code{\link{basemap}}
#' @param n.lon.grid. See \code{\link{basemap}}
#' @param lat.interval. See \code{\link{basemap}}
#' @param lon.interval. See \code{\link{basemap}}
#' @param expar Expansion parameter for \code{\link{deg_grid}}
#' @param clip Logical. Should shapefiles be clipped before plotting?
#' @param keep.glaciers. See \code{\link{basemap}}
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function. Common users do not need to worry about these details.
#' @keywords internal
#' @export
#' @import sp
#' @importFrom broom tidy
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}} \code{\link{deg_grid}} \code{\link{deg_grid_polar}}

## Test parameters
# basemap_data("svalbard", limits = c(12.2,12.65, 78.855,79))
# land = "barents.ld"
# glacier = "svalbard.gl"
# glacier = NULL
# map.type <- "barents" # "svalbard" "kongsfjorden" "panarctic"
# boundary = c(19.5,23.5,80,81.7)
# limits = c(19.5,23.5,80,81.7)
# round.lon = 0.5
# round.lat = 0.1
# keep.glaciers = TRUE
# type = "kongsfjorden"
# limits = NULL
# map_type("barentssea")
# expar = 0.3
# type = "arctic50"
# type = "arctic60"
# limits = NULL; expar = 0.1; clip = FALSE; keep.glaciers. = FALSE; lat.interval. = 10; lon.interval. = 45
# round.lat. = round.lat; n.lat.grid. = n.lat.grid; round.lon. = round.lon; n.lon.grid. = n.lon.grid; lat.interval. = lat.interval; lon.interval. = lon.interval; expar = 0.1; clip = FALSE; keep.glaciers. = keep.glaciers
basemap_data <- function(type, limits = NULL, round.lat. = round.lat, n.lat.grid. = n.lat.grid, round.lon. = round.lon, n.lon.grid. = n.lon.grid, lat.interval. = lat.interval, lon.interval. = lon.interval, expar = 0, clip = TRUE, keep.glaciers. = keep.glaciers) {

  ## bound = clip boundary for shapes
  ## limits = limits for map
  ## dd = in decimal degrees
  ## utm = in utm coordinates
  ## shp = as shape file
  MapType <- map_type(type)

  if(MapType$map.type == "panarctic") {
      if(!is.null(limits)) lims <- limits
  } else if(!is.null(limits)) {
    if(length(limits) != 4 | !is.numeric(limits)) stop("limits have to be a numeric vector of length 4. See Arguments")
    lims <- basemap_limits(limits = limits, type = MapType$map.type)
  } else {
    lims <- basemap_limits(limits = MapType$boundary, type = MapType$map.type) 
  }

  if(MapType$map.type == "svalbard") {
    Land <- clip_shapefile(get(MapType$land), lims$bound_utm_shp)
   # Land <- fortify(Land)
    Land <- broom::tidy(Land)
  } else {
    Land <- get(MapType$land)
  }


  if(any(is.null(MapType$glacier), !keep.glaciers., MapType$map.type == "barents", MapType$map.type == "panarctic")) {
    Glacier <- NULL
    Holes <- NULL
  } else {
    Glacier <- get(MapType$glacier)
    Glacier <- clip_shapefile(Glacier, lims$bound_utm_shp)
    Glacier <- broom::tidy(Glacier)
    Holes <- Glacier[Glacier$hole == TRUE,]
  }

  if(MapType$map.type == "panarctic") {

    if(!is.null(limits)) {
      Grid <- deg_grid_polar(dat = lims, lat.interval = lat.interval., lon.interval = lon.interval.)
    } else {
      Grid <- deg_grid_polar(dat = Land, lat.interval = lat.interval., lon.interval = lon.interval.)
    }
    Land <- broom::tidy(Land)
    
  } else if(!is.null(limits)) {
    Grid <- deg_grid(lims, round.lat = round.lat., n.lat.grid = n.lat.grid., round.lon = round.lon., n.lon.grid = n.lon.grid.)
   
  } else {
    
    Grid <- deg_grid(lims, round.lat = MapType$round.lat, round.lon = MapType$round.lon)
  }

out <- list(Land = Land, Glacier = Glacier, Boundary = MapType$boundary, Grid = Grid, Holes = Holes, MapType = type, MapClass = map_type(type)$map.type)

class(out) <- "basemapData"

out
}
