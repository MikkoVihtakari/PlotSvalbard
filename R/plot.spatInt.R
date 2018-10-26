##' @title Plot an interpolated map
##' @description Plot method for \code{\link[=interpolate]{spatInt}} objects.
##' @param x \code{spatInt} object from \code{\link{interpolate}} function.
##' @param basemap Logical. Should basemap from \code{\link{basemap}} be used? Not implemented
##' @param type Type of basemap. See \code{\link{basemap}}. Used only if \code{basemap = TRUE}.
##' @param limits Map limits. See \code{\link{basemap}}. The option "auto" (default) limits the map using coordinate range in \code{x}. Replace by \code{NULL} to remove the automatic zooming. Alternatively use a numeric vector as described in \code{\link{basemap}} documentation.
##' @param col.scale.limits A numeric vector of lenght 2 defining the limits for color scale of interpolated results. If \code{NULL} (default), the limits will be generated automatically.
##' @param legend.label Label for color legend. If NA (default), the labels are extracted from the \code{spatInt} object.
##' @param limits.lon,limits.lat,round.lon,round.lat,n.lon.grid,n.lat.grid,land.size,land.col,gla.col,grid.col,land.border.col,gla.size,gla.border.col,grid.size,base_size,keep.glaciers See \code{\link{basemap}} documentation.
##' @param ... Additional arguments. Required by R build checks. Ignore.
##' @method plot spatInt
##' @seealso \code{\link{interpolate}} \code{\link{basemap}}
##' @examples data(chlorophyll) ## load an example dataset
##' x <- interpolate(chlorophyll, Subset = "From <= 10", value = "Chla") ## Interpolate
##' plot(x, type = "kongsfjorden") ## Plot
##'
##' ## PlotSvalbard functions can be expanded by using ggplot2 syntax
##' plot(x, type = "kongsfjorden") + geom_text(data = chlorophyll,
##' aes(x = lon.utm, y = lat.utm, label = Station))
##'
##' ## Changing limits is easiest using the inbuild argument
##' plot(x, type = "kongsfjorden", limits = c(11.4,12.7,78.85,79.05))
##' @author Mikko Vihtakari
##' @import ggplot2
##' @export

## Test parameters
#x = X; basemap = TRUE; type = "kongsfjorden"; col.scale.limits = NULL; legend.label = NA; land.col = "#eeeac4"; gla.col = "grey95"; grid.col = "grey70"; x.lim = NULL; y.lim = NULL

plot.spatInt <- function(x, basemap = TRUE, type = "svalbard", limits = "auto", limits.lon = 0.001, limits.lat = 0.001, col.scale.limits = NULL, legend.label = NA, round.lat = FALSE, n.lat.grid = 3, round.lon = FALSE, n.lon.grid = 3, keep.glaciers = TRUE, land.col = "#eeeac4", land.size = 0.1, land.border.col = "black", gla.col = "grey95", gla.size = 0.1, gla.border.col = "black", grid.col = "grey70", grid.size = 0.1, base_size = 11, ...) {

if(basemap) {

  if(length(limits) == 1) {
  if(limits == "auto") {

    limits_utm <- c(min(x$interpolation$Lon), max(x$interpolation$Lon), min(x$interpolation$Lat), max(x$interpolation$Lat))

    bound_utm_shp <- sp::Polygon(matrix(c(limits_utm[1], limits_utm[3], limits_utm[1], limits_utm[4], limits_utm[2], limits_utm[4], limits_utm[2], limits_utm[3], limits_utm[1], limits_utm[3]), ncol = 2, byrow = TRUE))
    bound_utm_shp <- sp::SpatialPolygons(list(sp::Polygons(list(bound_utm_shp), ID = "clip_boundary")), proj4string = sp::CRS(map_projection(type)))
    tmp <- sp::spTransform(bound_utm_shp, sp::CRS(map_projection("decimal_degree")))
    tmpb <- as.data.frame(tmp@bbox)

    #tmp.lim <- transform_coord(lon = c(min(x$interpolation$Lon), max(x$interpolation$Lon)), lat = c(min(x$interpolation$Lat), max(x$interpolation$Lat)), proj.og = map_projection(type), proj.out = "+proj=longlat +datum=WGS84")

  limits <- c(tmpb$min[1], tmpb$max[1], tmpb$min[2], tmpb$max[2])

  #limits <- c(round_any(tmpb$min[1], limits.lon, floor), round_any(tmpb$max[1], limits.lon, ceiling), round_any(tmpb$min[2], limits.lat, floor), round_any(tmpb$max[2], limits.lat, ceiling))

  #limits <- c(round_any(tmp.lim$lon.utm[1], limits.lon, floor), round_any(tmp.lim$lon.utm[2], limits.lon, ceiling), round_any(tmp.lim$lat.utm[1], limits.lat, floor), round_any(tmp.lim$lat.utm[2], limits.lat, ceiling))

  }} else if(length(limits) == 3 & is.character(limits)) {
    limits <- c(round_any(min(get(limits[1])[limits[2]]), limits.lon, floor), round_any(max(get(limits[1])[limits[2]]), limits.lon, ceiling), round_any(min(get(limits[1])[limits[3]]), limits.lat, floor), round_any(max(get(limits[1])[limits[3]]), limits.lat, ceiling))
  }


X <- eval(parse(text=paste(map_cmd("base_dat"))))

if(is.na(legend.label)) {
  if(!is.null(x$variables$unit)) {
  legend.label <- paste0(x$variables$interpolated.variable, " (", x$variables$unit, ")")
} else {
  legend.label <- paste0(x$variables$interpolated.variable)
}}

if(keep.glaciers) {

  eval(parse(text=paste(map_cmd("base"), map_cmd("interpl_surface"), map_cmd("land_utm"), map_cmd("glacier_utm"), map_cmd("grid_utm"), map_cmd("defs_interpl_utm"), sep = "+")))

  } else {

     eval(parse(text=paste(map_cmd("base"), map_cmd("interpl_surface"), map_cmd("land_utm"), map_cmd("grid_utm"), map_cmd("defs_interpl_utm"), sep = "+")))

    }

  } else stop("basemap = FALSE is not implemented yet")
}

