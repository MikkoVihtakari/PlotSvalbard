##' @title Plot an interpolated map
##' @description Plot method for \code{\link[=interpolate]{spatInt}} objects.
##' @param x \code{spatInt} object from \code{\link{interpolate}} function.
##' @param basemap Logical. Should basemap from \code{\link{basemap}} be used? Not implemented
##' @param type Type of basemap. See \code{\link{basemap}}. Used only if \code{basemap = TRUE}.
##' @param col.scale.limits A numeric vector of lenght 2 defining the limits for color scale of interpolated results. If \code{NULL} (default), the limits will be generated automatically.
##' @param legend.label Label for color legend. If NA (default), the labels are extracted from the \code{spatInt} object.
##' @param limits Map limits. See \code{\link{basemap}}
##' @param round.lat specifying the level of rounding to be used to plot latitude grid lines. Overrides \code{n.lat.grid}
##' @param round.lon specifying the level of rounding to be used to plot longitude grid lines. Overrides \code{n.lon.grid}
##' @param n.lat.grid number of latitude grid lines. Alternatively use \code{round.lat}
##' @param n.lon.grid number of longitude grid lines. Alternatively use \code{round.lon}
##' @param land.col Color of land
##' @param gla.col Color of glaciers
##' @param grid.col Color of grid lines. Use \code{NA} to remove the grid lines.
##' @param keep.glaciers a logical indicating whether glaciers should be kept for the Svalbard maps. Setting this to \code{FALSE} speeds up map plotting little bit.
##' @param size.land,border.col.land,size.glacier,border.col.glacier,size.grid,base_size See \code{\link{basemap}}
##' @param ... Additional arguments passed somewhere.
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
##' @importFrom colorRamps matlab.like
##' @export

## Test parameters
#x = X; basemap = TRUE; type = "kongsfjorden"; col.scale.limits = NULL; legend.label = NA; land.col = "#eeeac4"; gla.col = "grey95"; grid.col = "grey70"; x.lim = NULL; y.lim = NULL

plot.spatInt <- function(x, basemap = TRUE, type = "kongsfjorden", col.scale.limits = NULL, legend.label = NA, limits = NULL, round.lat = FALSE, n.lat.grid = 3, round.lon = FALSE, n.lon.grid = 3, keep.glaciers = TRUE, land.col = "#eeeac4", size.land = 0.1, border.col.land = "black", gla.col = "grey95", size.glacier = 0.1, border.col.glacier = "black", grid.col = "grey70", size.grid = 0.1, base_size = 11, ...) {

if(basemap) {

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

