##' @title Plot interpolated map of Kongsfjorden
##' @description Plot method for \code{\link[=interpolate]{spatInt}} objects.
##' @param x \code{spatInt} object from \code{\link{interpolate}} function.
##' @param basemap Logical. Should basemap from \code{\link{basemap}} be used? Not implemented
##' @param type Type of basemap. See \code{\link{basemap}}. Used only if \code{basemap = TRUE}.
##' @param col.scale.lims A numeric vector of lenght 2 defining the limits for color scale of interpolated results. If \code{NULL} (default), the limits will be generated automatically.
##' @param legend.label Label for color legend. If NA (default), the labels are extracted from the \code{spatInt} object.
##' @param limits Map limits. See \code{\link{basemap}}
##' ##' @param round.lat specifying the level of rounding to be used to plot latitude grid lines. Overrides \code{n.lat.grid}
##' @param round.lon specifying the level of rounding to be used to plot longitude grid lines. Overrides \code{n.lon.grid}
##' @param n.lat.grid number of latitude grid lines. Alternatively use \code{round.lat}
##' @param n.lon.grid number of longitude grid lines. Alternatively use \code{round.lon}
##' @param ... Additional arguments passed somewhere.
##' @seealso \code{\link{interpolate}}
##' @examples data(chlorophyll) ## load an example dataset
##' x <- interpolate(chlorophyll, Subset = "From <= 10", value = "Chla") ## Interpolate
##' plot(x, type = "kongsfjorden") ## Plot
##'
##' ## PlotSvalbard functions can be expanded by using ggplot2 syntax
##' library(ggplot2)
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
# basemap = TRUE
# type = "kongsfjorden"
# col.scale.limits = NULL
# legend.label = NA
# land.col = "#eeeac4"
# gla.col = "grey95"
# grid.col = "grey70"
# x.lim = NULL
# y.lim = NULL

plot.spatInt <- function(x, basemap = TRUE, type = "kongsfjorden", col.scale.limits = NULL,
legend.label = NA, limits = NULL, round.lat = FALSE, n.lat.grid = 3, round.lon = FALSE, n.lon.grid = 3, land.col = "#eeeac4", gla.col = "grey95", grid.col = "grey70", ...) {

if(basemap) {

X <- basemap_data(type = type, limits = limits, round.lat. = round.lat, n.lat.grid. = n.lat.grid, round.lon. = round.lon, n.lon.grid. = n.lon.grid)

if(is.na(legend.label)) legend.label <- paste0(x$variables$interpolated.variable, " (", x$variables$unit, ")")

basemap(type = type, limits = limits) + geom_tile(data = x$interpolation, aes(x = Lon, y = Lat, fill = var1.pred)) + geom_contour(data = x$interpolation, aes(x = Lon, y = Lat, z = var1.pred), color = "black", size = 0.2) + geom_polygon(data=X$Land, aes(x=long, y=lat, group = group), fill = land.col, color = "black", size = 0.1) + geom_polygon(data=X$Glacier, aes(x=long, y=lat, group = group), fill = gla.col, color = "black", size = 0.1) + geom_polygon(data=X$Holes, aes(x=long, y=lat, group = group), fill = land.col, color = "black", size = 0.1) + geom_line(data=X$Grid$lat, aes(x=long.utm, y=lat.utm, group = ID), color = grid.col, size = 0.1) + geom_point(data = x$data, aes(x = Lon, y = Lat)) + scale_fill_gradientn(name = legend.label, colours = colorRamps::matlab.like(7), limits = col.scale.limits) + theme(axis.title.x = element_blank(), axis.title.y = element_blank())

#+ coord_fixed(xlim = x.lim, ylim = y.lim, expand = FALSE)

  } else stop("basemap = FALSE is not implemented yet")
}

