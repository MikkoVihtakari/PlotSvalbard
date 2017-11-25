##' @title Create a ggplot2 basemap for plotting variables
##' @description Creates a ggplot2 basemap for further plotting of variables.
##' @param type Type of map area. Options: "svalbard", "mosj", "kongsfjorden", "kongsfjordbotn" or "kronebreen". See details.
##' @param limits Map limits. A numeric vector of length 4 where first element defines the minimum longitude, second element the maximum longitude, third element the minimum latitude and fourth element the maximum latitude of the bounding box. The coordinates have to be given as decimal degrees.
##' @param land.col Color of land
##' @param gla.col Color of glaciers
##' @param grid.col Color of grid lines. Use \code{NA} to remove the grid lines.
##' @param round.lat specifying the level of rounding to be used to plot latitude grid lines. Overrides \code{n.lat.grid}
##' @param round.lon specifying the level of rounding to be used to plot longitude grid lines. Overrides \code{n.lon.grid}
##' @param n.lat.grid number of latitude grid lines. Alternatively use \code{round.lat}
##' @param n.lon.grid number of longitude grid lines. Alternatively use \code{round.lon}
##' @param  ... Additional arguments passed to \code{\link{deg_grid}}.
##' @return Returns a \link[ggplot2]{ggplot2} map, which can be assigned to an object and modified as any ggplot object.
##' @details The function uses \link[ggplot2]{ggplot2} and up-to-date (2017) detailed shapefiles to plot maps of Svalbard (currently only Kongsfjorden region). The map type is defined using the \code{type} argument and map limits can be controlled with the \code{limits} argument. Currently implemented map \code{type}s:
##' \itemize{
##' \item "svalbard". Detailed 1:250000 map of Svalbard land and glaciers. This option is slow due to the large file size.
##' \item "mosj" shows Kongsfjoden and Fram Strait as sampled during Norwegian Polar Institute's MOSJ campaigns. Some glaciers can be older than 2015.
##' \item "kongsfjorden" shows Kongsfjorden and parts of Prins Karls Forland. Glaciers are from 2015 to 2017.
##' \item "kongsfjordbotn" shows Kongsvegen, Kronebreen, Kongsbreen and Conwaybreen. Glaciers are from July 2017.
##' \item "kronebreen" shows mostly Kronebreen and Kongsvegen. Glacier fronts are from July 2017.
##' }
##' @source All shape files originate from the Norwegian Polar Institute (\url{http://geodata.npolar.no/}).
##' @examples basemap() ## Plots Kongsfjorden
##'
##' ## Maps work as normal ggplot2 objects:
##'
##' library(ggplot2)
##' data(chlorophyll)
##' p <- basemap("mosj")
##' p + geom_point(data = chlorophyll, aes(x = lon.utm, y = lat.utm,
##' size = Chla, color = Chla), shape = 1)
##'
##' ## limitting maps is possible, but you might need to define grid lines by hand:
##' basemap("kongsfjordbotn", limits = c(12.2,12.65,78.95,79.00))
##' basemap("kongsfjordbotn", limits = c(12.2,12.65,78.95,79.00),
##' Round.lat = 0.01, Round.lon = 0.1) # better
##'
##' ## Svalbard map. Warning: this is SLOW
##' basemap("svalbard")
##' basemap("svalbard", grid.col = NA, limits = c(10, 28, 79.5, 83)) # Removes grid lines
##'
##' @seealso \code{\link[ggplot2]{ggplot2}} \code{\link{theme_map}}
##' @author Mikko Vihtakari
##' @import ggplot2
##' @export

# type = "kongsfjordbotn"
# land.col = "#eeeac4"
# gla.col = "grey95"
# grid.col = "grey70"
# limits = NULL
# limits = c(12.2,12.65,78.95,79.00)
# round.lat = FALSE
# n.lat.grid = 3
# round.lon = FALSE
# n.lon.grid = 3

basemap <- function(type = "kongsfjorden", land.col = "#eeeac4", gla.col = "grey95", grid.col = "grey70", limits = NULL, round.lat = FALSE, n.lat.grid = 3, round.lon = FALSE, n.lon.grid = 3, ...) {

  X <- basemap_data(type = type, limits = limits, round.lat. = round.lat, n.lat.grid. = n.lat.grid, round.lon. = round.lon, n.lon.grid. = n.lon.grid)

ggplot(data=X$Land, aes(x=long, y=lat)) + geom_polygon(data = X$Land, aes(x = long, y = lat, group = group), fill = land.col, color = "black", size = 0.1) + geom_polygon(data = X$Glacier, aes(x = long, y = lat, group = group), fill = gla.col, color = "black", size = 0.1) + geom_polygon(data = X$Holes, aes(x=long, y=lat, group = group), fill = land.col, color = "black", size = 0.1) + geom_line(data = X$Grid$lat, aes(x = lon.utm, y=lat.utm, group = ID), color = grid.col, size = 0.1) + geom_line(data = X$Grid$lon, aes(x=lon.utm, y=lat.utm, group = ID), color = grid.col, size = 0.1) + scale_x_continuous(name = "Longitude (decimal degrees)", breaks = X$Grid$lon.breaks$utm, labels = X$Grid$lon.breaks$deg) + scale_y_continuous(name = "Latitude (decimal degrees)", breaks = X$Grid$lat.breaks$utm, labels = X$Grid$lat.breaks$deg) + coord_fixed(xlim = c(X$Grid$boundaries$lon.utm[1], X$Grid$boundaries$lon.utm[2]), ylim = c(X$Grid$boundaries$lat.utm[1], X$Grid$boundaries$lat.utm[2]), expand = FALSE) + theme_map()

}


map_type <- function(type) {
  switch (type,
  mosj = list(Land = mosj.ld, Glacier = mosj.gl, Boundary = mosj.cr, round.lon = 1, round.lat = 0.2),
  kongsfjorden = list(Land = kong.ld, Glacier = kong.gl, Boundary = kong.cr, round.lon = 0.5, round.lat = 0.1),
  kongsfjordbotn = list(Land = kong.ld, Glacier = kong.gl, Boundary = c(12.2,12.65,78.855,79.00), round.lon = 0.1, round.lat = 0.05),
  kronebreen = list(Land = kong.ld, Glacier = kong.gl, Boundary = c(12.32,12.62,78.855,78.91), round.lon = 0.1, round.lat = 0.02),
  svalbard = list(Land = svalbard.ld, Glacier = svalbard.gl, Boundary = c(10,28,76,81), round.lon = 2, round.lat = 1),
  stop(paste("type argument", type, "is not implemented."))
)
}

basemap_data <- function(type, limits, round.lat. = round.lat, n.lat.grid. = n.lat.grid, round.lon. = round.lon, n.lon.grid. = n.lon.grid) {

  Land <- map_type(type)$Land
  Glacier <- map_type(type)$Glacier
  Boundary <- map_type(type)$Boundary

  if(!is.null(limits)) {
    if(length(limits) != 4 | !is.numeric(limits)) stop("limits have to be a numeric vector of length 4. See Arguments")
      Boundary <- limits
      Grid <- deg_grid(Boundary, round.lat = round.lat., n.lat.grid = n.lat.grid., round.lon = round.lon., n.lon.grid = n.lon.grid.)
  } else {
    Grid <- deg_grid(Boundary, round.lat = map_type(type)$round.lat, round.lon = map_type(type)$round.lon)
  }

  tmp <- fortify(Glacier)
  Holes <- tmp[tmp$hole == TRUE,]

list(Land = Land, Glacier = Glacier, Boundary = Boundary, Grid = Grid, Holes = Holes)
}
