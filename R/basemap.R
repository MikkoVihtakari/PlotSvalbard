##' @title Create a ggplot2 basemap for plotting variables
##' @description Creates a ggplot2 basemap for further plotting of variables.
##' @param type Type of map area. Options: "svalbard", "mosj", "kongsfjorden", "kongsfjordbotn", "kronebreen" or "barentssea". See details.
##' @param limits Map limits. A numeric vector of length 4 where first element defines the minimum longitude, second element the maximum longitude, third element the minimum latitude and fourth element the maximum latitude of the bounding box. The coordinates have to be given as decimal degrees.
##' @param land.col Color of land
##' @param gla.col Color of glaciers
##' @param grid.col Color of grid lines. Use \code{NA} to remove the grid lines.
##' @param round.lat specifying the level of rounding to be used to plot latitude grid lines. Overrides \code{n.lat.grid}
##' @param round.lon specifying the level of rounding to be used to plot longitude grid lines. Overrides \code{n.lon.grid}
##' @param n.lat.grid number of latitude grid lines. Alternatively use \code{round.lat}
##' @param n.lon.grid number of longitude grid lines. Alternatively use \code{round.lon}
##' @param keep.glaciers a logical indicating whether glaciers should be kept for the Svalbard maps. Setting this to \code{FALSE} speeds up map plotting by a few seconds.
##' @param border.col.land color of the border line for land shapes.
##' @param border.col.glacier color of the border line for glacier shapes.
##' @param size.land width of the border line for land shapes. See details for explanation about line widths.
##' @param size.glacier width of the border line for glacier shapes.
##' @param size.grid width of the grid lines.
##' @return Returns a \link[ggplot2]{ggplot2} map, which can be assigned to an object and modified as any ggplot object.
##' @details The function uses \link[ggplot2]{ggplot2} and up-to-date (2017) detailed shapefiles to plot maps of Svalbard and other polar regions. The map type is defined using the \code{type} argument and map limits can be controlled with the \code{limits} argument. Currently implemented map \code{type}s:
##' \itemize{
##' \item "svalbard". Detailed 1:250 000 map of Svalbard land and glaciers. This option is slow (approx. 25 seconds) due to the large file size.
##' \item "mosj" shows Kongsfjoden and Fram Strait as sampled during Norwegian Polar Institute's MOSJ campaigns. Some glaciers can be older than 2015.
##' \item "kongsfjorden" shows Kongsfjorden and parts of Prins Karls Forland. Glaciers are from 2015 to 2017. This map uses a subset of the Svalbard shape files and is faster to plot due to smaller file size.
##' \item "kongsfjordbotn" shows Kongsvegen, Kronebreen, Kongsbreen and Conwaybreen. Glaciers are from July 2017. This map uses a subset of the Svalbard shape files and is faster to plot due to a smaller file size.
##' \item "kronebreen" shows mostly Kronebreen and Kongsvegen. Glacier fronts are from July 2017. This map uses a subset of the Svalbard shape files and is faster to plot due to a smaller file size.
##' \item "barentssea". A 1:10 000 000 map of the Barents Sea. See \strong{Source}.
##' }
##'
##' All maps are in \code{"+init=epsg:32633"} UTM projection at the momemnt. More maps can be added based on need.
##'
##' \strong{Line width} (size) aesthatics in \link[ggplot2]{ggplot2} generetes approximately 2.13 wider lines measured in pt than the given values. If you want a specific line width in pt, multiply it by 1/2.13.
##'
##' @source Svalbard shape files originate from the Norwegian Polar Institute (\url{http://geodata.npolar.no/}). Barents Sea map is downloaded from  \url{http://www.naturalearthdata.com} and uses the \code{ne_10m_land} (v 4.0.0) dataset.
##' @examples basemap() ## Plots Kongsfjorden
##'
##' ## Maps work as normal ggplot2 objects:
##'
##' data(chlorophyll)
##' p <- basemap("mosj")
##' p + geom_point(data = chlorophyll, aes(x = lon.utm, y = lat.utm,
##' size = Chla, color = Chla), shape = 1)
##'
##' ## limitting maps is possible, but you might need to define grid lines by hand:
##' basemap("kongsfjordbotn", limits = c(12.2,12.65,78.95,79.00))
##' basemap("kongsfjordbotn", limits = c(12.2,12.65,78.95,79.00),
##' round.lat = 0.01, round.lon = 0.1) # better
##'
##' ## Svalbard map. Warning: this is SLOW
##' basemap("svalbard")
##'
##' ## grid.col = NA removes grid lines
##' basemap("svalbard", grid.col = NA, limits = c(10, 28, 79.5, 83))
##'
##' ## Barents Sea
##' basemap("barentssea") # note wrong placing of axis labels
##'
##' ## Barents Sea map also prints mainland Norway,
##' ## but the projection is not optimal.
##' basemap("barentssea", limits = c(12, 24, 68, 71))
##'
##' @seealso \code{\link[ggplot2]{ggplot2}} \code{\link{theme_map}}
##'
##' \code{coastlineWorldMedium} from the \code{oce} package for plotting maps in maps in base graphics using the \code{ne_10m_land} dataset.
##' @author Mikko Vihtakari
##' @import ggplot2
##' @importFrom grDevices extendrange
##' @export

# type = "barentssea"
# land.col = "#eeeac4"
# gla.col = "grey95"
# grid.col = "grey70"
# limits = NULL
# limits = c(12.2,12.65,78.95,79.00)
# round.lat = FALSE
# n.lat.grid = 3
# round.lon = FALSE
# n.lon.grid = 3
# keep.glaciers = TRUE
# land.size = 0.1

basemap <- function(type = "kongsfjorden", land.col = "#eeeac4", gla.col = "grey95", grid.col = "grey70", limits = NULL, round.lat = FALSE, n.lat.grid = 3, round.lon = FALSE, n.lon.grid = 3, keep.glaciers = TRUE, size.land = 0.1, size.glacier = 0.1, size.grid = 0.1, border.col.land = "black", border.col.glacier = "black") {

  X <- eval(parse(text=paste(map_cmd("base_dat"))))

  if(keep.glaciers) {
    eval(parse(text=paste(map_cmd("land.utm"), map_cmd("glacier.utm"), map_cmd("grid.utm"), map_cmd("defs.utm"), sep = "+")))
  } else {
    eval(parse(text=paste(map_cmd("land.utm"), map_cmd("grid.utm"), map_cmd("defs.utm"), sep = "+")))
  }

}

map_cmd <- function(command) {
  switch(command,
    base_dat = 'basemap_data(type = type, limits = limits, round.lat. = round.lat, n.lat.grid. = n.lat.grid, round.lon. = round.lon, n.lon.grid. = n.lon.grid, keep.glaciers. = keep.glaciers)',
    land.utm = 'ggplot(data=X$Land, aes(x=long, y=lat)) + geom_polygon(data = X$Land, aes(x = long, y = lat, group = group), fill = land.col, color = border.col.land, size = size.land)',
    glacier.utm = 'geom_polygon(data = X$Glacier, aes(x = long, y = lat, group = group), fill = gla.col, color = border.col.glacier, size = size.glacier) + geom_polygon(data = X$Holes, aes(x=long, y=lat, group = group), fill = land.col, color = border.col.glacier, size = size.land)',
    grid.utm = 'geom_line(data = X$Grid$lat, aes(x = lon.utm, y=lat.utm, group = ID), color = grid.col, size = size.grid) + geom_line(data = X$Grid$lon, aes(x=lon.utm, y=lat.utm, group = ID), color = grid.col, size = size.grid)',
    defs.utm = 'scale_y_continuous(name = "Latitude (decimal degrees)", breaks = X$Grid$lat.breaks$utm, labels = X$Grid$lat.breaks$deg) + scale_x_continuous(name = "Longitude (decimal degrees)", breaks = X$Grid$lon.breaks$utm, labels = X$Grid$lon.breaks$deg) + coord_fixed(xlim = c(X$Grid$boundaries$lon.utm[1], X$Grid$boundaries$lon.utm[2]), ylim = c(X$Grid$boundaries$lat.utm[1], X$Grid$boundaries$lat.utm[2]), expand = FALSE) + theme_map()',
    stop(paste("map command", command, "not found."))
)
}

map_type <- function(type) {
  switch (type,
  mosj = list(land = "svalbard.ld", glacier = "svalbard.gl", boundary = "mosj.cr", map.type = "kongsfjorden", round.lon = 1, round.lat = 0.2),
  kongsfjorden = list(land = "kong.ld", glacier = "kong.gl", boundary = "kong.cr", map.type = "kongsfjorden", round.lon = 0.5, round.lat = 0.1),
  kongsfjordbotn = list(land = "kong.ld", glacier = "kong.gl", boundary = c(12.2,12.65,78.855,79.00), map.type = "kongsfjorden", round.lon = 0.1, round.lat = 0.05),
  kronebreen = list(land = "kong.ld", glacier = "kong.gl", boundary = c(12.32,12.62,78.855,78.91), map.type = "kongsfjorden", round.lon = 0.1, round.lat = 0.02),
  svalbard = list(land = "svalbard.ld", glacier = "svalbard.gl", boundary = c(10,28,76,81), map.type = "svalbard", round.lon = 2, round.lat = 1),
  rijpfjorden = list(land = "svalbard.ld", glacier = NULL, boundary = c(19.5,23.5,80,81.7), map.type = "svalbard", round.lon = 0.5, round.lat = 0.4),
  barentssea = list(land = "barents.ld", glacier = NULL, boundary = c(0,50,70,83), map.type = "barents", round.lon = 4, round.lat = 2),
  stop(paste("type argument", type, "is not implemented."))
)
}

map_projection <- function(map.type) {
  switch(map.type,
    kongsfjorden = "+init=epsg:32633",
    svalbard = "+init=epsg:32633",
    barents = "+init=epsg:32633",
    stop("map.type not found"))
}

## Test parameters
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

basemap_data <- function(type, limits = NULL, round.lat. = round.lat, n.lat.grid. = n.lat.grid, round.lon. = round.lon, n.lon.grid. = n.lon.grid, expar = 0.1, clip = FALSE, keep.glaciers. = keep.glaciers) {

  MapType <- map_type(type)

  if(!is.null(limits)) {
    if(length(limits) != 4 | !is.numeric(limits)) stop("limits have to be a numeric vector of length 4. See Arguments")
      boundary <- c(extendrange(limits[1:2], f = expar), extendrange(limits[3:4], f = expar))
  } else {
    if(is.numeric(MapType$boundary)) {
      if(length(MapType$boundary) != 4) stop("limits have to be a numeric vector of length 4. See Arguments")
      boundary <- c(extendrange(MapType$boundary[1:2], f = expar), extendrange(MapType$boundary[3:4], f = expar))
    } else {
      lims <- get(MapType$boundary)
      lims <- spTransform(lims, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))@bbox
      boundary <- c(extendrange(lims[1,], f = expar), extendrange(lims[2,], f = expar))
    }}

  if(clip) {
    Land <- clip_shapefile(get(MapType$land), boundary)
  } else {
    Land <- get(MapType$land)
  }


  if(any(is.null(MapType$glacier), !keep.glaciers., MapType$map.type == "barents", MapType$map.type == "panarctic")) {
    Glacier <- NULL
    Holes <- NULL
  } else {
    Glacier <- get(MapType$glacier)
    tmp <- fortify(Glacier)
    Holes <- tmp[tmp$hole == TRUE,]
  }

  if(!is.null(limits)) {
    Grid <- deg_grid(limits, round.lat = round.lat., n.lat.grid = n.lat.grid., round.lon = round.lon., n.lon.grid = n.lon.grid.)
  } else {
    if(is.character(MapType$boundary)) {
      lims <- get(MapType$boundary)
    } else {
      lims <- MapType$boundary
    }
    Grid <- deg_grid(lims, round.lat = MapType$round.lat, round.lon = MapType$round.lon)
  }

list(Land = Land, Glacier = Glacier, Boundary = MapType$boundary, Grid = Grid, Holes = Holes)
}
