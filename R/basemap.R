##' @title Create a ggplot2 basemap for plotting variables
##' @description Creates a ggplot2 basemap for further plotting of variables.
##' @param type Type of map area. Options: "svalbard", "mosj", "kongsfjorden", "kongsfjordbotn", "kronebreen", "barentssea", "arctic50" or "arctic60". See details.
##' @param limits Map limits. A numeric vector of length 4 where first element defines the minimum longitude, second element the maximum longitude, third element the minimum latitude and fourth element the maximum latitude of the bounding box. The coordinates have to be given as decimal degrees for Svalbard and Barents Sea maps and as UTM coordinates for pan-Arctic maps. See "Examples", \code{\link{map_projection}} and \code{\link{transform_coord}} how to find these coordinates.
##' @param land.col Character code specifying the color of land.
##' @param gla.col Character code specifying the color of glaciers.
##' @param grid.col Character code specifying the color of grid lines. Use \code{NA} to remove the grid lines.
##' @param round.lat Numeric value specifying the level of rounding to be used to plot latitude grid lines. Overrides \code{n.lat.grid}
##' @param round.lon Numeric value specifying the level of rounding to be used to plot longitude grid lines. Overrides \code{n.lon.grid}
##' @param n.lat.grid Numeric value specifying the number of latitude grid lines. Alternatively use \code{round.lat}
##' @param n.lon.grid Numeric value specifying the number of longitude grid lines. Alternatively use \code{round.lon}
##' @param lat.interval Numeric value specifying the interval of latitude grids for polar stereographic maps (\code{type = "arctic50"} or \code{"arctic60"})
##' @param lon.interval Numeric value specifying the interval of longitude grids for polar stereographic maps (\code{type = "arctic50"} or \code{"arctic60"})
##' @param keep.glaciers Logical indicating whether glaciers should be kept for the Svalbard maps. Setting this to \code{FALSE} speeds up map plotting by a few seconds.
##' @param border.col.land Character code specifying the color of the border line for land shapes.
##' @param border.col.glacier Character code specifying the color of the border line for glacier shapes.
##' @param size.land Numeric value specifying the width of the border line for land shapes. See details for explanation about line widths.
##' @param size.glacier Numeric value specifying the width of the border line for glacier shapes.
##' @param size.grid Numeric value specifying the width of the grid lines.
##' @param label.print Logical indicating whether labels should be printed for polar stereographic maps.
##' @param label.font Numeric value specifying the font size for labels in polar stereographic maps. Note that this value defines the actual font size in points, not the \code{ggplot2} font size.
##' @param label.offset Offset between the round polar stereographic maps and longitude labels. Optimized for a pdf output. Use 1.1 for larger size figures.
##' @return Returns a \link[ggplot2]{ggplot2} map, which can be assigned to an object and modified as any ggplot object.
##' @details The function uses \link[ggplot2]{ggplot2} and up-to-date (2017) detailed shapefiles to plot maps of Svalbard and other polar regions. The map type is defined using the \code{type} argument and map limits can be controlled with the \code{limits} argument. Currently implemented map \code{type}s:
##' \itemize{
##' \item "svalbard". Detailed 1:250 000 map of Svalbard land and glaciers. This option is slow (approx. 25 seconds) due to the large file size.
##' \item "mosj" shows Kongsfjoden and Fram Strait as sampled during Norwegian Polar Institute's MOSJ campaigns. Some glaciers can be older than 2015.
##' \item "kongsfjorden" shows Kongsfjorden and parts of Prins Karls Forland. Glaciers are from 2015 to 2017. This map uses a subset of the Svalbard shape files and is faster to plot due to smaller file size.
##' \item "kongsfjordbotn" shows Kongsvegen, Kronebreen, Kongsbreen and Conwaybreen. Glaciers are from July 2017. This map uses a subset of the Svalbard shape files and is faster to plot due to a smaller file size.
##' \item "kronebreen" shows mostly Kronebreen and Kongsvegen. Glacier fronts are from July 2017. This map uses a subset of the Svalbard shape files and is faster to plot due to a smaller file size.
##' \item "barentssea". A 1:10 000 000 map of the Barents Sea. See \strong{Source}.
##' \item "arctic50". A polar stereographic map of the Arctic with a limit at 50 degrees North.
##' \item "arctic60". A polar stereographic map of the Arctic with a limit at 60 degrees North.
##' }
##'
##' Svalbard and Barents Sea maps use the \code{"+init=epsg:32633"} UTM projection. The polar stereographic maps use \code{"+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"} projection.
##'
##' \strong{Line width} (size) aesthatics in \link[ggplot2]{ggplot2} generetes approximately 2.13 wider lines measured in pt than the given values. If you want a specific line width in pt, multiply it by 1/2.13.
##'
##' @source Svalbard shape files originate from the Norwegian Polar Institute (\url{http://geodata.npolar.no/}). Barents Sea and polar stereographic maps are from  \url{http://www.naturalearthdata.com}. They use the \code{ne_10m_land} and \code{ne_50m_land} (v 4.0.0) datasets, respectively.
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
##' ## Polar stereographic pan-Arctic maps
##' basemap("arctic50")
##'
##' ## To find UTM coordinates to limit a pan-Arctic map:
##' basemap("arctic60") + theme_bw()
##' basemap("arctic60", limits = c(250000, -2500000, 2000000, -250000))
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
#type = "arctic60"; land.col = "#eeeac4"; gla.col = "grey95"; grid.col = "grey70"; limits = NULL; round.lat = FALSE; n.lat.grid = 3; round.lon = FALSE; n.lon.grid = 3; keep.glaciers = TRUE; size.land = 0.1; size.glacier = 0.1; size.grid = 0.1; border.col.land = "black"; border.col.glacier = "black"; lat.interval = 10; lon.interval = 45; label.font = 3; label.offset = 1.04

basemap <- function(type = "kongsfjorden", limits = NULL, round.lat = FALSE, n.lat.grid = 3, lat.interval = 10, round.lon = FALSE, n.lon.grid = 3, lon.interval = 45, keep.glaciers = TRUE, land.col = "#eeeac4", size.land = 0.1, border.col.land = "black", gla.col = "grey95", size.glacier = 0.1, border.col.glacier = "black", grid.col = "grey70", size.grid = 0.1, label.print = TRUE, label.offset = 1.05, label.font = 8) {

X <- switch(map_type(type)$map.type,
  panarctic = eval(parse(text=paste(map_cmd("base_polar")))),
  svalbard = eval(parse(text=paste(map_cmd("base_dat")))),
  barents = eval(parse(text=paste(map_cmd("base_dat")))),
  kongsfjorden = eval(parse(text=paste(map_cmd("base_dat")))),
  stop(paste("map_type for", type, "not found."))
)

if(X$MapClass %in% c("panarctic")) {

## Pan-Arctic maps ####
    if(X$Grid$limits) { ## Square maps
      if(label.print) { ## With axis labels
        eval(parse(text=paste(map_cmd("grid_polar"), map_cmd("land_polar"), map_cmd("labels_polar_limits"), map_cmd("defs_polar_limits"), sep = "+")))
      } else { ## Without axis labels
      eval(parse(text=paste(map_cmd("grid_polar"), map_cmd("land_polar"), map_cmd("defs_polar_limits"), map_cmd("remove_labels"), sep = "+")))
    }} else if(label.print) { ## Round maps, with labels
      eval(parse(text=paste(map_cmd("grid_polar"), map_cmd("land_polar"), map_cmd("labels_polar"), map_cmd("defs_polar"), sep = "+")))
    } else { ## Wihtout labels
    eval(parse(text=paste(map_cmd("grid_polar"), map_cmd("land_polar"), map_cmd("defs_polar"), sep = "+")))
    }

  } else {

## Other maps ####
  if(keep.glaciers) { ## With glaciers
    eval(parse(text=paste(map_cmd("land_utm"), map_cmd("glacier_utm"), map_cmd("grid_utm"), map_cmd("defs_utm"), sep = "+")))
  } else {
    eval(parse(text=paste(map_cmd("land_utm"), map_cmd("grid_utm"), map_cmd("defs_utm"), sep = "+")))
  }}

}

formatterUTMkm <- function(x){
    x/1000
}

map_cmd <- function(command) {
  switch(command,
    base_dat = 'basemap_data(type = type, limits = limits, round.lat. = round.lat, n.lat.grid. = n.lat.grid, round.lon. = round.lon, n.lon.grid. = n.lon.grid, keep.glaciers. = keep.glaciers)',
    land_utm = 'ggplot(data=X$Land, aes(x=long, y=lat)) + geom_polygon(data = X$Land, aes(x = long, y = lat, group = group), fill = land.col, color = border.col.land, size = size.land)',
    glacier_utm = 'geom_polygon(data = X$Glacier, aes(x = long, y = lat, group = group), fill = gla.col, color = border.col.glacier, size = size.glacier) + geom_polygon(data = X$Holes, aes(x=long, y=lat, group = group), fill = land.col, color = border.col.glacier, size = size.land)',
    grid_utm = 'geom_line(data = X$Grid$lat, aes(x = lon.utm, y=lat.utm, group = ID), color = grid.col, size = size.grid) + geom_line(data = X$Grid$lon, aes(x=lon.utm, y=lat.utm, group = ID), color = grid.col, size = size.grid)',
    defs_utm = 'scale_y_continuous(name = "Latitude (decimal degrees)", breaks = X$Grid$lat.breaks$utm, labels = X$Grid$lat.breaks$deg) + scale_x_continuous(name = "Longitude (decimal degrees)", breaks = X$Grid$lon.breaks$utm, labels = X$Grid$lon.breaks$deg) + coord_fixed(xlim = c(X$Grid$boundaries$lon.utm[1], X$Grid$boundaries$lon.utm[2]), ylim = c(X$Grid$boundaries$lat.utm[1], X$Grid$boundaries$lat.utm[2]), expand = FALSE) + theme_map()',
    base_polar = 'basemap_data(type = type, limits = limits, lat.interval. = lat.interval, lon.interval. = lon.interval, keep.glaciers. = FALSE)',
    land_polar = 'geom_polygon(data = X$Land, aes(x = long, y = lat, group = group), fill = land.col, color = border.col.land, size = size.land)',
    grid_polar = 'ggplot(data=X$Land, aes(x=long, y=lat)) + geom_path(data = X$Grid$lat, aes(x = lon.utm, y=lat.utm, group = ID), color = grid.col, size = size.grid) + geom_segment(data = X$Grid$lon, aes(x = lon.start, xend = lon.end, y = lat.start, yend = lat.end, group = label), color = grid.col, size = size.grid)',
    labels_polar = 'geom_text(data = X$Grid$lon, aes(x = label.offset*lon.end, y = label.offset*lat.end, angle = angle, label = paste(label, "^o", sep = "")), size = label.font/2.845276, parse = TRUE) + geom_text(data = X$Grid$lat.breaks, aes(x = lon.utm, y = lat.utm, label = paste(label, "^o", sep = "")), hjust = 0, vjust = 0, size = label.font/2.845276, parse = TRUE)',
    labels_polar_limits = 'scale_y_continuous(name = "UTM latitude (km)", labels = formatterUTMkm) + scale_x_continuous(name = "UTM longitude (km)", labels = formatterUTMkm)',
    defs_polar_limits = 'coord_fixed(xlim = c(X$Grid$boundaries$lon.utm[1], X$Grid$boundaries$lon.utm[2]), ylim = c(X$Grid$boundaries$lat.utm[1], X$Grid$boundaries$lat.utm[2]), expand = FALSE) + theme_map()',
    defs_polar = 'geom_path(data = X$Grid$lat[X$Grid$lat$ID == levels(X$Grid$lat$ID)[which.min(as.numeric(gsub("[[:alpha:]]", "", levels(X$Grid$lat$ID))))],], aes(x = lon.utm, y=lat.utm, group = ID), color = border.col.land, size = size.land) + coord_fixed() + theme_void()',
    remove_labels = 'theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())',
    stop(paste("map command", command, "not found."))
)
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
# type = "arctic50"
# type = "arctic60"
# limits = NULL; expar = 0.1; clip = FALSE; keep.glaciers. = FALSE; lat.interval. = 10; lon.interval. = 45

basemap_data <- function(type, limits = NULL, round.lat. = round.lat, n.lat.grid. = n.lat.grid, round.lon. = round.lon, n.lon.grid. = n.lon.grid, lat.interval. = lat.interval, lon.interval. = lon.interval, expar = 0.1, clip = FALSE, keep.glaciers. = keep.glaciers) {

  MapType <- map_type(type)

  if(MapType$map.type == "panarctic") {
      if(!is.null(limits)) lims <- limits
  } else if(!is.null(limits)) {
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

  if(MapType$map.type == "panarctic") {

    if(!is.null(limits)) {
      Grid <- deg_grid_polar(dat = lims, lat.interval = lat.interval., lon.interval = lon.interval.)
    } else {
      Grid <- deg_grid_polar(dat = Land, lat.interval = lat.interval., lon.interval = lon.interval.)
    }
  } else if(!is.null(limits)) {
    Grid <- deg_grid(limits, round.lat = round.lat., n.lat.grid = n.lat.grid., round.lon = round.lon., n.lon.grid = n.lon.grid.)
  } else {
    if(is.character(MapType$boundary)) {
      lims <- get(MapType$boundary)
    } else {
      lims <- MapType$boundary
    }
    Grid <- deg_grid(lims, round.lat = MapType$round.lat, round.lon = MapType$round.lon)
  }

list(Land = Land, Glacier = Glacier, Boundary = MapType$boundary, Grid = Grid, Holes = Holes, MapType = type, MapClass = map_type(type)$map.type)
}
