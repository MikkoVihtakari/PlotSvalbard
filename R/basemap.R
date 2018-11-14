##' @title Create a ggplot2 basemap for plotting variables
##' @description Creates a ggplot2 basemap for further plotting of variables.
##' @param type Type of map area. Options: "svalbard", "mosj", "kongsfjorden", "kongsfjordbotn", "kronebreen", "barentssea", "arctic50" or "arctic60". See details.
##' @param limits Map limits. Either a numeric vector of length 4 or a character vector of length 3. Numeric vectors are used to constrain (zoom in) the maps using coordinates. Character vectors are used to automatically zoom into a dataset.
##' \itemize{
##'   \item \strong{numeric vector}: the first element defines the minimum longitude, the second element the maximum longitude, the third element the minimum latitude and the fourth element the maximum latitude of the bounding box. The coordinates have to be given as decimal degrees for Svalbard and Barents Sea maps and as UTM coordinates for pan-Arctic maps. See "Examples", \code{\link{map_projection}} and \code{\link{transform_coord}} how to find these coordinates.
##'   \item \strong{character vector}: the first element gives the object name of the data frame containing data to which the map should be limited, the second argument gives the column name of longitude data and the third argument the column name of latitude data. The map will be limited using rounded minimum and maximum floor and ceiling values for longitude and latitude. Use the \code{limits.lon} and \code{limits.lat} agruments to define the accuracy of rounding.
##' } 
##' @param limits.lon,limits.lat Numeric. The level of rounding for longitude and latitude, respectively, when using automatic limits (character vector in \code{limits} argument).
##' @param bathymetry Logical indicating whether bathymetry should be added to the map. Relatively slow. Defaults to \code{FALSE}
##' @param bathy.style Character defining the style for bathymetry contours. Alternatives: 
##' \itemize{
##' \item \code{"poly_blues"} plots polygons filled with different shades of blue.
##' \item \code{"poly_greys"} plots polygons filled with different shades of grey.
##' \item \code{"contour_blues"} contour lines with different shades of blue.
##' \item \code{"contour_grey"} plots grey contour lines.
##' }
##' @param legends Logical indicating whether legends for bathymetry and/or ocean currents should be shown. Defaults to \code{TRUE}
##' @param bathy.detailed Logical indicating whether detailed bathymetry shapefiles should be used. Works for Svalbard maps only (see \emph{Source}). Very slow due to the large file size. Use for limited areas, such as fjords, only.
##' @param land.col,gla.col,grid.col Character code specifying the color of land, glaciers and grid lines, respectively. Use \code{NA} to remove the grid lines.
##' @param round.lon,round.lat Numeric value specifying the level of rounding to be used to plot longitude and latitude grid lines. Override \code{n.lon.grid} or \code{n.lat.grid}
##' @param n.lon.grid,n.lat.grid Numeric value specifying the number of longitude and latitude grid lines, respectively. Alternatively use \code{round.lon} and \code{round.lat}
##' @param lon.interval,lat.interval Numeric value specifying the interval of longitude and latitude grids for polar stereographic maps (\code{type = "arctic50"} or \code{"arctic60"})
##' @param keep.glaciers Logical indicating whether glaciers should be kept for the Svalbard maps. Setting this to \code{FALSE} speeds up map plotting by a few seconds.
##' @param land.border.col,gla.border.col Character code specifying the color of the border line for land and glacier shapes.
##' @param land.size,gla.size,grid.size Numeric value specifying the width of the border line for land and glacier shapes as well as the width of the grid lines, respectively. See details for explanation about line widths.
##' @param currents logical indicating whether Arctic and Atlantic ocean currents for the Barents Sea should be plotted. See details. 
##' @param arc.col,atl.col Character code specifying the color for Arctic and Atlantic current arrows.
##' @param current.size Either a numeric value specifying the width of ocean current arrows or "scaled" for ocean currents that are approximately scaled to their size.   
##' @param current.alpha Value between 0 and 1 defining the transparency of current arrows.
##' @param label.print Logical indicating whether labels should be printed for polar stereographic maps.
##' @param label.font Numeric value specifying the font size for labels in polar stereographic maps. Note that this value defines the actual font size in points, not the \code{ggplot2} font size.
##' @param label.offset Offset between the round polar stereographic maps and longitude labels. Optimized for a pdf output. Use 1.1 for larger size figures.
##' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
##' @return Returns a \link[ggplot2]{ggplot2} map, which can be assigned to an object and modified as any ggplot object.
##' @details The function uses \link[ggplot2]{ggplot2} and shapefiles to plot maps of Svalbard and other polar regions. The Svalbard shapefiles are detailed and glaciers varyinly up-to-date (2017 for Kongsfjorden; mostly 2015 for the rest, but not systematically checked) detailed The map type is defined using the \code{type} argument and map limits can be controlled with the \code{limits} argument. Currently implemented map \code{type}s:
##' \itemize{
##' \item "svalbard". Detailed 1:250 000 map of Svalbard land and glaciers. This option is slow (approx. 25 seconds) due to the large file size.
##' \item "mosj" shows Kongsfjoden and Fram Strait as sampled during Norwegian Polar Institute's MOSJ campaigns. Some glaciers can be older than 2015.
##' \item "kongsfjorden" shows Kongsfjorden and parts of Prins Karls Forland. Glaciers are from 2015 to 2017. This map uses a subset of the Svalbard shape files and is faster to plot due to smaller file size.
##' \item "kongsfjordbotn" shows Kongsvegen, Kronebreen, Kongsbreen and Conwaybreen. Glaciers are from July 2017. This map uses a subset of the Svalbard shape files and is faster to plot due to a smaller file size.
##' \item "kronebreen" shows mostly Kronebreen and Kongsvegen. Glacier fronts are from July 2017. This map uses a subset of the Svalbard shape files and is faster to plot due to a smaller file size.
##' \item "barentssea". A 1:10 000 000 map of the Barents Sea.
##' \item "arctic50". A polar stereographic map of the Arctic with a limit at 50 degrees North.
##' \item "arctic60". A polar stereographic map of the Arctic with a limit at 60 degrees North.
##' }
##'
##' Svalbard and Barents Sea maps use the \code{"+init=epsg:32633"} UTM projection. The polar stereographic maps use \code{"+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"} projection.
##'
##' \strong{Line width} (size) aesthatics in \link[ggplot2]{ggplot2} generetes approximately 2.13 wider lines measured in pt than the given values. If you want a specific line width in pt, multiply it by 1/2.13. Internal functions \code{\link{LS}} and \code{\link{FS}} are available to convert line and font sizes in points to ggplot2 equivalents.
##'
##' \strong{Ocean currents} for the Barents Sea are implemented, but not peer-reviewed yet. This feature will be improved in the future versions of the package.
##'
##' @source \itemize{
##' \item Svalbard maps originate from the \href{http://geodata.npolar.no/}{Norwegian Polar Institute}. Distributed under the \href{https://creativecommons.org/licenses/by/4.0/}{CC BY 4.0 license} (\href{http://geodata.npolar.no/bruksvilkar/}{terms of use}).
##' \item Barents Sea and pan-Arctic maps are downloaded from \href{http://www.naturalearthdata.com}{Natural Earth Data}. They use the \href{http://www.naturalearthdata.com/downloads/10m-physical-vectors/}{\code{ne_10m_land}} and \href{http://www.naturalearthdata.com/downloads/50m-physical-vectors/}{\code{ne_50m_land}} (v 4.0.0) datasets, respectively. Distributed under the \href{https://creativecommons.org/publicdomain/}{CC Public Domain license} (\href{http://www.naturalearthdata.com/about/terms-of-use/}{terms of use}).
##' \item Pan-Arctic and Barents Sea bathymetry shapefiles are generalized from \href{https://www.ngdc.noaa.gov/mgg/bathymetry/arctic/ibcaoversion3.html}{IBCAO v3.0 500m RR grid}. Should be cited as \href{https://www.ngdc.noaa.gov/mgg/bathymetry/arctic/2012GL052219.pdf}{Jakobsson, M., et al. The International Bathymetric Chart of the Arctic Ocean (IBCAO) Version 3.0. Geophys. Res. Lett. 2012, 39:L12609.}
##' \item Svalbard fjord bathymetry shapefiles are from the \href{https://kartkatalog.geonorge.no/metadata/kartverket/dybdedata/2751aacf-5472-4850-a208-3532a51c529a}{Norwegian Mapping Authority}.  Distributed under the \href{https://creativecommons.org/licenses/by/4.0/}{CC BY 4.0 license}.
##' }
##'
##' @examples basemap() ## Plots Kongsfjorden
##'
##' ## Maps work as normal ggplot2 objects:
##'
##' data(chlorophyll)
##' p <- basemap("mosj")
##' p + geom_point(data = chlorophyll, aes(x = lon.utm, y = lat.utm,
##' size = Chla, color = Chla), shape = 1)
##'
##' ## limitting maps can be done using the limits argument:
##' basemap("kongsfjordbotn", limits = c(12.2,12.65,78.95,79.00))
##'
##' ## Svalbard map. Warning: this is SLOW
##' basemap("svalbard")
##'
##' ## grid.col = NA removes grid lines
##' basemap("svalbard", grid.col = NA, limits = c(10, 28, 79.5, 83))
##'
##' ## Use round.lat and n.lon.grid arguments
##' ## to control grid lines
##' basemap("svalbard", limits = c(3,24,78.5,82), round.lat = 1,
##' n.lon.grid = 4)
##'
##' ## Barents Sea
##' basemap("barentssea")
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
##' ## Bathymetry can be added using the bathymetry argument
##' basemap("arctic50", bathymetry = TRUE)
##'
##' ## Detailed bathymetry is available for some 
##' ## Svalbard fjords
##' basemap("kongsfjorden", bathymetry = TRUE, bathy.detailed = TRUE)
##'
##' ## Ocean currents for the Barents Sea
##' basemap("barentssea", bathymetry = TRUE, currents = TRUE)
##' 
##' @seealso \code{\link[ggplot2]{ggplot2}} \code{\link{theme_map}}
##'
##' @author Mikko Vihtakari, Anders Skoglund
##' @import ggplot2
##' @importFrom grDevices extendrange
##' @export

## Test parameters
# type = "barentssea"
# type = "svalbard"
# type = "kongsfjorden"
# limits = c("kongsfjord_moorings", "Lon", "Lat")
# type = "arctic60"; land.col = "#eeeac4"; gla.col = "grey95"; grid.col = "grey70"; limits = NULL; round.lat = FALSE; n.lat.grid = 3; round.lon = FALSE; n.lon.grid = 3; keep.glaciers = TRUE; size.land = 0.1; size.glacier = 0.1; size.grid = 0.1; border.col.land = "black"; border.col.glacier = "black"; lat.interval = 10; lon.interval = 45; label.font = 3; label.offset = 1.04; bathymetry = TRUE
# type = "arctic50"; land.col = "#eeeac4"; gla.col = "grey95"; grid.col = "grey70"; limits = NULL; round.lat = FALSE; n.lat.grid = 3; round.lon = FALSE; n.lon.grid = 3; keep.glaciers = TRUE; bathymetry = TRUE; size.land = 0.1; size.glacier = 0.1; size.grid = 0.1; border.col.land = "black"; border.col.glacier = "black"; lat.interval = 10; lon.interval = 45; label.font = 3; label.offset = 1.04
# type = "barentssea"; limits = NULL; limits.lon = 0.1; limits.lat = 0.1; round.lon = FALSE; n.lon.grid = 3; lon.interval = 45; round.lat = FALSE; n.lat.grid = 3; lat.interval = 10; keep.glaciers = TRUE; bathymetry = TRUE; bathy.style = "poly_blues"; bathy.legend = TRUE; bathy.detailed = FALSE; land.col = "#eeeac4"; land.border.col = "black"; land.size = 0.1; gla.col = "grey95"; gla.border.col = "black"; gla.size = 0.1; grid.col = "grey70"; grid.size = 0.1; currents = TRUE; arc.col = "#D696C8"; atl.col = "#BB1512"; current.size = "scaled"; current.alpha = 1; label.print = TRUE; label.offset = 1.05; label.font = 8; base_size = 11

basemap <- function(type = "kongsfjorden", limits = NULL, limits.lon = 0.1, limits.lat = 0.1, round.lon = FALSE, n.lon.grid = 3, lon.interval = 45, round.lat = FALSE, n.lat.grid = 3, lat.interval = 10, keep.glaciers = TRUE, bathymetry = FALSE, bathy.style = "poly_blues", bathy.detailed = FALSE, legends = TRUE, land.col = "#eeeac4", land.border.col = "black", land.size = 0.1, gla.col = "grey95", gla.border.col = "black", gla.size = 0.1, grid.col = "grey70", grid.size = 0.1, currents = FALSE, arc.col = "blue", atl.col = "#BB1512", current.size = 0.5, current.alpha = 1, label.print = TRUE, label.offset = 1.05, label.font = 8, base_size = 11) {

  if(length(limits) == 3 & is.character(limits)) {
    limits <- c(round_any(min(get(limits[1])[limits[2]]), limits.lon, floor), round_any(max(get(limits[1])[limits[2]]), limits.lon, ceiling), round_any(min(get(limits[1])[limits[3]]), limits.lat, floor), round_any(max(get(limits[1])[limits[3]]), limits.lat, ceiling))
  }
  
## Map data  
X <- switch(map_type(type)$map.type,
  panarctic = eval(parse(text=paste(map_cmd("base_dat_polar")))),
  svalbard = eval(parse(text=paste(map_cmd("base_dat")))),
  barents = eval(parse(text=paste(map_cmd("base_dat")))),
  kongsfjorden = eval(parse(text=paste(map_cmd("base_dat")))),
  stop(paste("map_type for", type, "not found."))
)

## Bathymetry data
if(bathymetry) {
  bathy <- clip_bathymetry(X, detailed = bathy.detailed)
  
  bathy_cmd <- switch(bathy.style,
    poly_blues = "bathy_pb",
    poly_greys = "bathy_pg",
    contour_blues = "bathy_cb",
    contour_grey = "bathy_cg",
    stop(paste("bathy_type for", type, "not found."))
  )
}

## Ocean current data
if(currents) {
  atl <- clip_current(atlantic_currents, X)
  arc <- clip_current(arctic_currents, X)
  scaled.currents <- ifelse(current.size == "scaled", TRUE, FALSE)
}

## Force switches

if(is.null(map_type(X$MapType)$glacier)) {
  keep.glaciers <- FALSE
}

## Map composing 

if(X$MapClass %in% c("panarctic")) {

## Pan-Arctic polar stereographic maps ####

  ## Bathymetry
  if(bathymetry) {
    layers <- paste(map_cmd("base"), map_cmd(bathy_cmd), map_cmd("grid_polar"), sep = " + ")
  } else {
    layers <- paste(map_cmd("base"), map_cmd("grid_polar"), sep = " + ")
  }
  
  ## Land
  if(length(X$Land) != 0) {
    layers <- paste(layers, map_cmd("land_polar"), sep = " + ")  
  }
  
  ## Square and round maps
  
  if(X$Grid$limits) { ## Square maps
    
    if(label.print) { ## With labels
      layers <- paste(layers, map_cmd("labels_polar_limits"), map_cmd("defs_polar_limits"), sep = " + ")  
    } else { ## Without labels
      layers <- paste(layers, map_cmd("defs_polar_limits"), map_cmd("remove_labels"), sep = " + ")  
    } 
    
  } else { ## Round maps
    
    if(label.print) { ## With labels
      layers <- paste(layers, map_cmd("labels_polar"), map_cmd("defs_polar"), sep = " + ")  
    } else { ## Without labels
      layers <- paste(layers, map_cmd("defs_polar"), sep = " + ")  
    }
  }
  
  ## Final plotting
  eval(parse(text=layers))

  
  } else {

  ## UTM maps (Svalbard, Barents Sea, etc.) ####
  
  ## Bathymetry
  if(bathymetry) {
    layers <- paste(map_cmd("base"), map_cmd(bathy_cmd), sep = " + ")
  } else {
    layers <- map_cmd("base")
  }
  
  ## Land and glaciers
  if(length(X$Land) != 0) {
    if(keep.glaciers) {
      layers <- paste(layers, map_cmd("land_utm"), map_cmd("glacier_utm"), sep = " + ")  
    } else {
      layers <- paste(layers, map_cmd("land_utm"), sep = " + ") 
    }
  }
  
  ## Ocean currents
  if(currents) {
    layers <- paste(layers, map_cmd("currents_utm", scaled.currents), sep = " + ")
  }
  
  ## Final plotting
  eval(parse(text=paste(layers, map_cmd("grid_utm"), map_cmd("defs_utm"), sep = " + ")))

  }
}