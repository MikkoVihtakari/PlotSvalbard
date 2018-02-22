##' @title Create a ggplot2 basemap for plotting variables
##' @description Creates a ggplot2 basemap for further plotting of variables.
##' @param type Type of map area. Options: "svalbard", "mosj", "kongsfjorden", "kongsfjordbotn", "kronebreen", "barentssea", "arctic50" or "arctic60". See details.
##' @param limits Map limits. A numeric vector of length 4 where first element defines the minimum longitude, second element the maximum longitude, third element the minimum latitude and fourth element the maximum latitude of the bounding box. The coordinates have to be given as decimal degrees for Svalbard and Barents Sea maps and as UTM coordinates for pan-Arctic maps. See "Examples", \code{\link{map_projection}} and \code{\link{transform_coord}} how to find these coordinates.
##' @param bathymetry Logical indicating whether bathymetry should be added to the map. Relatively slow. Defaults to \code{FALSE}
##' @param bathy_legend Logical indicating whether legend for bathymetry should be shown. Defaults to \code{TRUE}
##' @param bathy_detailed Logical indicating whether detailed bathymetry shapefiles should be used. Works for Svalbard maps only (see \emph{Source}). Very slow due to the large file size. Use for limited areas, such as fjords, only. 
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
##'
##' \strong{Line width} (size) aesthatics in \link[ggplot2]{ggplot2} generetes approximately 2.13 wider lines measured in pt than the given values. If you want a specific line width in pt, multiply it by 1/2.13.
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
##' ## Current bathymetry shapefiles are not detailed
##' ## enough for Svalbard fjords
##' basemap("kongsfjorden", bathymetry = TRUE)
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
# type = "arctic60"; land.col = "#eeeac4"; gla.col = "grey95"; grid.col = "grey70"; limits = NULL; round.lat = FALSE; n.lat.grid = 3; round.lon = FALSE; n.lon.grid = 3; keep.glaciers = TRUE; size.land = 0.1; size.glacier = 0.1; size.grid = 0.1; border.col.land = "black"; border.col.glacier = "black"; lat.interval = 10; lon.interval = 45; label.font = 3; label.offset = 1.04; bathymetry = TRUE
# type = "arctic50"; land.col = "#eeeac4"; gla.col = "grey95"; grid.col = "grey70"; limits = NULL; round.lat = FALSE; n.lat.grid = 3; round.lon = FALSE; n.lon.grid = 3; keep.glaciers = TRUE; bathymetry = TRUE; size.land = 0.1; size.glacier = 0.1; size.grid = 0.1; border.col.land = "black"; border.col.glacier = "black"; lat.interval = 10; lon.interval = 45; label.font = 3; label.offset = 1.04

basemap <- function(type = "kongsfjorden", limits = NULL, round.lat = FALSE, n.lat.grid = 3, lat.interval = 10, round.lon = FALSE, n.lon.grid = 3, lon.interval = 45, keep.glaciers = TRUE, bathymetry = FALSE, bathy_legend = TRUE, bathy_detailed = FALSE, land.col = "#eeeac4", size.land = 0.1, border.col.land = "black", gla.col = "grey95", size.glacier = 0.1, border.col.glacier = "black", grid.col = "grey70", size.grid = 0.1, label.print = TRUE, label.offset = 1.05, label.font = 8) {

X <- switch(map_type(type)$map.type,
  panarctic = eval(parse(text=paste(map_cmd("base_dat_polar")))),
  svalbard = eval(parse(text=paste(map_cmd("base_dat")))),
  barents = eval(parse(text=paste(map_cmd("base_dat")))),
  kongsfjorden = eval(parse(text=paste(map_cmd("base_dat")))),
  stop(paste("map_type for", type, "not found."))
)

if(bathymetry) {
  bathy <- clip_bathymetry(X, detailed = bathy_detailed)
}

if(X$MapClass %in% c("panarctic")) {

## Pan-Arctic maps ####
    if(X$Grid$limits) { ## Square maps

      if(label.print) { ## With axis labels

        if(bathymetry) { ## With bathymetry
          eval(parse(text=paste(map_cmd("base"), map_cmd("bathy"), map_cmd("grid_polar"), map_cmd("land_polar"), map_cmd("labels_polar_limits"), map_cmd("defs_polar_limits"), sep = "+")))

        } else { ## Without bathymetry
        eval(parse(text=paste(map_cmd("base"), map_cmd("grid_polar"), map_cmd("land_polar"), map_cmd("labels_polar_limits"), map_cmd("defs_polar_limits"), sep = "+")))
        }

      } else { ## Without axis labels

        if(bathymetry) { ## With bathymetry

          eval(parse(text=paste(map_cmd("base"), map_cmd("bathy"), map_cmd("grid_polar"), map_cmd("land_polar"), map_cmd("defs_polar_limits"), map_cmd("remove_labels"), sep = "+")))

        } else { ## Without bathymetry
        eval(parse(text=paste(map_cmd("base"), map_cmd("grid_polar"), map_cmd("land_polar"), map_cmd("defs_polar_limits"), map_cmd("remove_labels"), sep = "+")))
        }

      }} else if(label.print) { ## Round maps, with labels

      if(bathymetry) { ## With bathymetry
        eval(parse(text=paste(map_cmd("base"), map_cmd("bathy"), map_cmd("grid_polar"), map_cmd("land_polar"), map_cmd("labels_polar"), map_cmd("defs_polar"), sep = "+")))

      } else { ## Without bathymetry
      eval(parse(text=paste(map_cmd("base"), map_cmd("grid_polar"), map_cmd("land_polar"), map_cmd("labels_polar"), map_cmd("defs_polar"), sep = "+")))
      }

    } else { ## Without labels

      if(bathymetry) { ## With bathymetry
        eval(parse(text=paste(map_cmd("base"), map_cmd("bathy"), map_cmd("grid_polar"), map_cmd("land_polar"), map_cmd("defs_polar"), sep = "+")))

        } else { ## Without bathymetry
        eval(parse(text=paste(map_cmd("base"), map_cmd("grid_polar"), map_cmd("land_polar"), map_cmd("defs_polar"), sep = "+")))

  }}

  } else {

## Other maps ####
  if(keep.glaciers) { ## With glaciers

    if(bathymetry) { ## With bathymetry

    eval(parse(text=paste(map_cmd("base"), map_cmd("bathy"), map_cmd("land_utm"), map_cmd("glacier_utm"), map_cmd("grid_utm"), map_cmd("defs_utm"), sep = "+")))

    } else { ## Without bathymetry

    eval(parse(text=paste(map_cmd("base"), map_cmd("land_utm"), map_cmd("glacier_utm"), map_cmd("grid_utm"), map_cmd("defs_utm"), sep = "+")))

  }} else {

    if(bathymetry) { ## With bathymetry

    eval(parse(text=paste(map_cmd("base"), map_cmd("bathy"), map_cmd("land_utm"), map_cmd("grid_utm"), map_cmd("defs_utm"), sep = "+")))

    } else { ## Without bathymetry
    eval(parse(text=paste(map_cmd("base"), map_cmd("land_utm"), map_cmd("grid_utm"), map_cmd("defs_utm"), sep = "+")))
    }
  }}

}
