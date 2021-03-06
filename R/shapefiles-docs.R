#' @title SpatialPolygonsDataFrame containing pan-Arctic bathymetry
#' @docType data
#' @keywords datasets shapefiles internal
#' @family shapefiles
#' @name arctic_bathy
#' @format SpatialPolygonsDataFrame containing pan-Arctic bathymetry. In polar stereographic 71 degrees projection. Generalized from GEBCO One Minute Grid
#' @source \href{https://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_one_minute_grid/}{General Bathymetric Chart of the Oceans}
#' @seealso basemap
"arctic_bathy"

#' @title SpatialPolygonsDataFrame containing land shapes for pan-Arctic map with a cutpoint at 40 degrees latitude.
#' @docType data
#' @keywords datasets shapefiles internal
#' @family shapefiles
#' @name arctic
#' @format SpatialPolygonsDataFrame
#' @source \href{http://www.naturalearthdata.com/}{Natural Earth Data}
#' @seealso basemap
"arctic"

#' @title Data frame containing Atlantic and Arctic ocean currents for the Barents Sea
#' @description The data frame, intended to be shown as arrows, represents the current understanding of Atlantic and Arctic ocean currents. The currents are updated as our knowledge increases. The Barent Sea currents are based on Eriksen et al. 2018, while North of Svalbard currents are based on the articles published by Arild Sundfjord and Fram Strait currents on work done by Laura de Steur.
#' @docType data
#' @keywords datasets shapefiles internal
#' @family shapefiles
#' @name barents_currents
#' @format Data frame
#' @source Vihtakari M, Sundfjord A, de Steur L (2019). Barents Sea ocean-current arrows modified from Eriksen et al. (2018). Norwegian Polar Institute and Institute of Marine Research. Available at: https://github.com/MikkoVihtakari/Barents-Sea-currents
#'
#' Modified from Eriksen E, Gjøsæter H, Prozorkevich D et al. (2018) From single species surveys towards monitoring of the Barents Sea ecosystem. Progress in Oceanography, 166, 4–14. doi:https://doi.org/10.1016/j.pocean.2017.09.007
#' @seealso basemap
"barents_currents"

#' @title SpatialPolygonsDataFrame containing bathymetry for the Barents Sea
#' @docType data
#' @keywords datasets shapefiles internal
#' @family shapefiles
#' @name barents_bathy
#' @format SpatialPolygonsDataFrame containing pan-Arctic bathymetry. In \code{"+init=epsg:32633"} projection.
#' @source \href{https://www.ngdc.noaa.gov/mgg/bathymetry/arctic/ibcaoversion3.html}{Jakobsson, M., et al. The International Bathymetric Chart of the Arctic Ocean (IBCAO) Version 3.0. Geophys. Res. Lett. 2012, 39:L12609}
#' @seealso basemap
"barents_bathy"

#' @title SpatialPolygonsDataFrame containing land shapes for the Barents Sea region.
#' @docType data
#' @keywords datasets shapefiles internal
#' @family shapefiles
#' @name arctic60
#' @format SpatialPolygonsDataFrame
#' @source \href{http://www.naturalearthdata.com/}{Natural Earth Data}
#' @seealso basemap
"barents.ld"

#' @title SpatialPolygonsDataFrame containing clip boundaries for Kongsfjorden
#' @docType data
#' @keywords datasets shapefiles internal
#' @family shapefiles
#' @name kong.cr
#' @format SpatialPolygonsDataFrame.
#' @author Mikko Vihtakari
#' @seealso basemap
"kong.cr"

#' @title SpatialPolygonsDataFrame containing clip boundaries for the Environmental Monitoring on Svalbard and Jan Mayen transect
#' @docType data
#' @keywords datasets shapefiles internal
#' @family shapefiles
#' @name mosj.cr
#' @format SpatialPolygonsDataFrame.
#' @author Mikko Vihtakari
#' @seealso basemap
"mosj.cr"

#' @title SpatialPolygonsDataFrame containing detailed bathymetry for Svalbard region
#' @docType data
#' @keywords datasets shapefiles internal
#' @family shapefiles
#' @name svalbard_bathy
#' @format SpatialPolygonsDataFrame
#' @source \href{https://kartkatalog.geonorge.no/metadata/kartverket/sjokart-dybdedata/2751aacf-5472-4850-a208-3532a51c529a}{Norwegian Mapping Authority}
#' @seealso basemap
"svalbard_bathy"

#' @title SpatialPolygonsDataFrame containing glacier boundaries on Svalbard. The tidal-glacier fronts for Kongsfjorden are from July 2017, while the rest vary between 2005 and 2017.
#' @docType data
#' @keywords datasets shapefiles internal
#' @family shapefiles
#' @name svalbard.gl
#' @format SpatialPolygonsDataFrame containing 1:250 000 glacier shapes of Svalbard. In \code{"+init=epsg:32633"} projection.
#' @source Norwegian Polar Institute (\url{http://geodata.npolar.no/})
#' @seealso basemap
"svalbard.gl"

#' @title SpatialPolygonsDataFrame containing land shapes for Svalbard
#' @docType data
#' @keywords datasets shapefiles internal
#' @family shapefiles
#' @name svalbard.ld
#' @format SpatialPolygonsDataFrame containing 1:250 000 landshapes of Svalbard. In \code{"+init=epsg:32633"} projection.
#' @source Norwegian Polar Institute (\url{http://geodata.npolar.no/})
#' @seealso basemap
"svalbard.ld"
