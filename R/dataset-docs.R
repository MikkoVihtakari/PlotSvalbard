#' @title Chlorophyll-a recordings in Kongsfjorden
#' @docType data
#' @keywords datasets
#' @name chlorophyll
#' @usage data(chlorophyll)
#' @format A dataframe containing following columns:
#' \itemize{
#'   \item Name. Sample name.
#'   \item Station. Sampling station.
#'   \item lon. Longitude of the sampling location in decimal degrees
#'   \item lat. Latitude of the sampling location in decimal degrees.
#'   \item lon.utm. Longitude of the sampling location as UTM coordinates (\code{"+init=epsg:32633"})
#'   \item lat.utm. Latitude of the sampling location as UTM coordinates (\code{"+init=epsg:32633"})
#'   \item From. Sampling depth (m)
#'   \item Chla. Chlorophyll-a value (mg/m3)
#'  }
#' @source Norwegian Polar Institute (\url{https://data.npolar.no/home/})
"chlorophyll"
#' @title Location of NPI's permanent marine biological monitoring stations
#' @docType data
#' @keywords datasets
#' @name npi_stations
#' @usage data(npi_stations)
#' @format A dataframe containing following columns:
#' \itemize{
#'   \item Area. Geographic location of station.
#'   \item Station. Sampling station name.
#'   \item Lon. Longitude of the sampling location in decimal degrees
#'   \item Lat. Latitude of the sampling location in decimal degrees.
#'  }
#' @source Norwegian Polar Institute (\url{http://www.npolar.no/en/})
"npi_stations"
#' @title Location of recorded moorings in Kongsfjorden
#' @docType data
#' @keywords datasets
#' @name kongsfjord_moorings
#' @usage data(kongsfjord_moorings)
#' @format A dataframe 
#' @source Hop, H., Cottier, F., and Berge, J. (in press). "Marine observatories in Kongsfjorden, Svalbard" in The ecosystem of Kongsfjorden, Svalbard, eds. H. Hop and C. Wiencke (Advances in Polar Ecology, Springer Verlag).
"kongsfjord_moorings"
#' @title Pan-Arctic meiofauna data from Bluhm et al. 2018
#' @docType data
#' @keywords datasets
#' @name meiofauna
#' @usage data(meiofauna)
#' @format A dataframe containing pan-Arcitc meiofauna data sampled using ice-cores
#' @source \href{http://doi.wiley.com/10.1002/ece3.3797}{Bluhm, B.A., Hop, H., Vihtakari, M., Gradinger, R., Iken, K., Melnikov, I.A., & Søreide, J.E. (2018) Sea ice meiofauna distribution on local to pan-Arctic scales. Ecology and Evolution, 8:2350-2364.}
"meiofauna"
#' @title SpatialPolygons object for Svalbard landshapes
#' @docType data
#' @keywords datasets
#' @name svalbard.ld
#' @format Spatial polygons containing 1:250 000 landshapes of Svalbard. In \code{"+init=epsg:32633"} projection.
#' @source Norwegian Polar Institute (\url{http://geodata.npolar.no/})
"svalbard.ld"
# 'barents_bathy' 'arctic_bathy' 'barents.ld' 'kong.cr' 'kong.gl' 'kong.ld' 'krone.cr' 'krone.cr.det' 'krone.cr.mid' 'mosj.cr' 'mosj.gl' 'mosj.ld' 'arctic50' 'arctic60' 'svalbard.gl'  'svalbard_bathy' 'zooplankton'