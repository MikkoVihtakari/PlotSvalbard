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

#' @title Zooplankton data
#' @docType data
#' @keywords datasets
#' @name zooplankton
#' @usage data(zooplankton)
#' @format A dataframe
"zooplankton"

#' @title Example CTD data for Rijpfjorden
#' @docType data
#' @keywords datasets
#' @family ts_plot section_plot
#' @name ctd_rijpfjord
#' @description A dataframe containing an example CTD transect data from Rijpfjorden.
#' @format data.frame
"ctd_rijpfjord"

#' @title Example CTD data for Kongsfjorden
#' @docType data
#' @keywords datasets
#' @family ts_plot
#' @name ctd_kongsfjord
#' @description A dataframe containing example CTD data for Kongsfjorden.
#' @format data.frame
"ctd_kongsfjord"

#' @title Water mass definitions for Rijpfjorden
#' @docType data
#' @keywords datasets
#' @family ts_plot
#' @name rijpfjord_watermasses
#' @description A dataframe containing water mass definitions for Rijpfjorden.
#' @details A pre-made \code{\link[=define_water_type]{WM}} data frame for the NPI's Rijpfjorden transect. The water types in this classification have been modified from Pérez-Hernández et al. (2017), which was originally made for the same region without the fjord. The changes include addition of Winter Cooled Water from Cottier et al. (2005) and exlusion of density in the categorization. Instead, salinity threshold of 34.87 for Atlantic and Arctic Intermediate Waters (AIW) was used. This decision is purely due to the difficulty of programming polygons in 3D space. It may cause very minor changes in water mass classification at the boundary of AIW and PSW.
#' @format data.frame
"rijpfjord_watermasses"

#' @title Water mass definitions for Kongsfjorden
#' @docType data
#' @keywords datasets
#' @family ts_plot
#' @name kongsfjord_watermasses
#' @description A dataframe containing water mass definitions for Kongsfjorden. An example of formatting of the \code{WM} argument in \code{\link{define_water_type}}
#' @details A pre-made \code{\link[=define_water_type]{WM}} data frame for the NPI's Kongsfjorden transect (also called MOSJ transect). the water types are returned after the definition in Cottier et al. (2005) with slight modifications to avoid overlaps of water types. \strong{Density} boundaries have not been implemented as these seem to have relatively little meaning in tested data after limiting water types using temperature and salinity.
#' @format data.frame
"kongsfjord_watermasses"
