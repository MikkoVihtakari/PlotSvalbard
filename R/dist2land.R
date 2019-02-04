#' @title Calculate the closest distance to land for given coordinates
#' @description Calculates the closest distance to land within a \link[=basemap]{map type} for a given set of coordinates
#' @param x Data.frame containing geographic coordinates as decimal degrees
#' @param lon.col The name of the longitude column in \code{x}.
#' @param lat.col The name of the latitude column in \code{x}.
#' @param map.type a character string specifying the map type which land boundaries should be used for the distance calculation. See the \code{\link[=basemap]{type}} argument for possible map types.
#' @param bind Logical indicating whether \code{x} should be returned with the distances (\code{TRUE}, default) or should the distances be returned as vector (\code{FALSE}).
#' @param dist.col The name of the distance column, if \code{bind = TRUE}. Defaults to "dist".
#' @param geodesic_distances Logical indicating whether \code{\link[geosphere]{dist2Line}} function should be used to calculate shortest distances using the WGS84 ellipsoid (\code{TRUE}) or whether the distances should be calculated from UTM coordinates (\code{FALSE}, default). Setting the argument to \code{TRUE} presumably leads to more exact distance estimations, but takes a much longer time to process than the UTM coordinate estimation.
#' @return If \code{bind = TRUE}, returns a data frame with calculated distances to land. If \code{bind = FALSE} returns vector in the same order than coordinates specified in \code{x}. \strong{Distances are returned as kilometers}.
#' @details If \code{geodesic_distances = FALSE}, the function uses the \code{\link[rgeos]{gDistance}} function to calculate closest distances between coordinates in \code{x} and a specified SpatialPolygonsDataframe object. The spatial object (map) can be specified using the \code{\link[=basemap]{map.type}} argument. If \code{geodesic_distances = TRUE}, the \code{\link[geosphere]{dist2Line}} is used to calculate similar distances assumming an elliptical Earth. The \code{\link[geosphere]{dist2Line}} function is presumably more exact, especially for pan-Arctic maps, but considerably slower.
#' @import sp geosphere rgeos
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @author Mikko Vihtakari
#' @examples
#' ## Distances from land using UTM coordinates
#' library(ggplot2)
#' data("npi_stations")
#'
#' dists <- dist2land(npi_stations, lon.col = "Lon", lat.col = "Lat", map.type = "svalbard")
#' dists$Area <- ordered(dists$Area, c("Kongsfjorden", "Framstrait", "Rijpfjorden"))
#'
#' ggplot(dists, aes(x = Area, y = dist, label = Station)) +
#'   geom_text() + ylab("Distance to land (km)")
#'
#' ## Geodesic distances are presumably more exact,
#' ## but much slower to calculate. Do not use detailed
#' ## maps for these:
#'
#' d_utm <- dist2land(npi_stations, lon.col = "Lon", lat.col = "Lat",
#'   map.type = "barentssea", dist.col = "d_utm")
#' d_geo <- dist2land(npi_stations, lon.col = "Lon", lat.col = "Lat", map.type = "barentssea",
#'   geodesic_distances = TRUE, dist.col = "d_geo")
#'
#' y <- merge(d_utm[c("Station", "d_utm")], d_geo[c("Station", "d_geo")])
#'
#' ggplot(y, aes(x = d_utm, y = d_geo, label = Station)) +
#'  geom_text(color = "red") +
#'  geom_abline(slope=1, intercept=0) +
#'  scale_x_log10() + scale_y_log10()
#'
#' ## The processing difference between geodesic and UTM distances:
#'
# UTM:
#' system.time(dist2land(npi_stations, lon.col = "Lon", lat.col = "Lat",
#'  map.type = "barentssea"))
#' #> user  system elapsed
#' #> 0.22    0.01    0.25
#'
# Geodesic:
#' system.time(dist2land(npi_stations, lon.col = "Lon", lat.col = "Lat",
#'  map.type = "barentssea", geodesic_distances = TRUE))
#' #> user  system elapsed
#' #> 32.04    0.39   32.97
#'
#' ## Despite the inaccuracy due to polar stereographic protection
#' ## the UTM version seems to produce feasible distances from land
#' ## on pan-Arctic scale
#'
#' data("meiofauna")
#' d_panarctic <- dist2land(meiofauna,  lon.col = "Lon", lat.col = "Lat", map.type = "arctic50")
#' d_panarctic <- transform_coord(d_panarctic, lon = "Lon", lat = "Lat", map.type = "arctic50",
#' bind = TRUE)
#'
#' basemap("arctic50") +
#'  geom_point(data = d_panarctic, aes(x = lon.utm, y = lat.utm, color = dist), size = 3) +
#'  scale_color_viridis_c(name = "Distance (km)")
#'
#' @export

## Test parameters
# x <- dat[1:2,]; x <- npi_stations; lon.col = "Lon"; lat.col = "Lat"; map.type = "svalbard"; bind = TRUE; dist.col = "dist"; geodesic_distances = TRUE

dist2land <- function(x, lon.col = "longitude", lat.col = "latitude", map.type = "arctic50", bind = TRUE, dist.col = "dist", geodesic_distances = FALSE) {

  ## Land ####
  land <- get(map_type(map.type)$land)

  if(geodesic_distances) {
    land <- sp::spTransform(land, sp::CRS(map_projection("decimal_degree")))
  } else {
    land <- rgeos::createSPComment(land)
  }

  ## Coordinate points ####
  if(geodesic_distances) {
    mat <- as.matrix(x[c(lon.col, lat.col)])
  } else {
    pp <- x[c(lon.col, lat.col)]
    sp::coordinates(pp) <- c(lon.col, lat.col)
    sp::proj4string(pp) <- sp::CRS(map_projection("decimal_degree"))
    pp <- sp::spTransform(pp, sp::CRS(map_projection(map_type(map.type)$map.type)))
    if(sp::proj4string(pp) != sp::proj4string(land)) stop("Cannot convert projections correctly.")
  }

  ## Distance calculation

  if(geodesic_distances) {
    tmp <- data.frame(geosphere::dist2Line(mat, land))
    tmp <- tmp$distance/1000
  } else {
    pb <- utils::txtProgressBar(min = 0, max = length(pp), style = 3)
    tmp <- sapply(1:length(pp), function(i)  {
      rgeos::gDistance(pp[i], land)/1000
      utils::setTxtProgressBar(pb, i)
  })
  }

  ## Return
  if(bind) {
    x[[dist.col]] <- tmp
    x
  } else {
    tmp
  }

}


