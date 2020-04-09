#' @title Calculate the closest distance to land for given coordinates
#' @description Calculates the closest distance to land within a \link[=basemap]{map type} for a given set of coordinates
#' @param x Data.frame containing geographic coordinates as decimal degrees
#' @param lon.col The name of the longitude column in \code{x}.
#' @param lat.col The name of the latitude column in \code{x}.
#' @param map.type a character string specifying the map type which land boundaries should be used for the distance calculation. See the \code{\link[=basemap]{type}} argument for possible map types.
#' @param bind Logical indicating whether \code{x} should be returned with the distances (\code{TRUE}, default) or should the distances be returned as vector (\code{FALSE}).
#' @param dist.col The name of the distance column, if \code{bind = TRUE}. Defaults to "dist".
#' @param return.binary Logical indicating whether binary (TRUE = the position is in the ocean, FALSE = the position is on land) should be returned instead of distances. Speeds up the function considerably.
#' @param geodesic.distances Logical indicating whether \code{\link[geosphere]{dist2Line}} function should be used to calculate shortest distances using the WGS84 ellipsoid (\code{TRUE}) or whether the distances should be calculated from UTM coordinates (\code{FALSE}, default). Setting the argument to \code{TRUE} presumably leads to more exact distance estimations, but takes a much longer time to process than the UTM coordinate estimation.
#' @return If \code{bind = TRUE}, returns a data frame with calculated distances to land. If \code{bind = FALSE} returns vector in the same order than coordinates specified in \code{x}. \strong{Distances are returned as kilometers}. If \code{return_binary = TRUE}, binary values are returned instead of distances (TRUE = the position is in the ocean, FALSE = the position is on land).
#' @param cores Integer value defining how many cores should be used in the calculations. Parallelization speeds up the function (see \code{\link[parallel]{mclapply}}), but naturally eats up computer resources during the calculation. Set to 1 to remove parallelization (default).
#' @details If \code{geodesic.distances = FALSE}, the function uses the \code{\link[rgeos]{gDistance}} function to calculate closest distances between coordinates in \code{x} and a specified SpatialPolygonsDataframe object. The spatial object (map) can be specified using the \code{\link[=basemap]{map.type}} argument. If \code{geodesic.distances = TRUE}, the \code{\link[geosphere]{dist2Line}} is used to calculate similar distances assumming an elliptical Earth. The \code{\link[geosphere]{dist2Line}} function is presumably more exact, especially for pan-Arctic maps, but considerably slower.
#'
#' The function is very slow for large datasets. If you only want to use the function to remove (wrong) observations reported on land, set the \code{return.binary} argument to \code{TRUE}. This speeds up the function considerably.
#'
#' The function has a possibility for parallel processing, which speeds up the calculations for large datasets. The parallel processing has been turned off by default (\code{cores = 1}). You can turn it on by setting another integer or a function, which produces such an integer (for example \code{parallel::detectCores() - 2} uses all avaible cores exept two). Parallelizing does not work under Windows.
#' @import sp geosphere rgeos
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom parallel detectCores
#' @importFrom pbmcapply pbmclapply
#' @author Mikko Vihtakari
#' @examples
#' ## Distances from land using UTM coordinates
#' library(ggplot2)
#' data("npi_stations")
#'
#' dists <- dist2land(npi_stations, map.type = "svalbard")
#' dists$Area <- ordered(dists$Area, c("Kongsfjorden", "Framstrait", "Rijpfjorden"))
#'
#' ggplot(dists, aes(x = Area, y = dist, label = Station)) +
#'   geom_text() + ylab("Distance to land (km)")
#'
#' ## Geodesic distances are presumably more exact,
#' ## but much slower to calculate. Do not use detailed
#' ## maps for these:
#'
#' d_utm <- dist2land(npi_stations,
#'   map.type = "barentssea", dist.col = "d_utm")
#' d_geo <- dist2land(npi_stations, map.type = "barentssea",
#'   geodesic.distances = TRUE, dist.col = "d_geo")
#'
#' y <- merge(d_utm[c("Station", "d_utm")], d_geo[c("Station", "d_geo")])
#'
#' ggplot(y, aes(x = d_utm, y = d_geo, label = Station)) +
#'  geom_text(color = "red") +
#'  geom_abline(slope=1, intercept=0) +
#'  scale_x_log10() + scale_y_log10()
#'
#' ## The processing time difference between binary, geodesic and UTM distances:
#'
#' # Binary:
#' system.time(dist2land(npi_stations, map.type = "barentssea",
#' return.binary = TRUE))
#' #> user  system elapsed
#' #> 0.017   0.000   0.017
#'
#' # UTM:
#' system.time(dist2land(npi_stations, map.type = "barentssea"))
#' #> user  system elapsed
#' #> 0.073   0.003   0.073
#'
#' # Geodesic:
#' system.time(dist2land(npi_stations, map.type = "barentssea",
#' geodesic.distances = TRUE))
#' #> user  system elapsed
#' #> 14.096   0.615  14.755
#'
#' ## Despite the inaccuracy due to polar stereographic protection
#' ## the UTM version seems to produce feasible distances from land
#' ## on pan-Arctic scale
#'
#' data("meiofauna")
#' d_panarctic <- dist2land(meiofauna,  lon.col = "Lon", lat.col = "Lat", map.type = "panarctic")
#' d_panarctic <- transform_coord(d_panarctic, lon = "Lon", lat = "Lat", map.type = "panarctic",
#' bind = TRUE)
#'
#' basemap("panarctic", limits = c("d_panarctic", "lon.utm", "lat.utm")) +
#'  geom_point(data = d_panarctic, aes(x = lon.utm, y = lat.utm, color = dist), size = 3) +
#'  scale_color_viridis_c(name = "Distance (km)")
#'
#' @export

## Test parameters

# x <- head(x); lon.col = "longitudestart"; lat.col = "latitudestart"; map.type = "panarctic"; bind = TRUE; geodesic.distances = FALSE; cores = 2
# x <- head(lb); lon.col = "lon"; lat.col = "lat"; map.type = "panarctic"; bind = FALSE; dist.col = "dist"; geodesic.distances = FALSE; cores = parallel::detectCores() - 1
# x <- npi_stations; lon.col = "Lon"; lat.col = "Lat"; map.type = "svalbard"; bind = TRUE; dist.col = "dist"; geodesic.distances = TRUE
# lon.col = "lon.in"; lat.col = "lat.in"; map.type = "panarctic"; bind = FALSE; dist.col = "dist"; geodesic.distances = FALSE; cores = 1
# lon.col = NULL; lat.col = NULL; map.type = "panarctic"; bind = FALSE; dist.col = "dist"; return.binary = TRUE; geodesic.distances = FALSE; cores = 1

dist2land <- function(x, lon.col = NULL, lat.col = NULL, map.type = "panarctic", bind = TRUE, dist.col = "dist", return.binary = FALSE, geodesic.distances = FALSE, cores = 1) {

  # Convert data.table to data.frame (consider a way to remove this conversion later)

  if(any(class(x) %in% c("tbl", "data.table"))) {
    x <- as.data.frame(x)
  }

  ## Case for defined x and undefined lon or/and lat
  if(is.null(lon.col) | is.null(lat.col)) {
    if(all(class(x) != "data.frame")) stop("x argument has to be a data.frame")

    if(is.null(lon.col)) {
      lon.col <- colnames(x)[grep("^lon$|longitude|^long$", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(x)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
    }

    if(is.null(lat.col)) {
      lat.col <- colnames(x)[grep("^lat$|latitude", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(x)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
    }
  }

if(length(lon.col) != 1 | length(lat.col) != 1) {
  stop("Either lon.col or lat.col was not found. Define manually.")
}


## Land ####
land <- get(map_type(map.type)$land)

if(geodesic.distances & !return.binary) {
  land <- sp::spTransform(land, sp::CRS(map_projection("decimal_degree")))
} else {
  land <- rgeos::createSPComment(land)
}

## Remove NA coordinates (and add later)

na.rows <- is.na(x[[lon.col]]) | is.na(x[[lat.col]])
contains.nas <- any(na.rows)

## Coordinate points ####
if(geodesic.distances & !return.binary) {
  mat <- as.matrix(x[!na.rows, c(lon.col, lat.col)])
} else {
  pp <- x[!na.rows, c(lon.col, lat.col)]
  sp::coordinates(pp) <- c(lon.col, lat.col)
  sp::proj4string(pp) <- sp::CRS(map_projection("decimal_degree"))
  pp <- sp::spTransform(pp, sp::CRS(map_projection(map_type(map.type)$map.type)))
  if(sp::proj4string(pp) != sp::proj4string(land)) stop("Cannot convert projections correctly.")
}

############################
## Distance calculation ####


if(return.binary) {

  ## Binary positions
  tmp <- unname(is.na(sp::over(pp, land)))

} else if(geodesic.distances) {

  ## Geodesic distances

  tmp <- data.frame(geosphere::dist2Line(mat, land))
  tmp <- tmp$distance/1000
} else {

  ## gDistance

  if(cores > 1) {

    #pb <- utils::txtProgressBar(min = 0, max = length(pp), style = 3)
    tmp <- pbmcapply::pbmclapply(1:length(pp), function(i)  {
      #  utils::setTxtProgressBar(pb, i)
      return(rgeos::gDistance(pp[i], land)/1000)
    }, mc.cores = parallel::detectCores() - 1)
    tmp <- unlist(tmp)

  } else {

    pb <- utils::txtProgressBar(min = 0, max = length(pp), style = 3)
    tmp <- sapply(1:length(pp), function(i)  {
      utils::setTxtProgressBar(pb, i)
      return(rgeos::gDistance(pp[i], land)/1000)
    })

  }
}

## Return

if(contains.nas) {
  na.rows[!na.rows] <- tmp
  na.rows[na.rows == 1] <- NA
  tmp <- na.rows
}

if(bind) {
  x[[dist.col]] <- tmp
  x
} else {
  tmp
}

}


