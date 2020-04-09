#' @title Get information for a map type
#' @description The function prints the information used in internal calculations by the \code{\link{basemap}} function
#' @param type A character argument giving the type of map area. See \code{type} in \code{\link{basemap}}.
#' @details Returns a list of different map \code{type} definitions required by the \code{\link{basemap}} function. The list consists of following elements:
#' \itemize{
#' \item \strong{land} name of the land shapefile.
#' \item \strong{glacier} name of the glacier shapefile. \code{NULL} if glaciers are not provided.
#' \item \strong{boundary} either a shapefile or a \code{\link[=basemap]{limit}} argument defining the default limits for the map. Will be overriden by the \code{basemap limit} argument.
#' \item \strong{map.type} defines the class of the map with "svalbard", "barents", and "panarctic" as options. Due to more and longsighted naming early on during the package writing, this argument got the name \code{map.type}, which is unfortunate. Should be changed to map.class eventually.
#' \item \strong{round.lon,round.lat} default \code{\link[=basemap]{rounding}} arguments specifying the level of rounding to be used to plot longitude and latitude grid lines.
#' }
#' @seealso \code{\link{map_projection}} \code{\link{basemap}}
#' @keywords internal
#' @author Mikko Vihtakari
#' @export

map_type <- function(type) {
  switch (type,
  mosj = list(land = "svalbard.ld", glacier = "svalbard.gl", boundary = "mosj.cr", map.type = "svalbard", round.lon = 1, round.lat = 0.2, crs = 32633),
  kongsfjorden = list(land = "svalbard.ld", glacier = "svalbard.gl", boundary = "kong.cr", map.type = "svalbard", round.lon = 0.5, round.lat = 0.1, crs = 32633),
  kongsfjordbotn = list(land = "svalbard.ld", glacier = "svalbard.gl", boundary = c(12.2,12.65,78.855,79.00), map.type = "svalbard", round.lon = 0.1, round.lat = 0.05, crs = 32633),
  kronebreen = list(land = "svalbard.ld", glacier = "svalbard.gl", boundary = c(12.32,12.62,78.855,78.91), map.type = "svalbard", round.lon = 0.1, round.lat = 0.02, crs = 32633),
  svalbard = list(land = "svalbard.ld", glacier = "svalbard.gl", boundary = c(10,28,76,81), map.type = "svalbard", round.lon = 2, round.lat = 1, crs = 32633),
  rijpfjorden = list(land = "svalbard.ld", glacier = "svalbard.gl", boundary = c(19.5,23.5,80,81.7), map.type = "svalbard", round.lon = 0.5, round.lat = 0.4, crs = 32633),
  barentssea = list(land = "barents.ld", glacier = NULL, boundary = c(0,50,70,83), map.type = "barents", round.lon = 4, round.lat = 2, crs = 32633),
  panarctic = list(land = "arctic", glacier = NULL, boundary = c(NA,NA,30,90), map.type = "panarctic", lon.interval = 45, lat.interval = 10, crs = 3995),
  arctic50 = stop('"arctic50" and "arctic60" map types have been replaced by basemap(type = "panarctic", limits = N), where N is any integer between 30 and 88 defining the limiting latitude.'),
  arctic60 = stop('"arctic50" and "arctic60" map types have been replaced by basemap(type = "panarctic", limits = N), where N is any integer between 30 and 88 defining the limiting latitude.'),
  stop(paste("type argument", type, "is not implemented."))
)
}

