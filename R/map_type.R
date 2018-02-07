#' @title Get information for a map type
#' @description The function prints the information used in internal calculations by the \code{\link{basemap}} function
#' @param type A character argument giving the type of map area. See \code{type} in \code{\link{basemap}}.
#' @seealso \code{\link{map_projection}} \code{\link{basemap}}
#' @keywords internal
#' @author Mikko Vihtakari
#' @export

map_type <- function(type) {
  switch (type,
  mosj = list(land = "svalbard.ld", glacier = "svalbard.gl", boundary = "mosj.cr", map.type = "svalbard", round.lon = 1, round.lat = 0.2),
  kongsfjorden = list(land = "kong.ld", glacier = "kong.gl", boundary = "kong.cr", map.type = "kongsfjorden", round.lon = 0.5, round.lat = 0.1),
  kongsfjordbotn = list(land = "kong.ld", glacier = "kong.gl", boundary = c(12.2,12.65,78.855,79.00), map.type = "kongsfjorden", round.lon = 0.1, round.lat = 0.05),
  kronebreen = list(land = "kong.ld", glacier = "kong.gl", boundary = c(12.32,12.62,78.855,78.91), map.type = "kongsfjorden", round.lon = 0.1, round.lat = 0.02),
  svalbard = list(land = "svalbard.ld", glacier = "svalbard.gl", boundary = c(10,28,76,81), map.type = "svalbard", round.lon = 2, round.lat = 1),
  rijpfjorden = list(land = "svalbard.ld", glacier = NULL, boundary = c(19.5,23.5,80,81.7), map.type = "svalbard", round.lon = 0.5, round.lat = 0.4),
  barentssea = list(land = "barents.ld", glacier = NULL, boundary = c(0,50,70,83), map.type = "barents", round.lon = 4, round.lat = 2),
  arctic50 = list(land = "arctic50", glacier = NULL, boundary = c(NA,NA,50,90), map.type = "panarctic", lon.interval = 45, lat.interval = 10),
  arctic60 = list(land = "arctic60", glacier = NULL, boundary = c(NA,NA,60,90), map.type = "panarctic", lon.interval = 45, lat.interval = 10),
  stop(paste("type argument", type, "is not implemented."))
)
}

