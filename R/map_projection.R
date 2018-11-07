#' @title Get the projection for a map type
#' @description The function prints the \code{\link[sp]{proj4string}} code used for a map type
#' @param map.type A character argument giving the name of the map type. Options: "kongsfjorden", "svalbard", "barents" or "panarctic"
#' @details Note that the map type has to be specified using the general map type name specified by the \code{$map.type} argument returned by \code{\link{map_type}}.
#' @keywords internal
#' @seealso \code{\link{map_type}}
#' @author Mikko Vihtakari
#' @export

map_projection <- function(map.type) {
  switch(map.type,
    decimal_degree = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
    svalbard = "+init=epsg:32633 +proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
    barents = "+init=epsg:32633 +proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
    panarctic = "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
    stop("map.type not found"))
}
