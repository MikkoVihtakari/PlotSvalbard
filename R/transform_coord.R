##' @title Transform spatial coordinates to another projection
##' @description The function transforms spatial coordinates from original projection (decimal degrees assumed) to another projection.
##' @param x data frame to be transformed. Can be omitted if numeric vectors are assigned to \code{lon} and \code{lat}.
##' @param lon either a name of the longitude column in \code{x} or a numeric vector containing longitude coordinates.
##' @param lat either a name of the latitude column in \code{x} or a numeric vector containing latitude coordinates.
##' @param new.names a vector of length 2 specifying the names of transformed longitude and latitude columns, respectively. \code{NULL} returns column names from \code{x}
##' @param proj.og original \code{\link[sp]{proj4string}} projection. If \code{NULL}, the projection is taken from \code{x}. \code{x} must be a \link[sp]{Spatial} object in that case.
##' @param proj.out the \code{\link[sp]{proj4string}} projection the coordinates should be transformed to. Defaults to the \link[=basemap]{"svalbard"} shape file projection.
##' @param map.type a character string specifying the map type for which coordinates should be transformed to. If \code{NULL} (default), \code{proj.out} is used to determine the returned projection. See \code{\link[=basemap]{type}} argument for possible map types. Overrides \code{proj.out}.
##' @param verbose if \code{TRUE} (default), the function prints information about the returned data frame. Switch to \code{FALSE} to make the function silent.
##' @param bind Should only transformed coordinates be returned (\code{FALSE}, default) or should x be returned with transformed coordinates (\code{TRUE})?
##' @return Returns a data frame with transformed spatial coordinates
##' @author Mikko Vihtakari
##' @import sp
##' @export


#x = lims; lon = "longitude"; lat = "latitude"; new.names = c("lon.utm", "lat.utm"); proj.og = "+proj=longlat +datum=WGS84"; proj.out = "+init=epsg:32633"; map.type = NULL; verbose = TRUE

transform_coord <- function(x = NULL, lon = "longitude", lat = "latitude", new.names = c("lon.utm", "lat.utm"), proj.og = "+proj=longlat +datum=WGS84", proj.out = "+init=epsg:32633", map.type = NULL, verbose = TRUE, bind = FALSE) {

if(is.null(x) & (!is.numeric(lon) | !is.numeric(lat))) {
  stop("give either x or lon and lat as numeric vectors")
}

if(!is.null(x) & (!is.character(lon) | !is.character(lat))) {
  stop("lat and lon must give column names of x as character")
}

if(is.null(x) & (is.numeric(lon) | is.numeric(lat))) {
  if(length(lon) != length(lat)) stop("lat and lon must be of equal length")
  y <- data.frame(lon = lon, lat = lat)
  sp::coordinates(y) <- c("lon", "lat")
}

if(!is.null(x)) {
  if(!is.data.frame(x)) stop("x must be a data frame")
  y <- x
  sp::coordinates(y) <- c(lon, lat)
}

sp::proj4string(y) <- sp::CRS(proj.og)

if(!is.null(map.type)) {
  proj.out <- map_projection(map_type(map.type)$map.type)
}

y <- sp::spTransform(y, sp::CRS(proj.out))
y <- data.frame(sp::coordinates(y))

if(!is.null(new.names)) {
  if(any(length(new.names) != 2, !is.character(new.names))) {
    stop("new.names must be a character vector with length of 2")
  }
  colnames(y) <- new.names
}

if(verbose) {
  message(paste("projection transformed from", proj.og, "to", proj.out))
}

if(bind) {
  cbind(x, y)
  } else {
    y}
}
