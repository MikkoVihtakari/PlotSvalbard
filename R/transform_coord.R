##' @title Transform spatial coordinates to another projection
##' @description The function transforms spatial coordinates from original projection (decimal degrees assumed) to another projection.
##' @param x data frame to be transformed. Can be omitted if numeric vectors are assigned to \code{lon} and \code{lat}.
##' @param lon,lat either a name of the longitude and latitude columns in \code{x} or a numeric vector containing longitude and latitude coordinates. Use \code{NULL} to guess the longitude and/or latitude columns in \code{x}.
##' @param new.names a vector of length 2 specifying the names of transformed longitude and latitude columns, respectively. \code{NULL} returns column names from \code{x}
##' @param proj.og original \code{\link[sp]{proj4string}} projection. If \code{NULL}, the projection is taken from \code{x}. \code{x} must be a \link[sp]{Spatial} object in that case.
##' @param proj.out the \code{\link[sp]{proj4string}} projection the coordinates should be transformed to. Defaults to the \link[=basemap]{"svalbard"} shape file projection.
##' @param map.type a character string specifying the map type for which coordinates should be transformed to. If \code{NULL} (default), \code{proj.out} is used to determine the returned projection. See \code{\link[=basemap]{type}} argument for possible map types. Overrides \code{proj.out}.
##' @param verbose if \code{TRUE}, the function prints information about the changed projection. Switch to \code{FALSE} to make the function silent.
##' @param bind logical. Should only transformed coordinates be returned (\code{FALSE}, default) or should x be returned with transformed coordinates (\code{TRUE})?
##' @param na character specifying the NA action for missing coordinates. The "ignore" option ignores the coordinates and returns NAs to transformed coordinates. The "remove" option removes missing values from \code{x} returning a message while doing it. Any other character argument will tricker \code{na.fail} stopping the function in case of missing coordinates.
##' @return Returns a data frame with transformed spatial coordinates
##' @details If \code{x} is specified, the function guesses longitude and latitude columns from \code{x} by default.
##' @author Mikko Vihtakari
##' @import sp
##' @export

## Debug data
# x = stn; lon = NULL; lat = NULL; new.names = c("lon.utm", "lat.utm"); proj.og = "+proj=longlat +datum=WGS84"; proj.out = "+init=epsg:32633"; map.type = "panarctic"; verbose = FALSE; bind = FALSE; na = "ignore"

transform_coord <- function(x = NULL, lon = NULL, lat = NULL, new.names = c("lon.utm", "lat.utm"), proj.og = "+proj=longlat +datum=WGS84", proj.out = "+init=epsg:32633", map.type = NULL, verbose = FALSE, bind = FALSE, na = "ignore") {

  # Checks ----

    ## Case for defined x and undefined lon or/and lat
  if(!is.null(x) & (is.null(lon) | is.null(lat))) {
    if(class(x) != "data.frame") stop("x argument has to be a data.frame or NULL")

    if(is.null(lon)) {
      lon <- colnames(x)[grep("^lon$|longitude|long", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(x)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]

      if(verbose) message("lon argument not given, used ", lon, " from the given data frame")
    }

    if(is.null(lat)) {
      lat <- colnames(x)[grep("^lat$|latitude", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(x)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]

      if(verbose) message("lat argument not given, used ", lat, " from the given data frame")
    }

  }

  if(is.null(x) & (!is.numeric(lon) | !is.numeric(lat))) {
    stop("Define either x or lon and lat as numeric vectors")
  }

  ## Convert tibble to data.frame
  if(any(class(x) == "tbl")) {
    x <- as.data.frame(x)
  }

  ## Make the data frame ----

  if(is.null(x) & (is.numeric(lon) | is.numeric(lat))) {
    if(length(lon) != length(lat)) stop("lat and lon must be of equal length")
    y <- data.frame(lon = lon, lat = lat)
    lon <- "lon"; lat <- "lat"
  }

  if(!is.null(x)) {
    if(!is.data.frame(x)) stop("x must be a data frame")
    oldrownames <- rownames(x)
    rownames(x) <- 1:nrow(x)
    y <- x
  }

  ## NA action

  if(na == "ignore") {
      z <- y[is.na(y[[lon]]) | is.na(y[[lat]]), c(lon, lat)]
      y <- y[!is.na(y[[lon]]) | !is.na(y[[lat]]),]
  } else if(na == "remove") {
      y <- y[!is.na(y[[lon]]) | !is.na(y[[lat]]),]
      message("Removed rows that contained missing coordinates.")
  } else {
      stop("lon or lat coordinates contain missing values. Adjust the na argument or take care of the NAs.")
  }

  ## Coordinate transformation ----

  sp::coordinates(y) <- c(lon, lat)
  sp::proj4string(y) <- sp::CRS(proj.og) # original projection

  if(!is.null(map.type)) {
    proj.out <- map_projection(map_type(map.type)$map.type)
  }

  y <- sp::spTransform(y, sp::CRS(proj.out))
  y <- data.frame(sp::coordinates(y))

  ## ----

  if(na == "ignore" & nrow(z) > 0) {
    y <- rbind(y, z)
    y <- y[order(as.numeric(rownames(y))),]
  }

  if(!is.null(new.names)) {
    if(any(length(new.names) != 2, !is.character(new.names))) {
      stop("new.names must be a character vector with length of 2")
    }
    colnames(y) <- new.names
  }

  if(verbose) {
    message(paste("projection transformed from", proj.og, "to", proj.out))
  }

  ## Return ---

  if(bind) {
    out <- cbind(x, y)
    } else {
      out <- y
    }

  if(exists("oldrownames")) {
    rownames(out) <- oldrownames
    out
  } else {
    out
  }
}
