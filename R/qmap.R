#' @title Quick map
#' @description \code{qmap} is a shortcut similar to ggplot2's \code{\link[ggplot2]{qplot}} designed to quickly plot data with a limited range of options.
#' @param data Data frame to use.
#' @param x,y,... Aesthetics passed into each layer. Longitude and latitude columns are automatically recognized given that user has followed the PlotSvalbard standard naming. (ADD FUNCTION TO AUTOMATICALLY TRANSFORM DATA)
#' @param map.type Type of map area. Options: "panarctic", "barentssea", "svalbard", "mosj", "kongsfjorden", "kongsfjordbotn", "kronebreen", or "rijpfjorden". See the \code{type} argument in \code{\link{basemap}}.
#' @param geom Character vector specifying geom(s) to draw. Defaults to "point".
#' @param limits Map limits. See the \code{limits} argument in \code{\link{basemap}}. If "auto" the limits are automatically selected using the second option in the list.
#' @param bathymetry Logical indicating whether bathymetry should be added to the map.
#' @param bathy.style Character defining the style for bathymetry contours. See the \code{bathy.style} argument in \code{\link{basemap}}.
#' @import ggplot2
#' @author Mikko Vihtakari
#' @export

# x = NULL; y = NULL; map.type = "panarctic"; geom = "point"; limits = "auto"; bathymetry = FALSE; bathy.style = "poly_blues"
qmap <- function(data, x = NULL, y = NULL, map.type = "panarctic", geom = "point", limits = "auto", bathymetry = FALSE, bathy.style = "poly_blues", ...) {

    if(is.null(x)) {
      x <- colnames(data)[grep("^lon utm$|longitude utm|^long utm$", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(data)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
      guess.x <- TRUE
    } else {
      guess.x <- FALSE
    }

    if(is.null(y)) {
      y <- colnames(data)[grep("^lat utm$|latitude utm", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(data)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
      guess.y <- TRUE
    } else {
      guess.y <- FALSE
    }

  ## If the transformed coordinates were not found, transform

  if(is.na(x)) {
    x <- colnames(data)[grep("^lon$|longitude|^long$", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(data)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
    trans.x <- TRUE
  } else {
    trans.x <- FALSE
  }

  if(is.na(y)) {
    y <- colnames(data)[grep("^lat$|latitude", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(data)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
    trans.y <- TRUE
  } else {
    trans.y <- FALSE
  }

  ## Transform if the range of lon | lat is suitable

  if(max(data[,get("x")]) < 180 & min(data[,get("x")]) > -180) {
    trans.x <- TRUE
  }

  if(max(data[,get("y")]) < 90 & min(data[,get("y")]) > 40) {
    trans.y <- TRUE
  }

  ## Inform about the guessing.

  if(guess.x) {
    message(paste("Using", x, "as longitude column"))
  }

  if(guess.y) {
    message(paste("Using", y, "as latitude column"))
  }

  ### Transform

  if(trans.x | trans.y) {
    data[,c(get("x"), get("y"))] <- transform_coord(x = data, lon = x, lat = y, new.names = c(x, y), map.type = map.type)
    message(paste0("Transforming the coordinates from decimal degrees to those used by the ", map.type, " map"))
  }

  ## Limits

  if(any(limits == "auto")) {

    lims <- c(get("x"), get("y"))
    limits <- auto_limits(type = map.type, limits = lims, data = data)
  }

  ## Base map

  pb <- basemap(type = map.type, limits = limits, bathymetry = bathymetry, bathy.style = bathy.style)

  ## Geoms

  # geom_arguments <- list(
  #   ...,
  #   color = "red",
  #   shape = 21
  # )
  #
  # geom_arguments <- geom_arguments[!duplicated(names(geom_arguments))]


  if(geom == "point") {
    pb + geom_point(data = data, aes(x = get(x), y = get(y)), ...)
  } else {
    stop("Other geom than point have not been implemented yet.")
  }

}
