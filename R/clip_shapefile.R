##' @title Clip a shape file (SpatialPolygon) using a bounding area
##' @description Clips an area from a larger shape file (\link[sp]{SpatialPolygons}).
##' @param x Original shape file to be clipped. Required. Must contain \code{\link[sp]{proj4string}} information.
##' @param limits The constraining area used to clip \code{x}. Required. Either a numeric vector of length 4 or a \link[sp]{SpatialPolygons} object. The first element of the numeric vector defines the minimum longitude, second element the maximum longitude, third element the minimum latitude and fourth element the maximum latitude of the bounding box. The \link[sp]{SpatialPolygons} object must contain \code{\link[sp]{proj4string}} information. See details.
##' @param proj4.limits The \code{\link[sp]{proj4string}} projection attributes for \code{limits}. Defaults to decimal degrees (see **Usage**).
##' @param simplify Should the \code{x} geometry be simplified before clipping? Useful to make the function faster for large shape files. Uses \code{\link[rgeos]{gSimplify}} function.
##' @param tol Numerical tolerance value to be used for simplification. See \code{\link[rgeos]{gSimplify}}.
##' @details The function uses the \code{\link[rgeos]{gIntersection}} function to clip smaller \link[sp]{SpatialPolygons} from larger ones. The clip area is constrained by either a numeric vector or \link[sp]{SpatialPolygons} object in the \code{limits} argument. One of these arguments must be given. Defining \code{limits} by a \link[sp]{SpatialPolygons} object gives greater freedom for the clip area as the area does not have to be rectangular.
##' @import sp rgdal
##' @importFrom rgeos gIntersection
##' @importFrom rgeos gSimplify
##' @author Mikko Vihtakari
##' @export

# Test parameters
#x = readOGR(paste0(devel, "worldland/ne_10m_land.shp"))
#x = svalbard.ld
#limits = c(15, 20, 77, 79)
#limits = NULL
#limiting.polygon = NULL
#limiting.polygon = kong.cr
#proj4.limits = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

clip_shapefile <- function(x, limits = NULL, proj4.limits = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", simplify = FALSE, tol = 60) {

if(is.null(x)) stop("x, the original shape file must be supplied")
if(is.null(limits)) stop("Either limits or limiting.polygon must be supplied")

if(class(limits) == "SpatialPolygonsDataFrame" | class(limits) == "SpatialPolygons") {
  proj4.limits <- sp::proj4string(limits)
  clip_boundary <- limits
} else {
  if(!is.numeric(limits)) stop("limits have to be numeric, SpatialPolygonsDataFrame or SpatialPolygons object")
  if(is.numeric(limits) & length(limits) != 4) stop("the length of limits vector has to be 4. See limits argument")
  clip_boundary <- sp::Polygon(matrix(c(limits[1], limits[3], limits[1], limits[4], limits[2], limits[4], limits[2], limits[3], limits[1], limits[3]), ncol = 2, byrow = TRUE))
  clip_boundary <- sp::SpatialPolygons(list(sp::Polygons(list(clip_boundary), ID = "clip_boundary")), proj4string=CRS(proj4.limits))
}

x_proj <- sp::proj4string(x)

if(is.na(x_proj)) stop("proj4string for x is missing. Define the projection attributes and try again.")

if(proj4.limits != x_proj) {
  clip_boundary <- sp::spTransform(clip_boundary,CRS(x_proj))
}

if(simplify) {
  x <- rgeos::gSimplify(x, tol = tol)
}

rgeos::gIntersection(x, clip_boundary, byid=TRUE)

}
