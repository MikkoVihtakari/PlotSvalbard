##' @title Print decimal degree grid (\code{basemapData}) objects
##' @description \code{\link{print}} function for \code{\link[=basemap]{basemapData}} objects
##' @param x \code{basemapData} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print basemapData
##' @export
##' @author Mikko Vihtakari
##' @seealso \code{\link{deg_grid}} \code{\link{basemap}}

print.basemapData <- function(x, ...) {

  #Range <- data.frame(lon.deg = range(x$lon$lon), lat.deg = range(x$lat$lat), lon.utm = range(x$lon$lon.utm), lat.utm = range(x$lat$lat.utm))

  cat("Basemap data object")
  cat(paste(" of class", class(x)), sep = "\n")
  cat("Object size: ", sep = "")
  print(object.size(x), unit = "Mb")
  cat(paste("UTM projection:", x$Grid$utm.proj), sep = "\n")
  cat(paste("Decimal degree projection:", x$Grid$deg.proj), sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("MapType: ", x$MapType, ". MapClass: ", x$MapClass))
  cat(paste0(". Glacier: ", !is.null(x$Glacier), ". Holes: ", !is.null(x$Holes)), sep = "\n")
  cat(NULL, sep = "\n")
  cat("List containing following elements:", sep = "\n")
  cat(names(x), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  cat("Land: SpatialPolygons object. Object size: ")
  print(object.size(x$Land), unit = "Mb")
  cat(paste("Projection:", sp::proj4string(x$Land)), sep = "\n")
  cat(NULL, sep = "\n")
  if(!is.null(x$Glacier)) {
    cat("Glacier: SpatialPolygons object. Object size: ")
    print(object.size(x$Glacier), unit = "Mb")
    cat(paste("Projection:", sp::proj4string(x$Glacier)), sep = "\n")
    cat(NULL, sep = "\n")
  }
  cat("Grid: ")
  print(x$Grid)
  #cat(paste(length(x$lon.breaks$deg), "longitude breaks with", mean(diff(x$lon.breaks$deg)), "decimal degree spacing"), sep = "\n")
  #cat(NULL, sep = "\n")
  #cat(paste(length(x$lat.breaks$deg), "latitude breaks with", mean(diff(x$lat.breaks$deg)), "decimal degree spacing"), sep = "\n")
  #cat(NULL, sep = "\n")
  #cat("Map boundaries:", sep = "\n")
  #print(format(x$boundaries, justify = "left"), row.names = TRUE)
  #cat(NULL, sep = "\n")
  #cat("Grid boundaries:", sep = "\n")
  #print(format(Range, justify = "left"), row.names = TRUE)
  }

