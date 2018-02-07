##' @title Print decimal degree grid (\code{basemapLimits}) objects
##' @description \code{\link{print}} function for \code{\link[=basemap_limits]{basemapLimits}} objects
##' @param x \code{basemapLimits} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print basemapLimits
##' @keywords internal
##' @export
##' @author Mikko Vihtakari
##' @seealso \code{\link{basemap_limits}} \code{\link{deg_grid}} \code{\link{basemap}}

print.basemapLimits <- function(x, ...) {

  #Range <- data.frame(lon.deg = range(x$lon$lon), lat.deg = range(x$lat$lat), lon.utm = range(x$lon$lon.utm), lat.utm = range(x$lat$lat.utm))

  cat("Basemap limits object")
  cat(paste(" of class", class(x)), sep = "\n")
  cat("Object size: ", sep = "")
  print(object.size(x), unit = "auto")
  cat("List containing following elements:", sep = "\n")
  cat(names(x), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  
  cat("Map limits", sep = "\n")
  cat("Original: ")
  cat(x$limits_og[1:2], sep = "-")
  cat(" longitude, ")
  cat(x$limits_og[3:4], sep = "-")
  cat(" longitude.", sep ="\n")
  cat("Decimal degree: ")
  cat(x$limits_dd[1:2], sep = "-")
  cat(" longitude, ")
  cat(x$limits_dd[3:4], sep = "-")
  cat(" longitude.", sep ="\n")
  cat("UTM: ")
  cat(x$limits_utm[1:2], sep = "-")
  cat(" longitude, ")
  cat(x$limits_utm[3:4], sep = "-")
  cat(" longitude.", sep ="\n")
  cat(NULL, sep = "\n")
  
  cat("Map shape boundaries", sep = "\n")
  cat("Decimal degree: ")
  cat(x$bound_dd[1:2], sep = "-")
  cat(" longitude, ")
  cat(x$bound_dd[3:4], sep = "-")
  cat(" longitude.", sep ="\n")
  cat("UTM: ")
  cat(x$bound_utm[1:2], sep = "-")
  cat(" longitude, ")
  cat(x$bound_utm[3:4], sep = "-")
  cat(" longitude.", sep ="\n")
  cat(NULL, sep = "\n")
  
  cat("Contains limits and boundaries above as shapefiles: ")
  cat(grep("shp", names(x), value = TRUE), sep = ", ")
  cat(NULL, sep = "\n")
  cat(paste("UTM projection:", x$proj_utm), sep = "\n")
  cat(paste("Decimal degree projection:", x$proj_deg), sep = "\n")
  }

