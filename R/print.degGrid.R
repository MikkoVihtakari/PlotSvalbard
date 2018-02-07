##' @title Print decimal degree grid (\code{degGrid}) objects
##' @description \code{\link{print}} function for \code{\link[=deg_grid]{degGrid}} objects
##' @param x \code{degGrid} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print degGrid
##' @keywords internal
##' @export
##' @author Mikko Vihtakari
##' @seealso \code{\link{deg_grid}} \code{\link{basemap}}

print.degGrid <- function(x, ...) {

  Range <- data.frame(lon.deg = range(x$lon$lon), lat.deg = range(x$lat$lat), lon.utm = range(x$lon$lon.utm), lat.utm = range(x$lat$lat.utm))

  lon_breaks <- as.numeric(as.character(x$lon.breaks$label))
  lat_breaks <- as.numeric(as.character(x$lat.breaks$label))
  
  cat("Decimal degree grid for maps", sep = "")
  cat(paste(" of class", class(x)), sep = "\n")
  cat("Object size: ", sep = "")
  print(object.size(x), unit = "auto")
  cat(NULL, sep = "\n")
  cat("List containing following elements:", sep = "\n")
  cat(names(x), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste("UTM projection:", x$utm.proj), sep = "\n")
  cat(paste("Decimal degree projection:", x$deg.proj), sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste(length(lon_breaks), "longitude breaks with", mean(diff(lon_breaks)), "decimal degree spacing"), sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste(length(lat_breaks), "latitude breaks with", mean(diff(lat_breaks)), "decimal degree spacing"), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Map boundaries:", sep = "\n")
  print(format(x$limits, justify = "left"), row.names = TRUE)
  cat(NULL, sep = "\n")
  cat("Grid boundaries:", sep = "\n")
  print(format(Range, justify = "left"), row.names = TRUE)
  }

