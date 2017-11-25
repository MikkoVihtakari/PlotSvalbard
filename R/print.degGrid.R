##' @title Print decimal degree grid (\code{degGrid}) objects
##' @description \code{\link{print}} function for \code{\link[=deg_grid]{degGrid}} objects
##' @param x \code{degGrid} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print degGrid
##' @export
##' @author Mikko Vihtakari
##' @seealso \code{\link{deg_grid}} \code{\link{basemap}}

print.degGrid <- function(x, ...) {

  Range <- data.frame(lon.deg = range(x$lon$lon), lat.deg = range(x$lat$lat), lon.utm = range(x$lon$lon.utm), lat.utm = range(x$lat$lat.utm))

  cat("Decimal degree grid for maps", sep = "\n")
  cat(paste("of class", class(x)), sep = "\n")
  cat(paste("UTM projection:", x$utm.proj), sep = "\n")
  cat(paste("decial degree projection:", x$deg.proj), sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste(length(x$lon.breaks$deg), "longitude breaks with", mean(diff(x$lon.breaks$deg)), "decimal degree spacing"), sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste(length(x$lat.breaks$deg), "latitude breaks with", mean(diff(x$lat.breaks$deg)), "decimal degree spacing"), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Map boundaries:", sep = "\n")
  print(format(x$boundaries, justify = "left"), row.names = TRUE)
  cat(NULL, sep = "\n")
  cat("Grid boundaries:", sep = "\n")
  print(format(Range, justify = "left"), row.names = TRUE)
  }

