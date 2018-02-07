##' @title Print decimal degree grid (\code{degGridPolar}) objects
##' @description \code{\link{print}} function for \code{\link[=deg_grid_polar]{degGridPolar}} objects
##' @param x \code{degGridPolar} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print degGridPolar
##' @export
##' @author Mikko Vihtakari
##' @seealso \code{\link{deg_grid_polar}} \code{\link{basemap}}
##' @keywords internal

print.degGridPolar <- function(x, ...) {

  cat("Decimal degree grid for maps", sep = "")
  cat(paste(" of class", class(x)), sep = "\n")
  cat(paste("UTM projection:", x$utm.proj), sep = "\n")
  cat(paste("Decimal degree projection:", x$deg.proj), sep = "\n")
  cat(paste("Square map:", x$limits), sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste(length(x$lon$label), "longitude breaks with", 360/length(x$lon$label), "decimal degree spacing"), sep = "\n")
  cat(paste(length(x$lat.breaks$lat), "latitude breaks with", mean(diff(x$lat.breaks$lat)), "decimal degree spacing"), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Map boundaries:", sep = "\n")
  print(format(x$boundaries, justify = "left"), row.names = TRUE)
  cat(NULL, sep = "\n")
  }

