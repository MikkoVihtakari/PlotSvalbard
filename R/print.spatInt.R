##' @title Print \code{spatInt} objects
##' @description \code{\link{print}} function for \code{\link[=interpolate]{spatInt}} objects
##' @param x \code{spatInt} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print spatInt
##' @export
##' @author Mikko Vihtakari
##' @seealso \code{\link{interpolate}}

print.spatInt <- function(x, ...) {

  title <- "Spatially interpolated 2D surface"

  title2 <- "List structure with following elements:"

  title3 <- paste0("$interpolation")
  text1 <- paste("Spatial interpolation grid with", nrow(x$interpolation), "rows and", ncol(x$interpolation), "columns. Range of variables is")

  Range <- data.frame(Lon = range(x$interpolation$Lon), Lat = range(x$interpolation$Lat), var1.pred =  range(x$interpolation$var1.pred), row.names = c("min", "max"))
  text2 <- paste("where var1.pred is the interpolated value for", x$variables$interpolated.variable, "with unit", x$variables$unit)

  title4 <- paste0("$data")

  cat(title, sep = "\n")
  cat(title2, sep = "\n")
  cat(NULL, sep = "\n")
  cat(title3, sep = "\n")
  cat(NULL, sep = "\n")
  cat(text1, sep = "\n")
  print(format(Range, justify = "left"), row.names = TRUE)
  cat(NULL, sep = "\n")
  cat(text2, sep = "\n")
  cat(NULL, sep = "\n")
  cat(title4, sep = "\n")
  print(format(x$data, justify = "left"), row.names = TRUE)
  }

