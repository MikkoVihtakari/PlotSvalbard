##' @title Interpolate a spatial surface from observations
##' @description Interpolates a x-y surface from a data frame of spatial observations for plotting. Currently uses the \code{\link[gstat]{idw}} function.
##' @param df data frame containing required information
##' @param value Character referring to the name of the value column to be used for interpolation.
##' @param Subset A subset argument as a character (i.e. with " ". See Examples).
##' @param unit The unit for \code{value} column. Not required.
##' @param bin.method Character giving the method for binning data, if there are several observations for each spatial point. Alternatives:
##' \itemize{
##' \item \strong{"none"} for no binning. Requires one value per unique coordinate.
##' \item \strong{"average"} for average values
##' \item \strong{"integrate"} for vertical trapezoidal integration using the \code{\link[oce]{integrateTrapezoid}} function from \link[oce]{oce} package.
##' }
##' @param int.method Character giving the method for interpolation. Currently only one option is implemented: "idw", inverse distance weighted interpolation using the \code{\link[gstat]{idw}} function.
##' @param coords A vector of column names for x (longitude) and y (latitude) coordinates, respectively. It is recommended to use UTM coordinates instead of decimal degrees. See \code{\link[sp]{coordinates}}.
##' @param station.col Character. Name of the column that specifies unique stations (i.e. spatial points). Required.
##' @param id.cols Character vector. Identification columns that should be preserved together with value, From, To and UTM coordinate columns
##' @param strata.col Character. Column that specify the sampling depth.
##' @param name.col Character. Column giving sample names. Not required.
##' @param shear Map tilting. Either NULL for non-tilted maps or a shear matrix, f.ex (matrix(c(2,1.2,0,1),2,2)) to shear the interpolation. This feature works poorly.
##' @param n.tile Number of horizontal and vertical tiles. Default is 100 resulting to 10000 tiles.
##' @param accuracy Number to which the extent of the interpolation area should be rounded. Given in meters.
##' @return Returns a \code{spatInt} object which is a list
##' @details The function removes missing values (NAs) from \code{value} column.
##'
##' A word of warning about \code{bin.method = "integrate"}: this functionality works only if samples have been taken consistently at same depths. In other cases, it is recommended to use \code{bin.method = "average"}, although the user should be careful in comparing samples taken from different depths in general. The unit for integrated value is [amount]/m2, if the original value was [amount]/m3.
##' @author Mikko Vihtakari
##' @examples data(chlorophyll) ## load an example dataset
##' x <- interpolate_spatial(chlorophyll, Subset = "From <= 10", value = "Chla") ## Interpolate
##' plot(x) ## Plot
##' @seealso \code{\link{plot.spatInt}} for plotting; \code{\link{interpolate_section}} for
##' @importFrom gstat idw
##' @importFrom oce integrateTrapezoid
##' @import sp
##' @export

## Test params
# df = chlorophyll; value = "Chla"; Subset = "From <= 10";  coords = c("lon.utm", "lat.utm"); station.col = "Station"; strata.col = "From"; name.col = NULL; id.cols = NULL; bin.method = "average"; int.method = "idw"; unit = NULL; shear = NULL; n.tile = 100; accuracy = 100
interpolate_spatial <- function(df, value, Subset = NULL, coords = c("lon.utm", "lat.utm"), station.col = "Station", strata.col = "From", name.col = NULL, id.cols = NULL, bin.method = "average", int.method = "idw", unit = NULL, shear = NULL, n.tile = 100, accuracy = 100) {

  output <- list()

  if(missing(value)) stop("value column must be given.")

  if(int.method != "idw") stop("Other methods are not incorporated yet")

if(is.null(Subset)) x <- df else x <- subset(df, eval(parse(text=Subset)))

x <- x[!is.na(x[value]),] ## Remove NAs
x <- x[c(station.col, id.cols, name.col, coords, strata.col, value)]

x <- x[order(x[station.col], x[strata.col]),] ## Order

if(any(duplicated(x[c(station.col, id.cols, name.col)])) & bin.method != "none") {
  tmp <- split(x, x[c(station.col, id.cols, name.col)], drop = TRUE)
  x <- lapply(tmp, function(k) {
    tp <- k[1,]
    tp[,name.col] <- paste(k[,name.col], collapse = " ")
    tp$From <- max(k[,strata.col])
    tp$To <- min(k[,strata.col])

    if(bin.method == "integrate" & length(unique(k[,strata.col])) <= 1) warning("Integration does not work for single depths. Select a wider depth subset or use bin.method = average")

    switch(bin.method,
      average = tp[,value] <- mean(k[,value]),
      integrate = tp[,value] <- oce::integrateTrapezoid(x = k[,strata.col], y = k[,value]),
      stop("bin.method has to be either 'average' or 'integrate'")
      )
  tp
    })
  x <- do.call(rbind, x)
  row.names(x) <- 1:nrow(x)
}

if(!is.null(shear)) {
  x[,coords] <- as.matrix(x[,coords]) %*% shear # Transform coordinates according to the shear matrix
  }

y <- x
y$Lon <- y[,coords[1]]
y$Lat <- y[,coords[2]]
y$value <- y[,value]

if(any(coords %in% c("Lat", "Lon"))) {
  y <- y[!colnames(y) %in% c(coords[!coords %in% c("Lat", "Lon")], value)]
 } else {
  y <- y[!colnames(y) %in% c(coords, value)]
}

sp::coordinates(x) <- coords

x.range <- as.numeric(range(x@coords[,1])) #*1.1  # min/max longitude of the interpolation area
x.range[1] <- floor(x.range[1] / accuracy) * accuracy
x.range[2] <- ceiling(x.range[2] / accuracy) * accuracy
y.range <- as.numeric(range(x@coords[,2]))#*1.1  # min/max latitude of the interpolation area
y.range[1] <- floor(y.range[1] / accuracy) * accuracy
y.range[2] <- ceiling(y.range[2] / accuracy) * accuracy

grd <- expand.grid(Lon = seq(from = x.range[1], to = x.range[2], length.out = n.tile), Lat = seq(from = y.range[1], to = y.range[2], length.out = n.tile))  # expand points to grid

sp::coordinates(grd) <- ~Lon + Lat
sp::gridded(grd) <- TRUE

idw <- gstat::krige(formula = get(value) ~ 1, locations = x, newdata = grd)

## Rename y columns

if(strata.col != "From") {
  y[[strata.col]] <- y$From
}

output$interpolation <- as.data.frame(idw)  # output is defined as a data table
output$data <- y
output$variables <- list(interpolated.variable = value, unit = unit, method = bin.method)

class(output) <- "spatInt"
output
}
