##' @title Interpolate a spatial 2D surface from observations
##' @description Interpolates a 2D surface from a data.frame of observations for plotting. Currenly uses the \code{\link[gstat]{krige}} function.
##' @param df dataframe containing required information
##' @param value Name of the value column to be used for interpolation
##' @param Subset A subset argument as a name (i.e. with " ").
##' @param bin.method Method for binning data, if there are several observations for each spatial point. Either "average" for average values or "integrate" for \link[oce::integrateTrapezoid]{trapezoid integration}.
##' @param int.method Method for interpolation. Currently only \code{\link[gstat]{krige}}.
##' @param coords A vector of column names for x (longitude) and y (latitude) coordinates, respectively. It is recommended to use UTM coordinates instead of decimal degrees. See \code{\link[sp]{coordinates}}.
##' @param station.col Name of the column that specifies unique stations (i.e. spatial points). Required.
##' @param id.cols Identification columns that should be preserved together with value, From, To and UTM coodinate columns
##' @param strata.col Columns that specify the sampling depth. Should be in order: 1) sample from ("From"), 2) sample to ("To"). "To" can be NA.
##' @param name.col Column giving sample names. Not required.
##' @param shear Map tilting. Either NULL for non-tilted maps or a shear matrix, f.ex (matrix(c(2,1.2,0,1),2,2)) to shear the interpolation.
##' @param n.tile Number of horizontal and vertical tiles. Default is 100 resulting to 10000 tiles.
##' @param accuracy Number to which the extent of the interpolation should be rounded.
##' @return Returns a \code{spatInt} object which is a list
##' @details The function removes missing values (NAs) from \code{value} column. Both \code{strata.cols} have to be specified. If the sample was from one depth, place that depth to "From" column and leave "To" column empty (NA).
##' @author Mikko Vihtakari
##' ##' @examples data(chlorophyll) ## load an example dataset
##' x <- interpolate(chlorophyll, Subset = "From <= 10", value = "Chla") ## Interpolate
##' plot(x, type = "kongsfjorden") ## Plot
##' @seealso \code{\link{plot.spatInt}}
##' @importFrom gstat krige
##' @importFrom oce integrateTrapezoid
##' @export

## Test params
  # df <- chl
  # Subset <- NULL
  # bin.method = "average"
  # int.method = "krige"
  # coords = c("lon.utm", "lat.utm")
  # value = "Chla"
  # name.col = "Name"
  # station.col <- "Station"
  # id.cols <- NULL
  # strata.col = "From"
  # shear = NULL
  # n.tile = 100
  # accuracy = 100

interpolate <- function(df, value, Subset = NULL, coords = c("lon.utm", "lat.utm"), station.col = "Station", strata.col = "From", name.col = NULL, id.cols = NULL, bin.method = "average", int.method = "krige", shear = NULL, n.tile = 100, accuracy = 100) {

  output <- list()

  if(int.method != "krige") stop("Other methods are not incorporated yet")

if(is.null(Subset)) x <- df else x <- subset(df, eval(parse(text=Subset)))

x <- x[!is.na(x[value]),] ## Remove NAs
x <- x[c(station.col, id.cols, name.col, coords, strata.col, value)]

if(!any(colnames(x) %in% "To")) { ## Add "To" column
  x$To <- NA
}

x <- x[c(station.col, id.cols, name.col, coords, strata.col, "To", value)]
x <- x[order(x[station.col], x[strata.col]),] ## Order

if(any(duplicated(x[station.col]))) {
  tmp <- split(x, x[station.col], drop = TRUE)
  x <- lapply(tmp, function(k) {
    tp <- k[1,]
    tp[,name.col] <- paste(k[,name.col], collapse = " ")
    tp$From <- max(k[,strata.col])
    tp$To <- min(k[,strata.col])

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

coordinates(x) <- coords

x.range <- as.numeric(range(x@coords[,1])) #*1.1  # min/max longitude of the interpolation area
x.range[1] <- floor(x.range[1] / accuracy) * accuracy
x.range[2] <- ceiling(x.range[2] / accuracy) * accuracy
y.range <- as.numeric(range(x@coords[,2]))#*1.1  # min/max latitude of the interpolation area
y.range[1] <- floor(y.range[1] / accuracy) * accuracy
y.range[2] <- ceiling(y.range[2] / accuracy) * accuracy

grd <- expand.grid(Lon = seq(from = x.range[1], to = x.range[2], length.out = n.tile), Lat = seq(from = y.range[1], to = y.range[2], length.out = n.tile))  # expand points to grid

coordinates(grd) <- ~Lon + Lat
gridded(grd) <- TRUE

idw <- gstat::krige(formula = get(value) ~ 1, locations = x, newdata = grd)

output$interpolation <- as.data.frame(idw)  # output is defined as a data table
output$data <- y
output$variables <- data.frame(interpolated.variable = value, unit = NA)

class(output) <- "spatInt"
output
}
