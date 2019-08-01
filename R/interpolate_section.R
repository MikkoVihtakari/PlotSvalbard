#' @title Interpolate x-y-z data for oceanographic section plots
#' @description Wrapper function which interpolates 3D data. Designed for oceanographic section plots.
#' @param df data frame. Can be \code{NULL} if \code{x, y, z} are given as numeric vectors
#' @param x,y,z coordinates as numeric vectors, if \code{df = NULL}. Otherwise characters specifying the column names of the coodrinates in \code{df}. The interpolation will be done as f(x, y) ~ z
#' @param method Character specifying the interpolation method. Options: "linear", "spline", "mba", and "idw"
#' @param format Character specifying the output format. Options: "ggplot" (for plotting with ggplot2), "image" (for plotting with the \code{\link[graphics]{image}} function) or "wide" for exporting as a wide data frame.
#' @param nx,ny Integer giving the dimensions for the output
#' @param scale_distances Logical indicating whether \code{x} and \code{y} should be scaled to equal distances. May be useful when interpolating variables that have different units or distances.
#' @param log_y logical indicating whether y axis should be interpolated using \code{log10(y + 1)} scale.
#' @param extrapolate logical indicating whether the interpolation function should be allowed to extrapolate beyond initial data.
#' @param na.rm logical indicating whether rows containing NA values should be omited from \code{df} or \code{x, y, z}. NA values are not permitted in most of the interpolation methods. See \code{\link[=oce]{sectionGrid}} for one way of interpolating NA values away from the input dataset.
#' @param return_df_names logical indicating whether the original column names should be returned instead of x, y, and z if \code{df != NULL}
#' @details This is a wrapper function which uses existing interpolation solutions. Following interpolation \code{method}s have been implemented:
#' \itemize{
#' \item \code{"linear"} for linear interpolation using the \code{\link[akima]{interp}} function with \code{linear = TRUE}.
#' \item \code{"spline"} for spline interpolation using the \code{\link[akima]{interp}} function with \code{linear = FALSE}.
#' \item \code{"mba"} for multilevel B-spline interpolation using the \code{\link[MBA]{mba.surf}} function. Appears to produce the best looking results. Recommended.
#' \item \code{"idw"} for inverse distance weighted interpolation using the \code{\link[gstat]{idw}} function. This function was originally developed for spatial data where x- and y axes are given in the same units and have similar effect on the result. While it would, no doubt, be possible to account section design in the model specification, this has not been done in the \code{interpolate_section} function. Therefore, using this option is not recommended.
#' }
#' @return Returns a data.frame with interpolated \code{x,y,z} coordinates.
#' @importFrom gstat idw
#' @importFrom akima interp
#' @importFrom MBA mba.surf
#' @importFrom scales rescale
#' @importFrom reshape2 melt dcast
#' @importFrom stats na.omit
#' @author Mikko Vihtakari
#' @export
# Add clip

# df = NULL; x = g$lat; y = g$lp; z = g$temp;
# df = g; x = "lat"; y = "lp"; z = "temp"
# method = "mba"; nx = 100; ny = 100; scale_distances = FALSE; log_y = FALSE; format = "ggplot"; extrapolate = FALSE; na.rm = TRUE; return_df_names = FALSE
interpolate_section <- function(df = NULL, x, y, z, method = "linear", format = "ggplot", nx = 100, ny = 100, scale_distances = FALSE, log_y = FALSE, extrapolate = FALSE, na.rm = TRUE, return_df_names = FALSE) {

  ## Tests ####

  if(is.null(df)) {
    if(!all(diff(c(length(x), length(y) ,length(z))) == 0)) stop("x, y, and z have to have equal length")
  } else {
    if(class(df) != "data.frame") stop("df has to be a data.frame")
  }

  ## Data ###

  if(is.null(df)) {
    dt <- data.frame(x = x, y = y, z = z)
  } else {
    dt <- df[c(x, y, z)]
    org_names <- names(dt)
    names(dt) <- c("x", "y", "z")
  }

  ## Remove NAs ##

  if(na.rm & any(is.na(dt))) dt <- stats::na.omit(dt)

  ## Scale & log transform ##

  if(scale_distances) {
    dt$x <- scales::rescale(dt$x)
    if(log_y) {
      dt$y <- scales::rescale(log(dt$y))
    } else {
      dt$y <- scales::rescale(dt$y)
    }
  } else if(log_y) {
    dt$y <- log10(dt$y+10)
  }


  ## Interpolation

  if(method == "linear") {

    out <- akima::interp(x = dt$x, y = dt$y, z = dt$z , xo = seq(min(dt$x), max(dt$x), length = nx), yo = seq(min(dt$y), max(dt$y), length = 100), extrap = extrapolate)

  } else if(method == "spline") {

    out <- akima::interp(x = dt$x, y = dt$y, z = dt$z , xo = seq(min(dt$x), max(dt$x), length = nx), yo = seq(min(dt$y), max(dt$y), length = 100), extrap = extrapolate, linear = FALSE)

  } else if(method == "idw") {

    sp::coordinates(dt) <- ~x+y

    grd <- expand.grid(x = seq(from = min(dt$x), to = max(dt$x), length.out = nx), y = seq(from = min(dt$y), to = max(dt$y), length.out = ny))  # expand points to grid

    sp::coordinates(grd) <- ~x+y
    sp::gridded(grd) <- TRUE

    intp <- gstat::idw(formula = z ~ 1, locations = dt, newdata = grd)

    intp_df <- as.data.frame(intp)

    out <- list(x = intp_df$x, y = intp_df$y, z = intp_df$var1.pred)

  } else if(method == "mba") {

    if(nx != ny) stop("The mba.surf function appears to manage only with square output matrices. Adjust the arguments such that nx == ny")

    out <- MBA::mba.surf(dt, no.X = nx, no.Y = ny, extend = extrapolate)

  } else {

    stop(paste(method, "is not an accepted interpolation method"))
  }

  ## Output format

  if(format == "ggplot") {

    if(method %in% c("linear", "spline")) {

      out_df <- reshape2::melt(out$z, na.rm = TRUE)
      names(out_df) <- c("x", "y", "z")
      out_df$x <- out$x[out_df$x]
      out_df$y <- out$y[out_df$y]

      if(!is.null(df) & return_df_names) {
        names(out_df) <- org_names
      }

      out_df

    } else if (method == "mba") {

      #colnames(out$xyz.est$z) <- out$xyz.est$x
      #rownames(out$xyz.est$z) <- out$xyz.est$y
      dimnames(out$xyz.est$z) <- list(out$xyz.est$x, out$xyz.est$y)

      out_df <- reshape2::melt(out$xyz.est$z, varnames = c("x", "y"), value.name = "z")
      #out_df <- out_df[c("x", "y", "z")]

      if(!is.null(df) & return_df_names) {
        names(out_df) <- org_names
      }

      out_df

    } else {

      data.frame(x = out$x, y = out$y, z = out$z)

    }

  } else if(format == "wide") {
    tmp <- data.frame(x = out$x, y = out$y, z = out$z)
    tmp <- reshape2::dcast(tmp, y ~ x, value.var = "z")
    ynams <- tmp[[1]]
    xnams <- as.numeric(names(tmp)[-1])
    tmp <- as.matrix(unname(tmp[-1]))
    colnames(tmp) <- xnams
    rownames(tmp) <- ynams
  } else {
    out
  }
}
