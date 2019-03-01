#' @title Make an oceanographic section plot from an x-y-z data frame
#' @description Creates an oceanographic section plot from a data frame using ggplot2. Returns either an interpolated section or a bubble plot.
#' @param df Data frame containing the data
#' @param x Character specifying the column name which should be used as x (=distance) axis
#' @param y Character specifying the column name which should be used as y (=depth) axis. The y axis will be inverted in the resulting plot.
#' @param z Character specifying the column name which should be for bubbles or interpolation fill depending on the \code{interpolate} argument.
#' @param bottom Optional character specifying the column name which contains bottom depths OR a data frame with x-axis values in the first column and bottom depths in the second column. If provided, unique combinations of \code{x} and \code{bottom} will be used to plot bottom for the section plot
#' @param interpolate Logical indicating whether an interpolated section plot (\code{TRUE}) or a bubble plot (\code{FALSE}) should be returned.
#' @param interp_method (\code{interpolate = TRUE} only). Character specifying the interpolation method. See \code{\link{interpolate_section}}.
#' @param log_y logical indicating whether the y-axis should be \code{log10(y + 10)} transformed before plotting. Helps showing differences close to the surface if some stations are much deeper than others.
#' @param xlab,ylab,zlab Character specifying the labels for the x-axis, y-axis and legend, respectively.
#' @param xbreaks,ybreaks,zbreaks Numeric vector specifying the breaks for the x-axis, y-axis and legend. See \link[ggplot2]{scale_continuous}.
#' @param contour (\code{interpolate = TRUE} only). Numeric vector defining breaks for contour lines. Use \code{NULL} not to plot the contour lines.
#' @param contour_label_cex Numeric giving the \code{cex} (=size) parameter for contour labels. See \code{\link[directlabels]{geom_dl}}.
#' @param contour_color Character defining the color to be used for contour lines and labels.
#' @param xlim,ylim Numeric vector of length two providing limits of the scale. Use NA to refer to the existing minimum or maximum. Use \code{NULL} for default limits.
#' @param zlim Numeric vector of length two providing limits for fill or bubble size. Any values outside these limits will get the extreme values defined by \code{zrange} (using the \code{\link[scales]{squish}} function).
#' @param zscale (\code{interpolate = TRUE} only). Character specifying the color scale for interpolation tile fill. Either one of the \code{\link[ggplot2]{scale_colour_viridis_c}} \code{option} alternatives ("A", "B", "C", or "D") or "gradient2" for a \code{\link[ggplot2]{scale_colour_gradient2}} (red-white-blue) color scale. Note that you can use the ggplot2 color scale arguments to adjust the color scales as you want. Just place them inside the \code{section_plot} function.
#' @param zcolor (\code{interpolate = FALSE} only). Character specifying the color of bubbles. Use column name in \code{df} to scale a variable to bubble color (not implemented yet, here as a reminder).
#' @param add_bottom Numeric vector of length two providing the depths that should be added to \code{bottom} at the extremes (\code{\link[base]{range}}) of \code{xbreaks}. Useful for extending the plot giving space for graphical elements.
#' @param sampling_indicator (\code{interpolate = TRUE} only). Character giving the style of sampling indicator. Alternatives:
#' \itemize{
#' \item \code{"lines"} A dashed line reaching from data start depth to end depth at each station. Recommended for CTD sections.
#' \item \code{"points"} Points indicating the vertical and horizontal location of sample. Recommended for water samples.
#' \item \code{"ticks"} A black tick mark above the plot indicating the horizontal location of each station. A way to avoid clutter.
#' \item \code{"none"} No sampling indicator will be plotted.
#' }
#' @param legend.position Position for the ggplot legend. See the argument with the same name in \link[ggplot2]{theme}.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @param ... Additional arguments passed to color and size scales. See \code{\link[ggplot2]{scale_colour_gradient2}}, \code{\link[ggplot2]{scale_colour_viridis_c}} and \code{\link[ggplot2]{scale_size}}.
#' @details Note that you can use the ggplot2 color and size scale arguments to adjust the scales as you want. Just place them inside the \code{section_plot} function. See \code{\link[ggplot2]{scale_colour_gradient2}}, \code{\link[ggplot2]{scale_colour_viridis_c}} and \code{\link[ggplot2]{scale_size}}.
#' @return Returns either an interpolated section (\code{\link[ggplot2]{geom_tile}}) or a bubble (\code{\link[ggplot2]{geom_point}}) ggplot2 object.
#' @importFrom directlabels geom_dl
#' @importFrom dplyr summarise group_by
#' @importFrom scales squish
#' @importFrom magrittr %>%
#' @import ggplot2
#' @author Mikko Vihtakari
#' @examples
#' # Bubble plots
#' section_plot(df = chlorophyll[grepl("KpN.|Kb[0-4]", chlorophyll$Station),], x = "lon", y = "From",
#'   z = "Chla")
#'
#' # Interpolated CTD sections
#' ## Standard temperature section plot. Difficult to see surface due to large differences in y-scale
#' section_plot(ctd_rijpfjord, x = "dist", y = "pressure", z = "temp", bottom = "bdepth",
#' interpolate = TRUE)
#'
#' ## Logarithmic y axis
#' section_plot(ctd_rijpfjord, x = "dist", y = "pressure", z = "temp", bottom = "bdepth",
#' interpolate = TRUE, log_y = TRUE)
#'
#' ## Contour lines
#' section_plot(ctd_rijpfjord, x = "dist", y = "pressure", z = "temp", bottom = "bdepth",
#' interpolate = TRUE, log_y = TRUE, contour = c(-1.8, 0, 1, 3))
#' @export

# Test parameters
# df = g; x = "lat"; y = "pressure"; z = "temp"; interpolate = TRUE; interp_method = "mba"; bottom = "bdepth"; log_y = TRUE; xlab = "Distance"; ylab = "Depth (m, log scale)"; zlab = "Temperature"; ybreaks = c(0, 20, 50, 100, 200, 500, 1000, 2000, 4000); xbreaks = seq(80,82.5,0.5); add_bottom = c(0,4000); base_size = 10; zbreaks = c(-1, -0.5, 1, 3); zlim = c(-2,4)
# df = g; x = "dist"; y = "depth"; z = "no"; bottom = bottom; interpolate = FALSE; interp_method = "mba"; log_y = TRUE; xlab = NULL; ylab = "Depth (dbar, log scale)"; zlab = expression(NO[x]); ybreaks = c(0, 20, 50, 100, 200, 500, 1000, 2000, 4000); xbreaks = c(0, 20, 50, 100, 150, 200, 250, 280); zbreaks = waiver(); zlim = NULL; ylim = NULL; add_bottom = c(0,4000); legend.position = "right"; base_size = 10
# df = ctd_rijpfjord; x = "dist"; y = "pressure"; z = "temp"; bottom = "bdepth"; interpolate = TRUE; log_y = TRUE; add_bottom = NULL
#
# Fix:  add option to define the sampling range/location
section_plot <- function(df, x, y, z, bottom = NULL, interpolate = FALSE, interp_method = "mba", log_y = FALSE, xlab = "Distance", ylab = "Depth", zlab = "Variable", ybreaks = waiver(), xbreaks = waiver(), zbreaks = waiver(), contour = NULL, contour_label_cex = 0.8, contour_color = "white", xlim = NULL, ylim = NULL, zlim = NULL, zscale = "viridis", zcolor = "black", add_bottom = NULL, sampling_indicator = "lines", legend.position = "right", base_size = 10, ...) {

## Setup and tests


## Log_y

if(log_y) df[[y]] <- log10(df[[y]] + 10)

## Bottom ###

if(!is.null(bottom)) {

  if(!any(class(bottom) %in% c("data.frame", "character"))) stop("bottom argument has to be a character or data.frame. See Arguments.")

if(any(class(bottom) %in% "data.frame")) {
  if(ncol(bottom) < 2) stop("bottom data.frame has to contain at least two columns. See Arguments.")
  bd <- unique(bottom[1:2])
} else {
  bd <- unique(df[c(x, bottom)])
}

names(bd) <- c("x", "y")
if(!is.null(add_bottom)) bd <- rbind(bd, data.frame(x = range(xbreaks), y = add_bottom))
bd <- bd[order(bd$x),]

if(log_y) bd$y <- log10(bd$y + 10)

}


## Interpolation
if(interpolate) {
  dt <- interpolate_section(df = df, x = x, y = y, z = z, method = interp_method)

  samples <- df %>% group_by(get(x)) %>% summarise(min = min(get(y)), max = max(get(y)))
  names(samples) <- c("x", "min", "max")
} else {

## Bubbles

  dt <- df[c(x, y, z)]
  names(dt) <- c("x", "y", "z")
}

## Parameters ###

if(log_y) {
  yzero <- 0.98
  ytick.lim <- 0.95

  if(class(ybreaks) == "waiver") {
    ybreaks <- pretty_log(10^pretty(range(dt$y), n = 10) - 10)
    ybreaks_actual <- log10(ybreaks + 10)
  } else {
    ybreaks_actual <- log10(ybreaks + 10)
  }



} else {
  yzero <- -2
  ytick.lim <- -diff(range(df[[y]]))*0.02

  if(class(ybreaks) == "waiver") {
    ybreaks_actual <- waiver()
  } else {
    ybreaks_actual <- ybreaks
  }
}

# Add ≥ and ≤ signs to legend

if(!is.null(zlim)) {

  if(class(zbreaks) == "waiver") {
      zbreaks <- pretty(zlim, n = 4)
  }

  zlabels <- zbreaks

  if(any(df[[z]] > zlim[2])) {
    zlabels[length(zlabels)] <- paste0(">", zlabels[length(zlabels)])
  }

  if(any(df[[z]] < zlim[1])) {
    zlabels[1] <- paste0("<", zlabels[1])
  }

} else {
   if(class(zbreaks) == "waiver") {
    zlabels <- waiver()
   } else {
    zlabels <- zbreaks
  }


}


## Plot ####

if(interpolate) {

  p <- ggplot() +
    geom_tile(data = dt, aes(x = x, y = y, fill = z, color = z)) + {
      if(!is.null(contour)) geom_contour(data = dt, aes(x = x, y = y, z = z), color = contour_color, size = LS(0.5), breaks = contour)
    } + {
      if(!is.null(contour)) {
        directlabels::geom_dl(data = dt, aes(x = x, y = y, z = z, label = ..level..),
          method = list("bottom.pieces", cex = contour_label_cex, vjust = 0.5),
          stat = "contour", color = contour_color, breaks = contour)
      }
    } + {
     if(sampling_indicator == "lines") geom_segment(data = samples, aes(x = x, xend = x, y = min, yend = max), size = LS(0.5), color = "grey", linetype = 2)
    } + {
     if(sampling_indicator == "points") geom_point(data = df, aes(x = get(x), y = get(y)), size = contour_label_cex, color = "black")
    } + {
     if(sampling_indicator == "ticks") geom_segment(data = samples, aes(x = x, xend = x, y = ytick.lim, yend = yzero), size = LS(1), color = "black")
    } + {
      if(!is.null(bottom)) geom_ribbon(data = bd, aes(x = x, ymax = Inf, ymin = y), fill = "grey90")
      } +
    scale_y_reverse(name = ylab, breaks = ybreaks_actual, labels = ybreaks, limits = ylim, expand = c(0.03, 0)) +
    scale_x_continuous(name = xlab, breaks = xbreaks, limits = xlim, expand = c(0, 0)) +
    theme_classic(base_size = base_size) +
    theme(legend.position = legend.position,
      legend.key.size = unit(0.8,"line"),
      legend.spacing.y = unit(0.1,"line"),
      legend.title = element_text(size = 0.8*base_size),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank())

  ## Color scales

  if(zscale == "gradient2") {

    p + scale_fill_gradient2(name = zlab, na.value = "white", limits = zlim, breaks = zbreaks, labels = zlabels, oob = scales::squish, ...) +
    scale_colour_gradient2(name = zlab, na.value = "white", limits = zlim, breaks = zbreaks, labels = zlabels, oob = scales::squish, ...)

  } else {

    p + scale_fill_viridis_c(option = zscale, name = zlab, na.value = "white", limits = zlim, breaks = zbreaks, labels = zlabels, oob = scales::squish, ...) +
    scale_colour_viridis_c(option = zscale, name = zlab, na.value = "white", limits = zlim, breaks = zbreaks, labels = zlabels, oob = scales::squish, ...)

  }


} else {

  ggplot() + {
    if(!is.null(bottom)) geom_ribbon(data = bd, aes(x = x, ymax = Inf, ymin = y), fill = "grey90")
    } +
    geom_point(data = dt, aes(x = x, y = y, size = z), pch = 21, stroke = LS(0.5), color = zcolor) +
    scale_size_area(name = zlab, breaks = zbreaks, labels = zlabels, oob = scales::squish, ...) + #limits = zlim,
    scale_y_reverse(name = ylab, breaks = ybreaks_actual, labels = ybreaks, limits = ylim, expand = c(0.03, 0)) +
    scale_x_continuous(name = xlab, breaks = xbreaks, expand = c(0, 0)) + #, limits = xlim
    coord_cartesian(xlim = xlim, ylim = ylim, clip = "off") +
    theme_classic(base_size = base_size) +
    theme(legend.position = legend.position,
      legend.key.size = unit(0.8,"line"),
      legend.spacing.y = unit(0.1,"line"),
      legend.title = element_text(size = 0.8*base_size),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank())
}

## ####

}
