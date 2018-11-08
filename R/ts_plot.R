#' @title Temperature-salinity (T-S) plot from a data frame using ggplot2
#' @description Makes a T-S plot from salinity and temperature data. 
#' @param dt data frame
#' @param sal_col character specifying the column name that contains salinity information.
#' @param temp_col character specifying the column name that contains temperature information. Should be potential temperatures (theta) instead of in-situ tempretatures, if default \code{WM} is used.
#' @param WM dataframe containing the water type definitions for polygons boundaries for water types. Set to \code{NULL} to remove the polygon boundaries. See Details in \code{\link{define_water_type}} for further details.
#' @param color_wmpoly character defining the color for water mass polygon edges. 
#' @param color character indicating the mapping variable for color. Either a single character from \code{\link[grDevices]{colors}}, "watertype" or column name in \code{dt}. If \code{"watertype"}, the symbols are colored based on water type using a prededined palette. If the character specifies a valid color, that color is used for the data symbols.
#' @param xlim numeric vector specifying th limits for x-axis. If \code{NULL}, the \code{zoom} argument is used.
#' @param ylim numeric vector specifying th limits for y-axis. If \code{NULL}, the \code{zoom} argument is used.
#' @param zoom logical indicating whether the x- and y-axis should be limited to data. If \code{FALSE}, the entire water mass diagram from the \code{WM} data frame is shown.
#' @param margin_distr logical indicating whether kernel density estimates of temperature and salinity should be added to margins of the plot.
#' @param margin_width,margin_height numeric specifying the width and height of the x- and y-axis margin plots as a propotion of the plot width ("npc" from \code{\link[grid]{unit}}).
#' @param nlevels number of automatically-selected isopycnal levels. Set to 0 to remove isopycnals.
#' @param color_isopyc character defining the color for isopycnals. 
#' @param symbol_shape character or numeric. ggplot syntax for shape of plotting symbols. See \code{\link[ggplot2]{geom_point}}
#' @param symbol_size numeric. ggplot syntax for size of symbol. See \code{\link[ggplot2]{geom_point}}
#' @param symbol_alpha numeric value between 0 and 1 defining the transparency of symbols. Set to 1 to remove transparency. See \code{\link[ggplot2]{geom_point}}
#' @param color_scale named vector giving all \code{WM$abb} levels (names) and their corresponding colors. See the \code{values} argument in \code{\link[ggplot2]{scale_colour_manual}}
#' @param color_var_name character giving the name that should be used in legend of color scale. See the \code{name} argument in \code{\link[ggplot2]{scale_colour_manual}}
#' @param plot_data logical indicating whether salinity and temperature data should be plotted. \code{FALSE} returns an empty T-S plot frame allowing further customization using ggplot2 syntax.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @seealso \code{\link{define_water_type}}, the \link{kongsfjord_watermasses} data.frame ,the \link{rijpfjord_watermasses} data.frame
#' @family ts_plot
#' @author Mikko Vihtakari with help from \href{https://stackoverflow.com/questions/17370853/align-ggplot2-plots-vertically/17371177#17371177}{baptiste} to align marginal plots.
#' @import ggplot2
#' @importFrom gtable gtable_add_cols gtable_add_grob gtable_add_rows
#' @importFrom grid grid.newpage grid.draw
#' @importFrom cowplot get_legend
#' @importFrom oce swSTrho swRho
#' @importFrom grDevices colors
#' @examples # Using example data
#'
#' dt <- data.frame(temp = c(1, -1.1, -0.8, 3.2), sal = c(34, 34.5, 34.9, 33.9))
#' ts_plot(dt)
#'
#' # Using real CTD data
#'
#' data("ctd_kongsfjord")
#'
#' ts_plot(ctd_kongsfjord)
#' ts_plot(ctd_kongsfjord, zoom = FALSE) # show all of WM
#' ts_plot(ctd_kongsfjord, color = "area", margin_distr = TRUE) # scaling color to "area"
#' ts_plot(ctd_kongsfjord, color = "red", margin_distr = TRUE) # no color scaling
#'
#' # Using a different WM
#'
#' data("ctd_rijpfjord")
#'
#' ts_plot(ctd_rijpfjord, WM = rijpfjord_watermasses)
#' 
#' # example of graphical parameter modification
#' ts_plot(ctd_rijpfjord, WM = rijpfjord_watermasses, symbol_shape = 16, symbol_size = 0.1, 
#' symbol_alpha = 0.8, margin_distr = TRUE, xlim = c(32, 35), color = "area") 
#' @export

## ####

# dt <- ctd; temp_col = "theta"; sal_col = "salinity"; xlim = NULL; ylim = NULL; color = "watertype"; zoom = TRUE; margin_distr = FALSE; nlevels = 4
# dt = ctd_kongsfjord; temp_col = "temp"; sal_col = "sal"; WM = kongsfjord_watermasses; xlim = NULL; ylim = NULL; color = "watertype"; zoom = FALSE; margin_distr = TRUE; nlevels = 6; symbol_shape = 1; symbol_size = 3; symbol_alpha = 0.6; plot_data = TRUE; color_scale = NULL; color_wmpoly = "grey30"; color_isopyc = "grey90"; base_size = 10
# dt <- ctd_rijpfjord; temp_col = "theta"; sal_col = "salinity"; WM = rijpfjord_watermasses; xlim = NULL; ylim = NULL; color = "watertype"; zoom = TRUE; margin_distr = FALSE; nlevels = 6; symbol_shape = 1; symbol_size = 3; symbol_alpha = 0.6; color_scale = NULL; color_var_name = NULL; plot_data = TRUE

ts_plot <- function(dt, temp_col = "temp", sal_col = "sal", WM = kongsfjord_watermasses, color_wmpoly = "grey30", xlim = NULL, ylim = NULL, color = "watertype", zoom = TRUE, nlevels = 6, color_isopyc = "grey90", symbol_shape = 1, symbol_size = 3, symbol_alpha = 0.6, color_scale = NULL, color_var_name = NULL, margin_distr = FALSE, margin_width = 0.15, margin_height = 0.2, plot_data = TRUE, base_size = 10) {

## Definitions ####

if(color %in% colors()) {
  scale2color <- FALSE
} else if(!is.null(color)) {
  scale2color <- TRUE
} else {
  scale2color <- FALSE
  color <- "black"
}

## Water types ###
if(!is.null(WM)) {
  dt <- define_water_type(dt, temp_col = temp_col, sal_col = sal_col, WM = WM, bind = TRUE)

  wm_order <- as.character(unique(WM$abb))
  WM$abb <- factor(WM$abb, levels = rev(wm_order))
}

## Axis limits

if(is.null(xlim) & zoom) {
  xbreaks <- pretty(range(dt[[sal_col]]), n = nlevels)
  xlim <- range(xbreaks)

    if(max(dt[[sal_col]]) < 35.15 & xlim[2] == 36) {
    xlim <- c(xlim[1], 35.15)
  }

} else if(is.null(xlim)) {
  xbreaks <- pretty(range(c(dt[[sal_col]], c(32, 35))), n = nlevels)
  xlim <- range(xbreaks)

    if(max(dt[[sal_col]]) < 35.15 & xlim[2] == 36) {
    xlim <- c(xlim[1], 35.15)
  }

} else {
  xbreaks <- pretty(xlim)
}

if(is.null(ylim) & zoom) {
  ybreaks <- pretty(range(dt[[temp_col]]), n = nlevels)
  ylim <- range(ybreaks)
} else if(is.null(ylim)) {
  ybreaks <- pretty(range(c(dt[[temp_col]], c(-2, 8))), n = nlevels)
  ylim <- range(ybreaks)
} else {
  ybreaks <- pretty(ylim)
}

## Isopycnals

if(nlevels > 0) {

  if(zoom) {
    rho <- oce::swRho(salinity = xlim, temperature = ylim, pressure = rep(1, length(xlim))) - 1000
    rho_breaks <- pretty(range(rho), n = nlevels, min.n = nlevels %/% 2)
  } else {
    rho <- oce::swRho(salinity = xlim, temperature = ylim, pressure = rep(1, length(xlim))) - 1000
    rho_breaks <- pretty(range(rho), n = nlevels-1)
    #rho_breaks <- seq(10, 30, length.out = nlevels)
  }

  temp_breaks <- seq(from = ylim[1], to = ylim[2], by = 0.1)

  isopycs <- lapply(seq_along(rho_breaks), function(i) {
    data.frame(temp = temp_breaks, rho = rho_breaks[i], sal = oce::swSTrho(temperature = temp_breaks, density = rho_breaks[i], pressure = 1))
  })

  isopycs <- do.call(rbind, isopycs)

} else {
  isopycs <- data.frame(temp = NA, rho = NA, sal = NA)
}

## Water mass polygons and text

if(!is.null(WM)) {
  TMP <- lapply(wm_order, function(j) {
  tmp <- subset(WM, abb == j)

  poly <- data.frame(abb = tmp$abb, x = c(tmp$sal.min, tmp$sal.max, tmp$sal.max, tmp$sal.min), y = c(tmp$temp.min, tmp$temp.min, tmp$temp.max, tmp$temp.max))

  text <- data.frame(abb = tmp$abb, x = tmp$sal.min, y = tmp$temp.max)

  list(poly = poly, text = text)
  })


WMpoly <- do.call(rbind, lapply(TMP, function(j) j$poly))
WMtext <- do.call(rbind, lapply(TMP, function(j) j$text))

if(zoom) {

  WMpoly <- WMpoly[WMpoly$abb %in% as.character(unique(dt$watertype)),]
  WMpoly <- droplevels(WMpoly)

  WMtext <- WMtext[WMtext$abb %in% as.character(unique(dt$watertype)),]
  WMtext <- droplevels(WMtext)

  WMtext$y <- ifelse(WMtext$y > ylim[2], ylim[2], WMtext$y)
  WMtext$y <- ifelse(WMtext$y < ylim[1], ylim[1], WMtext$y)
  WMtext$x <- ifelse(WMtext$x > xlim[2], xlim[2], WMtext$x)
  WMtext$x <- ifelse(WMtext$x < xlim[1], xlim[1], WMtext$x)
}
}

## Main plot ####

## Water mass polygons

if(!is.null(WM)) {
   p <- ggplot(data = dt, aes_string(x = sal_col, y = temp_col, color = color), shape = symbol_shape, alpha = symbol_alpha, size = symbol_size) +
    geom_polygon(data = WMpoly, aes(x = x, y = y, group = abb), fill = "white", color = color_wmpoly, size = LS(0.5)) +
    scale_y_continuous(expression(paste("Potential temperature (", degree, "C", ")", sep = "")), breaks = ybreaks) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_classic(base_size = base_size) +
    theme(axis.line = element_line(size = LS(0.5)),
      axis.ticks = element_line(size = LS(0.5)),
      panel.border = element_rect(color = "black", size = LS(1), fill = NA),
      legend.background = element_blank())

} else {
  p <- ggplot(data = dt, aes_string(x = sal_col, y = temp_col, color = color), shape = symbol_shape, alpha = symbol_alpha, size = symbol_size) +
    scale_y_continuous(expression(paste("Potential temperature (", degree, "C", ")", sep = "")), breaks = ybreaks) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_classic(base_size = base_size) +
    theme(axis.line = element_line(size = LS(0.5)),
      axis.ticks = element_line(size = LS(0.5)),
      panel.border = element_rect(color = "black", size = LS(1), fill = NA),
      legend.background = element_blank())
}

## Isopycals

if(nlevels > 0) {
  p <- p +
    scale_x_continuous("Practical salinity", breaks = xbreaks,
    sec.axis = sec_axis(~., breaks = isopycs[isopycs$temp == ylim[2], "sal"], labels = isopycs[isopycs$temp == ylim[2], "rho"], name = "Density")) +
    geom_line(data = isopycs, aes(x = sal, y = temp, group = rho), color = color_isopyc, size = LS(0.5))
} else {
  p <- p +
    scale_x_continuous("Practical salinity", breaks = xbreaks)
}

## Data points ###

if(plot_data) {
  if(scale2color) {
    p <- p +
    geom_point(data = dt, aes_string(x = sal_col, y = temp_col, color = color), shape = symbol_shape, alpha = symbol_alpha, size = symbol_size)
  } else {
    p <- p +
    geom_point(data = dt, aes_string(x = sal_col, y = temp_col), shape = symbol_shape, alpha = symbol_alpha, size = symbol_size, color = color)
  }
}

## Water mass labels

if(!is.null(WM)) {
  p <- p + geom_text(data = WMtext, aes(x = x, y = y, label = abb), size = FS(base_size*0.8), vjust = 1.2, hjust = -0.1, color = color_wmpoly)
}

######################
## Marginal plots ####

if(margin_distr & plot_data) {

## Marginal plot for x-axis

  if(scale2color) {
    px <- ggplot(data = dt, aes_string(x = sal_col, fill = color)) +
    geom_density(alpha = 0.5, size = 0.2) +
    coord_cartesian(xlim = xlim, expand = FALSE) +
    theme_classic(base_size = base_size) +
    theme(axis.title = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      legend.position = "none")

  } else {
    px <- ggplot(data = dt, aes_string(x = sal_col)) +
    geom_density(alpha = 0.5, size = 0.2, fill = color) +
    coord_cartesian(xlim = xlim, expand = FALSE) +
    theme_classic(base_size = base_size) +
    theme(axis.title = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      legend.position = "none")

  }


## Marginal plot for y-axis

  if(scale2color) {
    py <- ggplot(data = dt, aes_string(x = temp_col, fill = color)) +
      geom_density(alpha = 0.5, size = 0.2) +
      coord_flip(xlim = ylim, expand = FALSE) +
      theme_classic(base_size = base_size) +
      theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")
  } else {
    py <- ggplot(data = dt, aes_string(x = temp_col)) +
      geom_density(alpha = 0.5, size = 0.2, fill = color) +
      coord_flip(xlim = ylim, expand = FALSE) +
      theme_classic(base_size = base_size) +
      theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")

  }


}


## Change the color scale for data points

if(scale2color & (!is.null(color_scale) | color == "watertype") & plot_data) {

  if(is.null(color_scale) & color == "watertype") {
    
    if(any(WM$abb %in% "TAW")) {
      color_scale <- c("AWs" = "#D44F56", "SWs" = "#649971", "ArWs" = "#3881AC", "AW" = "#B6434A", "AIW" = "#056A89", "TAW" = "#FF5F68", "IW" = "#82C893", "SW" = "#517D5B", "WCW" = "#B27DA6", "ArW" = "#449BCF", "PSW" = "#449BCF", "Other" = "grey50")
    } else {
      color_scale <- c("AWs" = "#D44F56", "SWs" = "#649971", "ArWs" = "#3881AC", "AW" = "#FF5F68", "AIW" = "#056A89", "IW" = "#82C893", "SW" = "#517D5B", "WCW" = "#B27DA6", "ArW" = "#449BCF", "PSW" = "#449BCF", "Other" = "grey50")
    }
    
    color_var_name <- "Water type"
  } else {
    if(is.null(color_var_name)) color_var_name <- color
  }

 p <- p + scale_color_manual(name = color_var_name, values = color_scale)

 if(margin_distr) {
 px <- px + scale_fill_manual(name = color_var_name, values = color_scale)
 py <- py + scale_fill_manual(name = color_var_name, values = color_scale)
  }

}

## Finally plotting ####

if(margin_distr & plot_data) {
  
  if(color %in% names(dt) | color == "watertype") {
    legend <- cowplot::get_legend(p)
    legend$vp$x <- unit(.9, 'npc')
    legend$vp$y <- unit(.9, 'npc')
  }
  
  g <- ggplot2::ggplotGrob(p + theme(legend.position = "none"))

  panel_id <- g$layout[g$layout$name == "panel",c("t","l")]

  g <- gtable::gtable_add_cols(g, unit(margin_width,"npc"))
  g <- gtable::gtable_add_grob(g, ggplot2::ggplotGrob(py), t = panel_id$t, l = ncol(g))

  g <- gtable::gtable_add_rows(g, unit(margin_height,"npc"), 0)
  g <- gtable::gtable_add_grob(g, ggplot2::ggplotGrob(px), t = 1, l = panel_id$l)

  grid::grid.newpage()
  grid::grid.draw(g)
  
  if(color %in% names(dt) | color == "watertype") {
    grid::grid.draw(legend)
  }
  
} else {
  p
}

}
