% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_plot.R
\name{ts_plot}
\alias{ts_plot}
\title{Temperature-salinity (T-S) plot from a data frame using ggplot2}
\usage{
ts_plot(
  dt,
  temp_col = "temp",
  sal_col = "sal",
  WM = kongsfjord_watermasses,
  color_wmpoly = "grey30",
  xlim = NULL,
  ylim = NULL,
  color = "watertype",
  zoom = TRUE,
  nlevels = 6,
  color_isopyc = "grey90",
  symbol_shape = 1,
  symbol_size = 3,
  symbol_alpha = 0.6,
  color_scale = NULL,
  color_var_name = NULL,
  margin_distr = FALSE,
  margin_width = 0.15,
  margin_height = 0.2,
  plot_data = TRUE,
  base_size = 10
)
}
\arguments{
\item{dt}{data frame}

\item{temp_col}{character specifying the column name that contains temperature information. Should be potential temperatures (theta) instead of in-situ tempretatures, if default \code{WM} is used.}

\item{sal_col}{character specifying the column name that contains salinity information.}

\item{WM}{dataframe containing the water type definitions for polygons boundaries for water types. Set to \code{NULL} to remove the polygon boundaries. See Details in \code{\link{define_water_type}} for further details.}

\item{color_wmpoly}{character defining the color for water mass polygon edges.}

\item{xlim}{numeric vector specifying th limits for x-axis. If \code{NULL}, the \code{zoom} argument is used.}

\item{ylim}{numeric vector specifying th limits for y-axis. If \code{NULL}, the \code{zoom} argument is used.}

\item{color}{character indicating the mapping variable for color. Either a single character from \code{\link[grDevices]{colors}}, "watertype" or column name in \code{dt}. If \code{"watertype"}, the symbols are colored based on water type using a prededined palette. If the character specifies a valid color, that color is used for the data symbols.}

\item{zoom}{logical indicating whether the x- and y-axis should be limited to data. If \code{FALSE}, the entire water mass diagram from the \code{WM} data frame is shown.}

\item{nlevels}{number of automatically-selected isopycnal levels. Set to 0 to remove isopycnals.}

\item{color_isopyc}{character defining the color for isopycnals.}

\item{symbol_shape}{character or numeric. ggplot syntax for shape of plotting symbols. See \code{\link[ggplot2]{geom_point}}}

\item{symbol_size}{numeric. ggplot syntax for size of symbol. See \code{\link[ggplot2]{geom_point}}}

\item{symbol_alpha}{numeric value between 0 and 1 defining the transparency of symbols. Set to 1 to remove transparency. See \code{\link[ggplot2]{geom_point}}}

\item{color_scale}{named vector giving all \code{WM$abb} levels (names) and their corresponding colors. See the \code{values} argument in \code{\link[ggplot2]{scale_colour_manual}}}

\item{color_var_name}{character giving the name that should be used in legend of color scale. See the \code{name} argument in \code{\link[ggplot2]{scale_colour_manual}}}

\item{margin_distr}{logical indicating whether kernel density estimates of temperature and salinity should be added to margins of the plot.}

\item{margin_width, margin_height}{numeric specifying the width and height of the x- and y-axis margin plots as a propotion of the plot width ("npc" from \code{\link[grid]{unit}}).}

\item{plot_data}{logical indicating whether salinity and temperature data should be plotted. \code{FALSE} returns an empty T-S plot frame allowing further customization using ggplot2 syntax.}

\item{base_size}{Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.}
}
\description{
Makes a T-S plot from salinity and temperature data.
}
\examples{
# Using example data

dt <- data.frame(temp = c(1, -1.1, -0.8, 3.2), sal = c(34, 34.5, 34.9, 33.9))
ts_plot(dt)

# Using real CTD data

data("ctd_kongsfjord")

ts_plot(ctd_kongsfjord)
ts_plot(ctd_kongsfjord, zoom = FALSE) # show all of WM
ts_plot(ctd_kongsfjord, color = "area", margin_distr = TRUE) # scaling color to "area"
ts_plot(ctd_kongsfjord, color = "red", margin_distr = TRUE) # no color scaling

# Using a different WM

data("ctd_rijpfjord")

ts_plot(ctd_rijpfjord, WM = rijpfjord_watermasses)

# example of graphical parameter modification
ts_plot(ctd_rijpfjord, WM = rijpfjord_watermasses, symbol_shape = 16, symbol_size = 0.1, 
symbol_alpha = 0.8, margin_distr = TRUE, xlim = c(32, 35), color = "area") 
}
\seealso{
\code{\link{define_water_type}}, the \link{kongsfjord_watermasses} data.frame ,the \link{rijpfjord_watermasses} data.frame

Other ts_plot: 
\code{\link{ctd_kongsfjord}},
\code{\link{define_water_type}()},
\code{\link{kongsfjord_watermasses}},
\code{\link{rijpfjord_watermasses}}
}
\author{
Mikko Vihtakari with help from \href{https://stackoverflow.com/questions/17370853/align-ggplot2-plots-vertically/17371177#17371177}{baptiste} to align marginal plots.
}
\concept{ts_plot}
