## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(fig.dpi = 96, collapse = TRUE, fig.width = 7, fig.height = 6, comment = "#>")

## ----message=FALSE, warning=FALSE----------------------------------------
library(PlotSvalbard)
basemap("svalbard")

## ----message=FALSE, warning=FALSE----------------------------------------
basemap("kongsfjorden") # or just basemap()

## ----message=FALSE, warning=FALSE----------------------------------------
basemap("kongsfjordbotn")

## ----message=FALSE, warning=FALSE----------------------------------------
basemap("kronebreen")

## ----message=FALSE, warning=FALSE----------------------------------------
basemap("mosj")

## ----message=FALSE, warning=FALSE----------------------------------------
basemap("barentssea")

## ----message=FALSE, warning=FALSE----------------------------------------
basemap("barentssea", limits = c(12, 24, 68, 71))

## ----message=FALSE, warning=FALSE----------------------------------------
basemap("arctic50")

## ----message=FALSE, warning=FALSE----------------------------------------
basemap("arctic60")

## ------------------------------------------------------------------------
basemap("svalbard", limits = c(11, 18, 77.9, 78.85)) # limits in decimal degrees

## ------------------------------------------------------------------------
basemap("arctic50", limits = c(3*10^6, -3*10^6, 3*10^6, -3*10^6)) # limits in uTM coordinates

## ------------------------------------------------------------------------
basemap(type = "svalbard", limits = c("kongsfjord_moorings", "Lon", "Lat"), limits.lon = 0.01, limits.lat = 0.01) + geom_point(data = kongsfjord_moorings, aes(x = lon.utm, y = lat.utm), color = "red")

## ------------------------------------------------------------------------
basemap("arctic60", bathymetry = TRUE)

## ------------------------------------------------------------------------
basemap("barentssea", bathymetry = TRUE)

## ------------------------------------------------------------------------
basemap("kongsfjorden", bathymetry = TRUE)

## ------------------------------------------------------------------------
basemap("kongsfjorden", bathymetry = TRUE, bathy.detailed = TRUE)

## ----bathystyle----------------------------------------------------------
basemap("barentssea", bathymetry = TRUE, bathy.style = "poly_blues") # default
basemap("barentssea", bathymetry = TRUE, bathy.style = "poly_greys") # grey polygons with shading
basemap("barentssea", bathymetry = TRUE, bathy.style = "contour_blues") # colored contours
basemap("barentssea", bathymetry = TRUE, bathy.style = "contour_grey") # grey contours

## ------------------------------------------------------------------------
basemap("barentssea", bathymetry = TRUE) + scale_fill_viridis_d("Water depth (m)")
basemap("barentssea", bathymetry = TRUE, bathy.style = "contour_blues") + scale_color_hue()


## ----currents------------------------------------------------------------
basemap("barentssea", bathymetry = TRUE, bathy.style = "poly_greys", currents = TRUE, current.alpha = 0.7)

## ----message=FALSE, warning=FALSE----------------------------------------
data("npi_stations")

x <- transform_coord(npi_stations, lon = "Lon", lat = "Lat", bind = TRUE)

basemap("svalbard", limits = c(3,24,78.5,82), round.lat = 1, round.lon = 2, 
  land.col = "#a9750d", gla.border.col = "grey95") + 
  geom_text(data = x, aes(x = lon.utm, y = lat.utm, 
    label = Station), color = "red", fontface = 2)

## ----message=FALSE, warning=FALSE----------------------------------------
data("kongsfjord_moorings")

basemap("kongsfjorden", limits = c(11.3, 12.69, 78.85, 79.1), round.lat = 0.05, round.lon = 0.5) + 
  geom_text(data = kongsfjord_moorings, aes(x = lon.utm, y = lat.utm, 
  label = Mooring.name, color = Name), fontface = 2, 
  size = 25.4/72.27*8) # font size = 8, see Graphical parameters


## ------------------------------------------------------------------------
data(zooplankton)

x <- transform_coord(zooplankton, lon = "Longitude", lat = "Latitude", bind = TRUE)

species <- colnames(x)[!colnames(x) %in% c("lon.utm", "lat.utm", "ID",
  "Longitude", "Latitude", "Total")]

library(scatterpie)

basemap("barentssea", limits = c(4, 24, 79.5, 83.5), round.lon = 2, round.lat = 1) + 
  geom_scatterpie(aes(x = lon.utm, y = lat.utm, group = ID, r = 100*Total), 
  data = x, cols = species, size = 0.1) + scale_fill_discrete(name = "Species", 
  breaks = species, labels = parse(text = paste0("italic(" , sub("*\\.", "~", species), ")")))

## ------------------------------------------------------------------------
data("chlorophyll")

x <- interpolate(chlorophyll, Subset = "From <= 10", value = "Chla") ## Interpolate

plot(x, legend.label = "Chlorophyll-a\n(mg/m3)") 


## ----message=FALSE, warning=FALSE----------------------------------------
basemap("svalbard", land.size = 0.01, gla.size = 0.05, grid.size = 0.05, 
  gla.border.col = "#52bfe4", land.border.col = "#a9750d")

## ----message=FALSE, warning=FALSE----------------------------------------
basemap("kongsfjorden", gla.border.col = "grey95", land.border.col = "#eeeac4")

## ------------------------------------------------------------------------
system.time(basemap("barentssea"))
system.time(basemap("svalbard"))
system.time(basemap("barentssea", limits = c(c(19.5,23.5,80,81.7))))
system.time(basemap("svalbard", limits = c(c(19.5,23.5,80,81.7))))

## ------------------------------------------------------------------------
library(ggplot2)
data("npi_stations")

dists <- dist2land(npi_stations, lon.col = "Lon", lat.col = "Lat", map.type = "svalbard")
dists$Area <- ordered(dists$Area, c("Kongsfjorden", "Framstrait", "Rijpfjorden"))

ggplot(dists, aes(x = Area, y = dist, label = Station, color = Area)) + 
  geom_text() + ylab("Distance to land (km)") + scale_color_hue()


## ----waffle--------------------------------------------------------------
library(reshape2)
data("zooplankton")

# Remove coordinates
x <- zooplankton[!names(zooplankton) %in% c("Longitude", "Latitude")]

## Make get the absolute values
x[!names(x) %in% c("ID", "Total")] <- (x[!names(x) %in% c("ID", "Total")]/100)*x$Total
x <- melt(x, id = c("ID", "Total"))

waffle_chart(x, fill = "variable", facet = "ID")

## ----waffle scaled, message=FALSE, warning=FALSE-------------------------
library(dplyr)

y <- x %>% group_by(ID) %>% summarise(sum = sum(value))

waffle_chart(x, fill = "variable", facet = "ID", composition = FALSE, max_value = max(y$sum))


## ----message=FALSE, warning=FALSE----------------------------------------
citation("PlotSvalbard")

