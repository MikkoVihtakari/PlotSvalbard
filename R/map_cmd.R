#' @title Return map elements for basemap
#' @description An internal function to make \code{\link{basemap}} code more readable
#' @param command basemap layer to be added
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function. Common users do not need to worry about these details. Basemap elements can added together using this function, \code{\link[base]{parse}} and \code{\link[base]{eval}}.
#' @examples ## An example for utm map without glaciers or bathymetry
#' \dontrun{eval(parse(text=paste(map_cmd("base"), map_cmd("land_utm"), 
#' map_cmd("grid_utm"), map_cmd("defs_utm"), sep = "+")))}
#' @keywords internal
#' @export
#' @import ggplot2
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}}

map_cmd <- function(command) {
  switch(command,
    base_dat = 'basemap_data(type = type, limits = limits, round.lat. = round.lat, n.lat.grid. = n.lat.grid, round.lon. = round.lon, n.lon.grid. = n.lon.grid, keep.glaciers. = keep.glaciers)',
    base_dat_polar = 'basemap_data(type = type, limits = limits, lat.interval. = lat.interval, lon.interval. = lon.interval, keep.glaciers. = FALSE)',
    base = 'ggplot(data=X$Land, aes(x=long, y=lat))',
    bathy = 'geom_polygon(data = bathy, aes(x = long, y = lat, group = group, fill = depth)) + scale_fill_brewer(name = "Depth (m)")',
    land_utm = 'geom_polygon(data = X$Land, aes(x = long, y = lat, group = group), fill = land.col, color = border.col.land, size = size.land)',
    glacier_utm = 'geom_polygon(data = X$Glacier, aes(x = long, y = lat, group = group), fill = gla.col, color = border.col.glacier, size = size.glacier) + geom_polygon(data = X$Holes, aes(x=long, y=lat, group = group), fill = land.col, color = border.col.glacier, size = size.land)',
    grid_utm = 'geom_line(data = X$Grid$lat, aes(x = lon.utm, y=lat.utm, group = ID), color = grid.col, size = size.grid) + geom_line(data = X$Grid$lon, aes(x=lon.utm, y=lat.utm, group = ID), color = grid.col, size = size.grid)',
    defs_utm = 'scale_y_continuous(name = "Latitude (decimal degrees)", breaks = X$Grid$lat.breaks$lat_utm, labels = X$Grid$lat.breaks$label) + scale_x_continuous(name = "Longitude (decimal degrees)", breaks = X$Grid$lon.breaks$lon_utm, labels = X$Grid$lon.breaks$label) + coord_fixed(xlim = c(X$Grid$limits$lon.utm[1], X$Grid$limits$lon.utm[2]), ylim = c(X$Grid$limits$lat.utm[1], X$Grid$limits$lat.utm[2]), expand = FALSE) + theme_map()',
    land_polar = 'geom_polygon(data = X$Land, aes(x = long, y = lat, group = group), fill = land.col, color = border.col.land, size = size.land)',
    grid_polar = 'geom_path(data = X$Grid$lat, aes(x = lon.utm, y=lat.utm, group = ID), color = grid.col, size = size.grid) + geom_segment(data = X$Grid$lon, aes(x = lon.start, xend = lon.end, y = lat.start, yend = lat.end, group = label), color = grid.col, size = size.grid)',
    labels_polar = 'geom_text(data = X$Grid$lon, aes(x = label.offset*lon.end, y = label.offset*lat.end, angle = angle, label = paste(label, "^o", sep = "")), size = label.font/2.845276, parse = TRUE) + geom_text(data = X$Grid$lat.breaks, aes(x = lon.utm, y = lat.utm, label = paste(label, "^o", sep = "")), hjust = 0, vjust = 0, size = label.font/2.845276, parse = TRUE)',
    labels_polar_limits = 'scale_y_continuous(name = "UTM latitude (km)", labels = formatterUTMkm) + scale_x_continuous(name = "UTM longitude (km)", labels = formatterUTMkm)',
    defs_polar_limits = 'coord_fixed(xlim = c(X$Grid$boundaries$lon.utm[1], X$Grid$boundaries$lon.utm[2]), ylim = c(X$Grid$boundaries$lat.utm[1], X$Grid$boundaries$lat.utm[2]), expand = FALSE) + theme_map()',
    defs_polar = 'geom_path(data = X$Grid$lat[X$Grid$lat$ID == levels(X$Grid$lat$ID)[which.min(as.numeric(gsub("[[:alpha:]]", "", levels(X$Grid$lat$ID))))],], aes(x = lon.utm, y=lat.utm, group = ID), color = border.col.land, size = size.land) + coord_fixed() + theme_void()',
    remove_labels = 'theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())',
    interpl_surface = 'geom_tile(data = x$interpolation, aes(x = Lon, y = Lat, fill = var1.pred)) + geom_contour(data = x$interpolation, aes(x = Lon, y = Lat, z = var1.pred), color = "black", size = 0.2)',
    defs_interpl_utm = 'scale_fill_gradientn(name = legend.label, colours = colorRamps::matlab.like(7), limits = col.scale.limits) + scale_y_continuous(name = "Latitude (decimal degrees)", breaks = X$Grid$lat.breaks$lat_utm, labels = X$Grid$lat.breaks$label) + scale_x_continuous(name = "Longitude (decimal degrees)", breaks = X$Grid$lon.breaks$lon_utm, labels = X$Grid$lon.breaks$label) + coord_fixed(xlim = c(X$Grid$limits$lon.utm[1], X$Grid$limits$lon.utm[2]), ylim = c(X$Grid$limits$lat.utm[1], X$Grid$limits$lat.utm[2]), expand = FALSE) + theme_map()',
    stop(paste("map command", command, "not found."))
)
}

formatterUTMkm <- function(x){
    x/1000
}
