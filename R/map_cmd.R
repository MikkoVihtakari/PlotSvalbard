#' @title Return map elements for basemap
#' @description An internal function to make \code{\link{basemap}} code more readable
#' @param command basemap layer to be added
#' @param alternative logical to return alternative formmatting in certain cases. Used to reduce \code{if}-\code{else} statements in \code{\link{basemap}}.
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function. Common users do not need to worry about these details. Basemap elements can added together using this function, \code{\link[base]{parse}} and \code{\link[base]{eval}}.
#' @examples ## An example for utm map without glaciers or bathymetry
#' \dontrun{eval(parse(text=paste(map_cmd("base"), map_cmd("land_utm"),
#' map_cmd("grid_utm"), map_cmd("defs_utm"), sep = "+")))}
#' @keywords internal
#' @export
#' @import ggplot2
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}}

map_cmd <- function(command, alternative = FALSE) {
  out <- switch(command,
    base_dat = '
      basemap_data(type = type, limits = limits, round.lat. = round.lat, n.lat.grid. = n.lat.grid, round.lon. = round.lon, n.lon.grid. = n.lon.grid, keep.glaciers. = keep.glaciers)
    ',
    base_dat_polar = '
      basemap_data(type = type, limits = limits, lat.interval. = lat.interval, lon.interval. = lon.interval, keep.glaciers. = FALSE)
    ',
    base = '
      ggplot(data=X$Land, aes(x=long, y=lat))
    ',
    bathy_pb = '
      geom_polygon(data = bathy, aes(x = long, y = lat, group = group, fill = depth), show.legend = bathy.legend, color = bathy.border.col, size = bathy.size) +
      scale_fill_manual(name = "Depth (m)", values = colorRampPalette(c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#08306B"))(nlevels(bathy$depth)), guide =
        if(bathy.legend) {
          guide_legend(order = 1, override.aes = list(colour = NA))
        } else {
          FALSE
        })
    ',
    bathy_pg = '
      geom_polygon(data = bathy, aes(x = long, y = lat, group = group, fill = depth), show.legend = bathy.legend, color = bathy.border.col, size = bathy.size) +
      scale_fill_grey("Depth (m)", start = 1, end = 0.7, guide =
        if(bathy.legend) {
          guide_legend(order = 1, override.aes = list(colour = NA))
        } else {
          FALSE
        })
      ',
    bathy_cg = '
      geom_polygon(data = bathy, aes(x = long, y = lat, group = group, color = depth), fill = NA, color = bathy.border.col, size = bathy.size)
    ',
    bathy_cb = '
      geom_polygon(data = bathy, aes(x = long, y = lat, group = group, color = depth), show.legend = bathy.legend, fill = NA, size = land.size) +
      scale_color_manual(name = "Depth (m)", values = colorRampPalette(c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#08306B"))(nlevels(bathy$depth)))
    ',
    land_utm = '
      geom_polygon(data = X$Land, aes(x = long, y = lat, group = group), fill = land.col, color = land.border.col, size = land.size)
    ',
    glacier_utm = '
      geom_polygon(data = X$Glacier, aes(x = long, y = lat, group = group), fill = gla.col, color = gla.border.col, size = gla.size) +
      geom_polygon(data = X$Holes, aes(x=long, y=lat, group = group), fill = land.col, color = gla.border.col, size = land.size)
    ',
    grid_utm = '
      geom_line(data = X$Grid$lat, aes(x = lon.utm, y=lat.utm, group = ID), color = grid.col, size = grid.size, na.rm = TRUE) +
      geom_line(data = X$Grid$lon, aes(x=lon.utm, y=lat.utm, group = ID), color = grid.col, size = grid.size, na.rm = TRUE)
    ',
    defs_utm = '
      scale_y_continuous(breaks = X$Grid$lat.breaks$lat_utm, labels = X$Grid$lat.breaks$label) +
      scale_x_continuous(breaks = X$Grid$lon.breaks$lon_utm, labels = X$Grid$lon.breaks$label) +
      labs(y = "Latitude (decimal degrees)", x = "Longitude (decimal degrees)") +
      coord_fixed(xlim = c(X$Grid$limits$lon.utm[1], X$Grid$limits$lon.utm[2]), ylim = c(X$Grid$limits$lat.utm[1], X$Grid$limits$lat.utm[2]), expand = FALSE) +
      theme_map(base_size = base_size) +
      theme(legend.margin=margin(t = 0.2, b = 0, unit="cm"), legend.position = legend.position)
    ',
    currents_utm = if(alternative) {
      '
      geom_path(data = cur, aes(x = long, y = lat, group = group, size = size, color = type), alpha = current.alpha, arrow = arrow(type = "open", angle = 15, ends = "last", length = unit(0.3, "lines")), show.legend = current.legend, linejoin = "mitre") +
      scale_color_manual(name = "Current\ntype", values = c("Arctic" = arc.col, "Atlantic" = atl.col), guide =
        if(current.legend) {
          guide_legend(order = 2, override.aes = list(fill = NA))
        } else {
          FALSE
        }) +
      scale_size(name = "Current\nsize", range = c(LS(0.5), LS(3)), breaks = c(1, 2, 4, 8, 14), labels = c("Intermittent", "Minor", "Medium", "Major", "Main"), guide =
        if(current.legend) {
          guide_legend(order = 3, override.aes = list(fill = NA))
        } else {
          FALSE
        })'
    } else {'
      geom_path(data = cur, aes(x = long, y = lat, group = group, color = type), alpha = current.alpha, size = current.size, arrow = arrow(type = "open", angle = 15, ends = "last", length = unit(0.3, "lines")), show.legend = current.legend) +
      scale_color_manual(name = "Current\ntype", values = c("Arctic" = arc.col, "Atlantic" = atl.col), guide = guide_legend(order = 2, override.aes = list(fill = NA)))
      '},
    land_polar = '
      geom_polygon(data = X$Land, aes(x = long, y = lat, group = group), fill = land.col, color = land.border.col, size = land.size)
    ',
    grid_polar = '
      geom_path(data = X$Grid$lat, aes(x = lon.utm, y=lat.utm, group = ID), color = grid.col, size = grid.size, na.rm = TRUE) +
      geom_segment(data = X$Grid$lon, aes(x = lon.start, xend = lon.end, y = lat.start, yend = lat.end, group = label), color = grid.col, size = grid.size, na.rm = TRUE)
    ',
    labels_polar = '
      geom_text(data = X$Grid$lon, aes(x = label.offset*lon.end, y = label.offset*lat.end, angle = angle, label = paste(label, "^o", sep = "")), size = FS(label.font), parse = TRUE) +
      geom_text(data = X$Grid$lat.breaks, aes(x = lon.utm, y = lat.utm, label = paste(label, "^o", sep = "")), hjust = 0, vjust = 0, size = FS(label.font), parse = TRUE)
    ',
    labels_polar_limits = '
      scale_y_continuous(labels = formatterUTMkm) +
      scale_x_continuous(labels = formatterUTMkm) +
      labs(x = "Longitude (km)", y = "Latitude (km)")
    ',
    defs_polar_limits = '
      coord_fixed(xlim = c(X$Grid$boundaries$lon.utm[1], X$Grid$boundaries$lon.utm[2]), ylim = c(X$Grid$boundaries$lat.utm[1], X$Grid$boundaries$lat.utm[2]), expand = FALSE) +
      theme_map(base_size = base_size)
    ',
    defs_polar = '
      geom_path(data = X$Grid$lat[X$Grid$lat$ID == levels(X$Grid$lat$ID)[which.min(as.numeric(gsub("[[:alpha:]]", "", levels(X$Grid$lat$ID))))],], aes(x = lon.utm, y=lat.utm, group = ID), color = land.border.col, size = land.size) +
      coord_fixed() +
      theme_void(base_size = base_size) +
      theme(legend.position = legend.position)
    ',
    remove_labels = '
      theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
    ',
    interpl_surface = '
      geom_tile(data = x$interpolation, aes(x = Lon, y = Lat, fill = var1.pred, color = var1.pred)) + geom_contour(data = x$interpolation, aes(x = Lon, y = Lat, z = var1.pred), color = "black", size = 0.2)
    ',
    defs_interpl_utm = '
      scale_fill_continuous(type = "viridis", limits = col.scale.limits) +
      scale_colour_continuous(type = "viridis", limits = col.scale.limits) +
      scale_y_continuous(breaks = X$Grid$lat.breaks$lat_utm, labels = X$Grid$lat.breaks$label) +
      scale_x_continuous(breaks = X$Grid$lon.breaks$lon_utm, labels = X$Grid$lon.breaks$label) +
      labs(x = "Latitude (decimal degrees)", y = "Longitude (decimal degrees)", fill = legend.label, colour = legend.label) +
      coord_fixed(xlim = c(X$Grid$limits$lon.utm[1], X$Grid$limits$lon.utm[2]), ylim = c(X$Grid$limits$lat.utm[1], X$Grid$limits$lat.utm[2]), expand = FALSE) +
      theme_map(base_size = base_size) +
      theme(legend.margin=margin(t = 0.2, b = 0, unit="cm"), legend.position = legend.position)
    ',
    stop(paste("map command", command, "not found."))
  )

  trimws(gsub("\n", " ", out))
}

