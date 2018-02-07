#' @title Add a land layer on basemaps
#' @description Adds a layer of land on \code{\link{basemap}} plots
#' @param plot Not implemented yet. \code{\link{basemap}} where the land should be added to. Extracts the definitions automatically.
#' @param land.col Character code specifying the color of land.
#' @param gla.col Character code specifying the color of glaciers.
#' @param keep.glaciers Logical indicating whether glaciers should be kept for the Svalbard maps. Setting this to \code{FALSE} speeds up map plotting by a few seconds.
#' @param border.col.land Character code specifying the color of the border line for land shapes.
#' @param border.col.glacier Character code specifying the color of the border line for glacier shapes.
#' @param size.land Numeric value specifying the width of the border line for land shapes. See details for explanation about line widths.
#' @param size.glacier Numeric value specifying the width of the border line for glacier shapes.
#' @param label.print Logical indicating whether labels should be printed for polar stereographic maps.
#' @param label.font Numeric value specifying the font size for labels in polar stereographic maps. Note that this value defines the actual font size in points, not the \code{ggplot2} font size.
#' @param label.offset Offset between the round polar stereographic maps and longitude labels. Optimized for a pdf output. Use 1.1 for larger size figures.
#' @param grid.col Character code specifying the color of grid lines. Use \code{NA} to remove the grid lines.
#' @param size.grid Numeric value specifying the width of the grid lines.
#' @author Mikko Vihtakari
#' @import ggplot2
#' @export


add_land <- function(plot = NULL, keep.glaciers = FALSE, land.col = "#eeeac4", size.land = 0.1, border.col.land = "black", gla.col = "grey95", size.glacier = 0.1, border.col.glacier = "black", grid.col = "grey70", size.grid = 0.1, label.print = FALSE, label.offset = 1.05, label.font = 8) {

  if(label.print) stop("label.print = TRUE in add_land() has not been implemente yet")

  if(is.null(plot)) {
    if(keep.glaciers) {
    stop("keep.glaciers = TRUE in add_land() has not been implemented yet")
    } else {
      geom_polygon(aes(x = long, y = lat, group = group), fill = land.col, color = border.col.land, size = size.land)
    }


  } else {
    stop("add_land as non layer function has not been implemented yet")
  }
}
