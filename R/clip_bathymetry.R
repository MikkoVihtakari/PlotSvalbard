#' @title Clip a bathymetry shapefile to fit a basemap
#' @description An internal utility function to clip bathymetry to fit the boundaries of a basemap
#' @param X a basemapData object
#' @export

clip_bathymetry <- function(X) {

if(class(X) != "basemapData") stop("clip_bathymetry requires basemapData object")

if(X$MapClass == "panarctic") {
  ## Define clip boundary shapefile
  bd  <- X$Grid$lat[X$Grid$lat$ID == paste0("lat", X$Boundary[3]),]
  ch <- chull(bd$lat.utm, bd$lon.utm)
  coords <- as.matrix(bd[c(ch, ch[1]), 1:2])
  clipBound <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID = 1)))
  proj4string(clipBound) <- map_projection(X$MapClass)

  ## Clip bathymetry
  bathy <- clip_shapefile(arctic_bathy, clipBound)
  fbathy <- ggplot2::fortify(bathy)

  fbathy$id <- select(strsplit(fbathy$id, " "), 1)
  info <- arctic_bathy@data
  info$id <- rownames(info)

  out <- merge(fbathy, info[c("id", "depth")], by = "id", all.x = TRUE, sort = FALSE)

} else {
  stop(paste("Bathymetry for", X$MapClass, "has not been implemented yet."))
}

}
