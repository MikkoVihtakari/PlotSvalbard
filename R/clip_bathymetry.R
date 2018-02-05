#' @title Clip a bathymetry shapefile to fit a basemap
#' @description An internal utility function to clip bathymetry to fit the boundaries of a basemap
#' @param X a basemapData object
#' @author Mikko Vihtakari
#' @import sp
#' @export

clip_bathymetry <- function(X) {

if(class(X) != "basemapData") stop("clip_bathymetry requires basemapData object")

if(X$MapClass == "panarctic") {

  ## Define clip boundary shapefile
  if(!X$Grid$limits) {
    bd  <- X$Grid$lat[X$Grid$lat$ID == paste0("lat", X$Boundary[3]),]
    ch <- chull(bd$lat.utm, bd$lon.utm)
    coords <- as.matrix(bd[c(ch, ch[1]), 1:2])
    clipBound <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords)), ID = 1)))

    } else {
    limits <- c(X$Grid$boundaries$lon.utm, X$Grid$boundaries$lat.utm)
    bd <- sp::Polygon(matrix(c(limits[1], limits[3], limits[1], limits[4], limits[2], limits[4], limits[2], limits[3], limits[1], limits[3]), ncol = 2, byrow = TRUE))
    clipBound <- sp::SpatialPolygons(list(sp::Polygons(list(bd), ID = "clip_boundary")))
  }

  clip_bathy <- arctic_bathy

} else if(X$MapClass == "barents") {

    limits <- c(X$Grid$boundaries$lon.utm, X$Grid$boundaries$lat.utm)
    bd <- sp::Polygon(matrix(c(limits[1], limits[3], limits[1], limits[4], limits[2], limits[4], limits[2], limits[3], limits[1], limits[3]), ncol = 2, byrow = TRUE))
    clipBound <- sp::SpatialPolygons(list(sp::Polygons(list(bd), ID = "clip_boundary")))

    clip_bathy <- sp::spTransform(arctic_bathy ,CRS(map_projection("barents")))
    clip_bathy <- rgeos::gBuffer(clip_bathy, byid=TRUE, width=0)
} else {
  stop(paste("Bathymetry for", X$MapClass, "has not been implemented yet."))
}

  sp::proj4string(clipBound) <- map_projection(X$MapClass)

  ## Clip bathymetry
  bathy <- clip_shapefile(clip_bathy, clipBound)
  fbathy <- ggplot2::fortify(bathy)

  fbathy$id <- select(strsplit(fbathy$id, " "), 1)
  info <- arctic_bathy@data
  info$id <- rownames(info)

  out <- merge(fbathy, info[c("id", "depth")], by = "id", all.x = TRUE, sort = FALSE)

  out$depth <- ordered(out$depth)
  levels(out$depth) <- c(paste(levels(out$depth)[-nlevels(out$depth)], levels(out$depth)[-1], sep = "-"), paste0(">", levels(out$depth)[nlevels(out$depth)]))

  out
}
