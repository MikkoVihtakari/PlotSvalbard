#' @title Clip a bathymetry shapefile to fit a basemap
#' @description An internal utility function to clip bathymetry to fit the boundaries of a basemap
#' @param X A basemapData object
#' @param detailed Logical indicating whether detailed bathymetry shapefiles should be used for Svalbard maps.
#' @author Mikko Vihtakari
#' @keywords internal
#' @importFrom broom tidy
#' @importFrom grDevices chull
#' @import sp
#' @export

clip_bathymetry <- function(X, detailed = FALSE) {

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
  sp::proj4string(clipBound) <- map_projection(X$MapClass)
  clip_bathy <- arctic_bathy

} else {

  clipBound <- X$Grid$limits_shp_utm

  if(X$MapClass == "barents" | !detailed) {
    clip_bathy <- barents_bathy
  } else {
    clip_bathy <- svalbard_bathy
  }
}


  ## Clip bathymetry
  bathy <- quiet(clip_shapefile(clip_bathy, clipBound))
  fbathy <- suppressWarnings(broom::tidy(bathy))

  fbathy$id <- select(strsplit(fbathy$id, " "), 1)
  info <- clip_bathy@data
  info$id <- rownames(info)

  if(any(grepl("Depth", names(info)))) {
    names(info)[grepl("Depth", names(info))] <- "depth"
  }

  if(any(grepl("DYBDE_MAX", names(info)))) {
    names(info)[grepl("DYBDE_MAX", names(info))] <- "depth"
  }

  out <- merge(fbathy, info[c("id", "depth")], by = "id", all.x = TRUE, sort = FALSE)

  out$depth <- ordered(out$depth)

  if(X$MapClass == "panarctic") {

  levels(out$depth) <- c(paste(levels(out$depth)[-nlevels(out$depth)], levels(out$depth)[-1], sep = "-"), paste0(">", levels(out$depth)[nlevels(out$depth)]))

  } else {
    levels(out$depth) <- paste(c("0", levels(out$depth)[-nlevels(out$depth)]), levels(out$depth), sep = "-")
  }

  out <- out[with(out, order(depth, id, order)),]

  out$group <- ordered(out$group, unique(out$group)) ## Order $group to plot holes (fixes a problem caused by a bad shapefile)

  out
}
