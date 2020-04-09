#' @title Automatic limits for basemap
#' @description An internal function to make \code{\link{basemap}} code more readable
#' @param type see the \code{type} argument for \code{\link{basemap}}
#' @param limits Longitude and latitude columns in \code{data}. The elements 2 and 3 in the \code{limits} argument for \code{\link{basemap}}
#' @param data Dataframe containing data for which the limits should be calculated. The element 1 in the limits \code{limits} argument for \code{\link{basemap}}
#' @param limits.lon,limits.lat Numeric. The level of rounding for longitude and latitude, respectively. If \code{NULL}, sensible default values are used.
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function.
#' @keywords internal
#' @export
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}}


auto_limits <- function(type, limits, data, limits.lon = NULL, limits.lat = NULL) {

  rdiff.lon <- diff(range(data[limits[1]], na.rm = TRUE))
  rdiff.lat <- diff(range(data[limits[2]], na.rm = TRUE))
  #rdiff.lon <- diff(range(get(limits[1])[limits[2]], na.rm = TRUE))
  #rdiff.lat <- diff(range(get(limits[1])[limits[3]], na.rm = TRUE))

    ## Pan-Arctic maps (makes a square map)
    if(type %in% c("panarctic")) {

      if(is.null(limits.lon)) {
        limits.lon <- ifelse(rdiff.lon > 1e6, 1e5, ifelse(rdiff.lon > 1e5, 1e4, 1e3))
      }

      if(is.null(limits.lat)) {
        limits.lat <- ifelse(nchar(round(rdiff.lon, 0)) == nchar(round(rdiff.lat, 0)), limits.lon, ifelse(rdiff.lon > 1e6, 1e5, ifelse(rdiff.lon > 1e5, 1e4, 1e3)))
      }

      limits <- c(round_any(min(data[limits[1]], na.rm = TRUE), limits.lon, floor),
                  round_any(max(data[limits[1]], na.rm = TRUE), limits.lon, ceiling),
                  round_any(min(data[limits[2]], na.rm = TRUE), limits.lat, floor),
                  round_any(max(data[limits[2]], na.rm = TRUE), limits.lat, ceiling)
      )


      #limits <- c(round_any(min(get(limits[1])[limits[2]], na.rm = TRUE), limits.lon, floor),
      #            round_any(max(get(limits[1])[limits[2]], na.rm = TRUE), limits.lon, ceiling),
      #            round_any(min(get(limits[1])[limits[3]], na.rm = TRUE), limits.lat, floor),
      #            round_any(max(get(limits[1])[limits[3]], na.rm = TRUE), limits.lat, ceiling)
      #            )

    ## UTM maps
    } else {

      if(is.null(limits.lon)) {
        limits.lon <- ifelse(rdiff.lon > 30, 2, ifelse(rdiff.lon > 10, 1, 0.1))
      }

      if(is.null(limits.lat)) {
        limits.lat <- ifelse(rdiff.lon > 10, 1, ifelse(rdiff.lon > 1, 0.1, 0.05))
      }

      limits <- c(round_any(min(data[limits[1]], na.rm = TRUE), limits.lon, floor),
                  round_any(max(data[limits[1]], na.rm = TRUE), limits.lon, ceiling),
                  round_any(min(data[limits[2]], na.rm = TRUE), limits.lat, floor),
                  round_any(max(data[limits[2]], na.rm = TRUE), limits.lat, ceiling)
      )

      # limits <- c(round_any(min(get(limits[1])[limits[2]], na.rm = TRUE), limits.lon, floor),
      #             round_any(max(get(limits[1])[limits[2]], na.rm = TRUE), limits.lon, ceiling),
      #             round_any(min(get(limits[1])[limits[3]], na.rm = TRUE), limits.lat, floor),
      #             round_any(max(get(limits[1])[limits[3]], na.rm = TRUE), limits.lat, ceiling)
      #             )
    }

limits

}
