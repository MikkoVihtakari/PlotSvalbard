#' @title Define water type based on temperature and salinity
#' @description Defines water type based on potential temperature and practical salinity data from a CTD. Default values are for Kongsfjorden
#' @param dt Dataframe containing temperature and salinity information
#' @param temp_col Character string giving the column name containing temperature data. Should be potential temperature, if the default \code{WM} is used.
#' @param sal_col Character string giving the column name containing salinity data
#' @param WM Dataframe giving the water mass definitions.
#' @param bind Should the water type information be \code{\link{cbind}} with \code{dt} (\code{TRUE}) or should a character vector containing water type abbreviations returned instead (\code{FALSE}; default)
#' @details Water-types are useful in summarizing salinity, temperature and density characteristics of water masses. By default, the water types are returned after the definition in Cottier et al. (2005) with slight modifications to avoid overlaps of water types. \strong{Density} boundaries have not been implemented as these seem to have relatively little meaning on Svalbard after limiting water types using temperature and salinity.
#'
#'\strong{Custom water type classifications} can be defined using the \code{WM} argument. This argument should be a data frame containing the following column names: type, abb, sal.min, sal.max, temp.min, and temp.max. See the default \code{WM}, \link{kongsfjord_watermasses}, as an example. Rows in the data frame represent separate water types. The "type" column gives the name of the water mass and the "abb" column the abbreviation of the water mass (used in \code{\link{ts_plot}}). The water types are defined using \strong{rectangular} boundaries on salinity (x) - temperature (y) coordinate system. The "sal.min" and "sal.max" columns give the minimum and maximum extent for the x-axis while the corresponding "temp.*" columns define the limits for the y-axis of the rectangles. The rectangles are rendered to polygons inside the function. If water types overlap, the water type that \strong{comes first} in the \code{WM} data frame dominates and defines the water type for a point based on salinity and temperature. Consequently, \strong{one point cannot have more than one water type}. When making the \code{WM} data frame, use \code{Inf} and \code{-Inf} for water types, which do not have positive and negative limits, respectively.  
#'
#'You can use the \link[=ts_plot]{TS diagram} to check that your custom water types are defined correctly. Note that most sources refer to \strong{potential temperatures} and \strong{practical salinities}. Make sure that your data use the same units and standards than the source when defining water types. 
#'
#'MarineDatabase also contains a pre-made \code{WM} data frame for the NPI's Rijpfjorden transect (\link{rijpfjord_watermasses}). The water types in this classification have been modified from Cottier et al. (2005) to better correspond water masses for North of Svalbard. The salinity limit of the Atlantic water layer has been increased to match that in the Atlantic current north of Rijpfjorden (main Atlantic water pathway to the Arctic Ocean) after Crews et al. (2018). Deep Atlantic Water layer represents the water under the current that is clearly Atlantic origin, but has been mixed with Deep Arctic Ocean water masses. 
#'
#' @references Cottier, F., Tverberg, V., Inall, M., Svendsen, H., Nilsen, F., Griffiths, C., 2005. Water mass modification in an Arctic fjord through cross-shelf exchange: The seasonal hydrography of Kongsfjorden, Svalbard. J. Geophys. Res. 110, C12005. doi:10.1029/2004JC002757
#' 
#' Crews L, Sundfjord A, Albretsen J, Hattermann T (2018) Mesoscale Eddy Activity and Transport in the Atlantic Water Inflow Region North of Svalbard. J Geophys Res Ocean 123:201–215. doi: 10.1002/2017JC013198
#' @examples dt <- data.frame(temp = c(1, -1, -0.8), sal = c(34, 34.5, 34.9)) # Make example data
#' define_water_type(dt) # returned as a factor
#' define_water_type(dt, bind  = TRUE) # returned as a data frame
#' @author Mikko Vihtakari
#' @export

# dt <- ctd; temp_col = "theta"; sal_col = "salinity"
# dt = data.frame(temp = c(1, -1, -0.8), sal = c(34, 34.5, 34.9)); temp_col = "temp"; sal_col = "sal";  WM = kongsfjord_watermasses; bind = FALSE
define_water_type <- function(dt, sal_col = "sal", temp_col = "temp", WM = kongsfjord_watermasses, bind = FALSE) {

wm_order <- as.character(unique(WM$abb))

WM$abb <- factor(WM$abb, levels = wm_order)

watertype <- sapply(1:nrow(dt), function(i) {
  # print(i)
  
  tmp <- dt[i, c(sal_col, temp_col)]
  
  
  if(any(c(apply(tmp, 1, is.na)))) {
    
    out <- NA
  
    } else {
  
  index <- dt[[i, sal_col]] > WM$sal.min & dt[[i, sal_col]] <= WM$sal.max & dt[[i, temp_col]] > WM$temp.min & dt[[i, temp_col]] <= WM$temp.max
  out <- WM[index, "abb"]
  
  if(length(out) > 1) out <- out[1] 
  if(length(out) == 0) out <- "Other"
  
  }
  
  as.character(out)
})

watertype <- factor(watertype, levels = c(wm_order, "Other"))

 if(bind) {
    cbind(dt, watertype)
    } else {
    watertype 
  }

}