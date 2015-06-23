#' NNinterp interpolates data on one grid set to a second grid set 
#'
#' @author Zed Zulkafli
#'
#' @description Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
#' do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad
#' minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex
#' ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
#' velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
#' cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id
#' est laborum.
#'
#' @param map_ts is a list containing [[1]]: a time series data frame [[2]]: a spatial grid data frame associated with [[1]]. map_ts[[2]]$ID is the identifier (can be string or numeric) for each data point in the data frame. Map_ts relates to the source data that requires interpolation 
#' @param hires.map is a spatial grid data frame that is the target interpolation grid
#'
#' @return RegridTS is a list containing [[1]]: an interpolated time series data frame [[2]]: a spatial grid data frame associated with [[1]].
#'
#' @examples
#' # ....
#'
#######################################################################################
NNinterp <- function(map_ts, hires.map){

#read in source data coordinates
map     <- map_ts[[2]]
ts      <- map_ts[[1]]

#interpolate source data ID to interpolation grid 
new_map <- idw(ID ~ 1, map, hires.map, nmax = 1)[,"var1.pred"]

#interpolate entire time series
new_ts      <- as.data.frame(ts)[,as.character(new_map$var1.pred)]

#return results
RegridTS <- list(new_ts,new_map)

return(RegridTS)
}
