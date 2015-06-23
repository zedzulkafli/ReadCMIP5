#' CMIP5_getDomain defines a distributed model grid at the spatial limits and resolution provided by the user
#'
#' @author Zed Zulkafli
#'
#' @This function reads in the coordinate limits and spatial resolution of the analysis/modelling domain and returns a list containing the 1) bounding box and 2) a spatial grid data frame of points interpolated within this bounding box
#'
#' @param lat_range is a numeric vector of size 2 containing the lower and higher latitudinal limits (acceptable values -90 to +90)
#' @param lon_range is a numeric vector of size 2 containing the left and right longitudinal limits (acceptable values -180 to +180)
#' @param res is a numeric vector of size 1 containing the required distance between points (uniform in x and y direction)
#'
#' @return Model_Domain is a list containing 
#' 			1) bboxes: a 2 x 2 matrix of the bounding box  
#' 			2) hires.grid: a spatial grid data frame of points 
#' 			within this bounding box, separated distances equal to @param res 
#'
#' @examples
#' # ModelDomain <- Define.Domain(lat_range = c(-11,1), c(-80, -70), res = 0.125)
#'
#######################################################################################

Define.Domain <- function(lat_range, lon_range, res) {
## define bbox
x <- lon_range + 360
y <- lat_range 
bbox <- expand.grid(x,y); names(bbox) <- c("x","y")
bbox[3:4,] <- bbox[4:3,]
bboxes <- list(bbox)
names(bboxes) <- "domain"

## define model grids
res <- 0.125
newlon <- seq(x[1]+res/2, x[2]-res/2, res)	
newlat <- seq(y[1]+res/2, y[2]-res/2, res)
hires.map    			<- data.frame(expand.grid(newlon, newlat))
coordinates(hires.map) 	<- ~Var1+Var2	
Model_Domain <- list(bboxes, hires.map)	

return(Model_Domain)
}





