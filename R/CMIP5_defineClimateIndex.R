#' CMIP5_defineClimateIndex reads in user input of Climate Indices and returns  
#' their corresponding geographical bounding boxes  
#'
#' @author Zed Zulkafli
#'
#' @description This function reads in user input of Climate Indices names, 
#' and returns their corresponding geographical bounding boxes for subsequent
#' data extraction and analysis (e.g spatial averaging). The bounding box 
#' definitions are currently limited to ENSO and IOD indices.  
#' references: http://www.esrk.noaa.gov/psd/data/climateindices/list/
#'            http://www.bom.gov.au/climate/IO/about_IOD.shtml
#'
#' @param climateIndices is a string vector of Climate Indices names
#'
#' @return bboxes is a list of bounding box definions in matrix format
#'
#' @examples
#' # bboxes  <- Define.ClimateIndex(  c("NINO_3", "NINO_4", "NINO_3.4", 
#'            							"NINO_1+2", "IOD_west", "IOD_east") 
#'
#######################################################################################
Define.ClimateIndex <- function(climateIndices){

bboxes <- list()

#NINO 3
x <- c(-150, -90) + 360
y <- c(-5, 5)
bbox <- expand.grid(x,y); names(bbox) <- c("x","y")
bbox[3:4,] <- bbox[4:3,]
bboxes[["NINO_3"]] <- bbox

#NINO 1+2
x <- c(-90, -80) + 360
y <- c(-10, 0)
bbox <- expand.grid(x,y); names(bbox) <- c("x","y")
bbox[3:4,] <- bbox[4:3,]
bboxes[["NINO_1+2"]] <- bbox

#NINO 4
x <- c( (160-360), -150) + 360
y <- c(-5, 5)
bbox <- expand.grid(x,y); names(bbox) <- c("x","y")
bbox[3:4,] <- bbox[4:3,]
bboxes[["NINO_4"]] <- bbox

#NINO 3.4
x <- c(-170, -120) + 360
y <- c(-5, 5)
bbox <- expand.grid(x,y); names(bbox) <- c("x","y")
bbox[3:4,] <- bbox[4:3,]
bboxes[["NINO_3.4"]] <- bbox

#IOD east
x <- c(90-360, 110-360) + 360
y <- c(-10, 0)
bbox <- expand.grid(x,y); names(bbox) <- c("x","y")
bbox[3:4,] <- bbox[4:3,]
bboxes[["IOD_east"]] <- bbox

#IOD west
x <- c(50-360, 70-360) + 360
y <- c(-10, 10)
bbox <- expand.grid(x,y); names(bbox) <- c("x","y")
bbox[3:4,] <- bbox[4:3,]
bboxes[["IOD_west"]] <- bbox

return(bboxes[as.character(climateIndices)])
}