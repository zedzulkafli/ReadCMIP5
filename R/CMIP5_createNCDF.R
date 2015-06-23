#' CMIP5_createNCDF writes annual files for various weather variables according #' to JULES accepted format
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
#' @param ...
#'
#' @return ...
#'
#' @examples
#' # ....
#'
#######################################################################################

CMIP5.createNCDF <- function(var, scenario, model, starttime, endtime, hires.map, subdata, out_path){

var.cmip5      <- c("pr", "tas", "psl", "huss", "rsds", "rlds", "sfcWind", "uas", "vas") 
names.varz     <- c("prcp","tas","pres","shum", "dswrf","dlwrf", "wind", "u", "v")
longnames.varz <- c("Precipitation","Air temperature",  "Surface pressure",  "Specific humidity", "Downward shortwave radiation", "Downward longwave radiation", "Windspeed", "Zonal wind", "Meridional wind")

name.varz     <-  names.varz[which(var.cmip5 == var)]
longname.varz <-  longnames.varz[which(var.cmip5 == var)]

land     <- 1:ncol(subdata)
timestep <- 1:nrow(subdata)

dim3 <- dim.def.ncdf("land","", as.double(land))
dim4 <- dim.def.ncdf("tstep","", as.double(timestep))
varz <-  var.def.ncdf(name.varz,"", list(dim3,dim4), -1, longname=longname.varz)

if(!file.exists(paste(out_path,"JULES", sep="/"))) dir.create(paste(out_path,"JULES", sep="/"))
if(!file.exists(paste(out_path,"JULES", scenario, sep="/"))) dir.create(paste(out_path,"JULES", scenario, sep="/"))
if(!file.exists(paste(out_path,"JULES", scenario, strsplit(model, "/")[[1]][2], sep="/"))) dir.create(paste(out_path,"JULES", scenario, strsplit(model, "/")[[1]][2], sep="/"))
setwd(paste(out_path,"JULES", scenario, strsplit(model, "/")[[1]][2], sep="/"))

startyear   <- as.character(as.POSIXlt(starttime)$year + 1900)
endyear     <- as.character(as.POSIXlt(endtime)$year + 1900)

year_subdata <- as.character(as.POSIXlt(index(subdata))$year + 1900)
 
for (year in startyear:endyear){
subdata_year <- subdata[which(year_subdata==year),]
newfilename <- paste(name.varz,"_", year,".nc",sep="")
nc.ex <- create.ncdf(newfilename, varz)

ncdata <- as.matrix(t(subdata_year))
put.var.ncdf(nc.ex, name.varz, ncdata); rm(ncdata)
close.ncdf(nc.ex)

}
}
