#' CMIP5_getIndex reads the full time series in climate model output files and
#' returns the time series in the user specified spatial (averaged) and temporal domain  
#'
#' @author Zed Zulkafli
#'
#' @description This function performs multiple tasks in series. 
#' 1. The function reads through each of the GCM output files in a list and
#'    populates the time series based on the calendar type (gregorian, 365,  
#'    360 days, etc., varies from model to model). 
#' 2. The function identifies the starting points and endpoints in time  
#'    series within each file that fall within the time domain from the user  
#'    input and extracts the data.  
#' 3. The function clips the global time series using the bounding box(es) 
#'    from the user input, then calculates the spatial mean.
#' 4. The function saves the time series of spatial mean in rda files.
#' 
#' @param vars is a string vector containing climate variable names. 
#' @param models is a string vector containing climate model names. 
#' @param CCscenarios is a string vector containing climate scenarios names. 
#' @param out_path is a string vector containing the user's working directory name 
#' @param starttime is a numeric vector of size 1 containing the start of the time period of interest
#' @param endtime is a numeric vector of size 1 containing the end of the time period of interest
#' @param bboxes is list of matrices containing the bounding box(es) associated 
#' to all climate indices of interest. The list can be generated using Define.ClimateIndex()  
#'
#' @return NULL. Results are saved in files in the user's working directory.
#'
#' @examples
#' # CMIP5.getIndex(var 		= "tos", CCscenarios	= "historical", models 		= "CSIRO-QCCCE/CSIRO-Mk3-6-0", starttime	= ISOdate(1976,01,01,0,0), endtime		= ISOdate(2005,12,31,0,0), CMIP5_paths = "/var/data/CMIP5", bboxes = cbind(c(210,270,270,210),c(-5,-5,5,5)), out_path = "/home/me/")
#'								  
#'
#######################################################################################

CMIP5.getIndex <- function(var, CCscenarios, models, startyear, endyear, CMIP5_paths, bboxes, out_path){

starttime <- ISOdate(startyear,01,01,0,0)
endtime   <- ISOdate(endyear,12,31,0,0)
 
period 	<- paste(as.character(as.POSIXlt(starttime)$year + 1900),
                  as.character(as.POSIXlt(endtime)$year + 1900), sep="_to_")
      
for (scenario in CCscenarios){
 
  if (!file.exists(paste(out_path, var, "_",scenario,"_timeindex.rda",sep=""))){

  ## get timestamps
  time   <- list()
  start  <- list()
  length <- list()
   
  for(model in models){
    files <- CMIP5_paths[[var]][[scenario]][[model]]
    for (i in 1:length(files)){
    nc <- open.nc(files[i])
    print(files[i])
    time[[model]][[i]] <- var.get.nc(nc, "time")
    start[[model]][[i]] <- att.get.nc(nc, "time", "units")
    length[[model]][[i]] <- att.get.nc(nc, "time", "calendar")
    close.nc(nc)
    }
   }

  #get start year for each file
  for(model in models){
    for (i in 1:length(start[[model]])){

    if (model != "MPI-M/MPI-ESM-P")  { 
    start[[model]][[i]] <- strsplit(start[[model]][[i]], " ")[[1]][3] } else {
    start[[model]][[i]] <- strsplit(start[[model]][[i]], " ")[[1]][4] }
    start[[model]][[i]] <- as.numeric(as.character(strsplit(start[[model]][[i]], "-")[[1]][1]))
    }
   }
  
  #standardize time stamps in date format
  time2 <- list()
 
  ts_365 <- seq(ISOdate(1961,1,1,0,0), ISOdate(1961,12,31,0,0), by="1 day")
  ts_365_mon <- as.numeric(format(ts_365,"%m"))
  ts_365_day <- as.numeric(format(ts_365,"%d"))

  for(model in models){

   for (i in 1:length(start[[model]])){
   if(!is.null(length[[model]][[i]])){

    if(length[[model]][[i]] == "gregorian" | length[[model]][[i]] == "proleptic_gregorian"| length[[model]][[i]] == "standard") {
      seq <- seq(ISOdate(as.numeric(start[[model]][[i]]),1,2,0,0), ISOdate(2300,12,31,0,0), by="1 day")
      time2[[model]][[i]] <- seq[floor(time[[model]][[i]])]; rm(seq)
    }
    if(length[[model]][[i]] == "365_day" | length[[model]][[i]] == "noleap") {
      year  <- as.numeric(start[[model]][[i]]) + floor(time[[model]][[i]] / 365) 
      month  <- rep(ts_365_mon, length(unique(year)))
      day    <- rep(ts_365_day, length(unique(year)))   
      time2[[model]][[i]] <- ISOdate(year,month,day,0,0,0) 
    }
    if(length[[model]][[i]] == "360_day") {
      year <- as.numeric(start[[model]][[i]]) + floor(time[[model]][[i]] / 360) +1
      month <- floor((time[[model]][[i]] - 360 * floor(time[[model]][[i]] / 360)) / 30)+ 1
      day   <- rep(1:30, 12*length(unique(year)))
      time2[[model]][[i]] <- as.POSIXct.PCICt(as.PCICt(paste(year,month,day,sep="-"), cal="360_day"), cal="360_day")  	  
    } 
    } else {
      seq <- seq(ISOdate(as.numeric(start[[model]][[i]]),1,2,0,0), ISOdate(2300,12,31,0,0), by="1 day")
      time2[[model]][[i]] <- seq[floor(time[[model]][[i]])]; rm(seq);
    }
   }
  }

  #strange cases where there first day of the start year is omitted (tos only)
  for (model in c("BNU/BNU-ESM","NCC/NorESM1-M",  "NCC/NorESM1-ME")) {

   for (i in 1){
   if(!is.null(length[[model]][[i]])){
      year  <- as.numeric(start[[model]][[i]]) + floor(time[[model]][[i]] / 365)
      month  <- rep(ts_365_mon, length(unique(year)))[-1]
      day    <- rep(ts_365_day, length(unique(year)))[-1]   
      time2[[model]][[i]] <- ISOdate(year,month,day,0,0,0)
   }
   }
  }
 
  rm(files,time,start,length, year, month, day, ts_365, ts_365_mon, ts_365_day);

  save(time2,file=paste(out_path, var, "_",scenario,"_timeindex.rda",sep=""))

  } else {
  load (file=paste(out_path, var, "_",scenario,"_timeindex.rda",sep=""))
  }
  
  ## locate start and end indices to read in each file:
  starti <- list()
  endi   <- list()
  
  for(model in models) {
    for (i in 1:length(time2[[model]])){
    		starti[[model]][[i]] <-  which.min(abs(time2[[model]][[i]] - starttime))
    		endi[[model]][[i]]   <-  which.min(abs(time2[[model]][[i]] - endtime)) 
    }
   }

  ClimateIndexTS <- list()
  
  ## read data, bind, and convert to Climate Indices
  for(model in models) {

    ClimateIndexTS[[model]] <- list() 
    ## get and bind data
    
    count <- 1
    for (i in 1:length(starti[[model]])){
    if(starti[[model]][[i]]< endi[[model]][[i]]){ #check that file contains data within time domain 
                         
            nc <- open.nc(CMIP5_paths[[var]][[scenario]][[model]][[i]])
	        print(CMIP5_paths[[var]][[scenario]][[model]][[i]])
        
            if (count==1) {

	    lon   <- var.get.nc(nc, "lon");  lon [which(lon <0) ] <- lon [which(lon <0) ]  + 360
	    lat   <- var.get.nc(nc, "lat")
        counti <- endi[[model]][[i]] - starti[[model]][[i]] +1 
	    
		data <- var.get.nc(nc, var, start=c(NA,NA,starti[[model]][[i]]), count=c(NA,NA,counti))
	    
		close.nc(nc)
   
            if(is.finite(dim(lon)[2])) {
             	coord <- cbind(as.vector(lon), as.vector(lat)) }  else { 
				coord <- expand.grid(lon,lat)}          
				rm(lon,lat) 

            data  <- apply(data,3, as.vector)

            ts <- time2[[model]][[i]][starti[[model]][[i]]:endi[[model]][[i]]]  

            count <- count + 1 
	    
	    } else {
	    
            counti <- endi[[model]][[i]] - starti[[model]][[i]] +1 

			data.subset <- var.get.nc(nc, var, start=c(NA,NA,starti[[model]][[i]]), count=c(NA,NA,counti))

			close.nc(nc)
   
            data.subset <- apply(data.subset,3, as.vector)
           
            data <- cbind(data,data.subset)
            ts <- c(ts, time2[[model]][[i]][starti[[model]][[i]]:endi[[model]][[i]]])

            count <- count + 1 
			}
        }
      }
    
    for (k in 1:length(bboxes)){

      # clip data in bboxes 
      # point input
      if (!is.matrix(bboxes[[k]])){
        pip      <- which.min(spDistsN1(as.matrix(coord),bboxes[[k]] ,longlat=TRUE))
        subdata <- data[pip,]} else {		
          # bbox input
          pip     <- pnt.in.poly(coord, bboxes[[k]])
          subdata <- data[which(pip$pip>0),] 
          # calculate mean i.e. index
          subdata <- apply(subdata,2, mean, na.rm=T)}

		# calculate mean i.e. index
        subdata <- apply(subdata,2, mean, na.rm=T)

		# convert to zoo 
		subdata <- as.zoo(subdata); index(subdata) <- ts
    
        # uncomment for aggregating to monthly
		#subdata <- apply.monthly(subdata, mean,na.rm=T)
       
        ClimateIndexTS[[model]][[names(bboxes)[k]]] <- subdata
     } 
	 
	rm(data); rm(subdata); rm(pip);
	gc()
  	
    
   }
    setwd(out_path)
	save(ClimateIndexTS=ClimateIndexTS, 
	file =paste("CMIP5", scenario, period, "climateindices.RData", sep="_"))
   
 } 
} 




