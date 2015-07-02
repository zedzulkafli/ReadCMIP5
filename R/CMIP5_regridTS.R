#' CMIP5_regridTS reads the full time series in climate model output files and
#' returns the time series in the user specified spatial (interpolated to the user 
#' grid) and temporal domain  
#'
#' @author Zed Zulkafli, Wouter Buytaert
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

CMIP5.regridTS <- function(var, CCscenarios, models, year, CMIP5_paths, ModelDomain, out_path){

bboxes    <- ModelDomain[[1]]
hires.map <- ModelDomain[[2]]

starttime <- ISOdate(year,01,01,0,0)
endtime   <- ISOdate(year,12,31,0,0)
 
period 	<- paste(as.character(as.POSIXlt(starttime)$year + 1900),
                  as.character(as.POSIXlt(endtime)$year + 1900), sep="_to_")
      
for (scenario in CCscenarios){
 
  if (!file.exists(paste(out_path, var, "_",scenario,"_timeindex.rda",sep=""))){

  ## get timestamps from each raw data file
  time   <- list()
  start  <- list()
  length <- list()
   
  for(model in models){
    files <- CMIP5_paths[[var]][[scenario]][[model]]
    for (i in 1:length(files)){
    nc <- open.nc(files[i])
    print(paste('reading metadata of ', files[i],sep=''))
    time[[model]][[i]] <- var.get.nc(nc, "time")
    start[[model]][[i]] <- att.get.nc(nc, "time", "units")
    length[[model]][[i]] <- att.get.nc(nc, "time", "calendar")
    close.nc(nc)
    }
   }

  #get start year from each raw data file
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
    } else if(length[[model]][[i]] == "365_day" | length[[model]][[i]] == "noleap") {
      year  <- as.numeric(start[[model]][[i]]) + floor(time[[model]][[i]] / 365) 
      month  <- rep(ts_365_mon, length(unique(year)))
      day    <- rep(ts_365_day, length(unique(year)))   
      time2[[model]][[i]] <- ISOdate(year,month,day,0,0,0) 
    } else if(length[[model]][[i]] == "360_day") {
      year <- as.numeric(start[[model]][[i]]) + floor(time[[model]][[i]] / 360) +1
      month <- floor((time[[model]][[i]] - 360 * floor(time[[model]][[i]] / 360)) / 30)+ 1
      day   <- rep(1:30, 12*length(unique(year)))
      time2[[model]][[i]] <- as.POSIXct.PCICt(as.PCICt(paste(year,month,day,sep="-"), cal="360_day"), cal="360_day")  	  
    }  else {
      seq <- seq(ISOdate(as.numeric(start[[model]][[i]]),1,2,0,0), ISOdate(2300,12,31,0,0), by="1 day")
      time2[[model]][[i]] <- seq[floor(time[[model]][[i]])]; rm(seq);
     }
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

  ## read data, crop, and regrid (NN-interpolation)
  for(model in models) {

    RegridTS <- list() 
    ## get and bind data
    
    count <- 1
    for (i in 1:length(starti[[model]])){
    if(starti[[model]][[i]]< endi[[model]][[i]]){ #check that file contains data within time domain 
                         
            nc <- open.nc(CMIP5_paths[[var]][[scenario]][[model]][[i]])
            print(paste('extracting from ',CMIP5_paths[[var]][[scenario]][[model]][[i]],sep=''))
        
            if (count==1) {

			lon   <- var.get.nc(nc, "lon"); lon [which(lon <0) ] <- lon [which(lon <0) ]  + 360
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
    
		## get time series 
		for (k in 1:length(bboxes)){
     
        # clip data in bbox
        pip     <- pnt.in.poly(coord, bboxes[[k]])
        subdata <- data[which(pip$pip>0),]; subcoord <- coord[which(pip$pip>0),]; 
		
		# regrid data
        subcoord$ID <- 1:nrow(subcoord); coordinates(subcoord) <- ~Var1+Var2
        subdata     <- data.frame(t(subdata)); names(subdata) <- subcoord$ID
        subdata     <- NNinterp(list(subdata,subcoord),hires.map)[[1]]
		
		# convert to zoo 
		subdata <- as.zoo(subdata); index(subdata) <- ts
    
		# uncomment for aggregating to monthly
    	#subdata <- apply.monthly(subdata, mean,na.rm=T)

        # store data and write ncdf       
        RegridTS[[names(bboxes)[k]]] <- subdata; 
        CMIP5.createNCDF(var, scenario, model, starttime, endtime, hires.map, subdata, out_path)
        rm( subcoord, subdata, pip)	
     } #end for bboxes loop
  
  setwd(out_path)
  save(CMIP5Ts=RegridTS, 
	file =paste(var, "CMIP5", strsplit(model, "/")[[1]][2], scenario, period,  "regridTS.Rdata", sep="."))
 
   rm(data, coord );
   }

} 
}
