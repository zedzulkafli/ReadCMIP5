#' CMIP5_getPaths identifies relevant GCM output files on the data directory 
#' based on the user preferences 
#'
#' @author Zed Zulkafli
#'
#' @description This function scans the CMIP5 data directory and returns the 
#' names of GCM output files that contain the variables, scenarios, ensembles, 
#' and models of the user's interest 
#'
#' @param ClimateIndices is a string vector containing climate variable names
#' @param models is a string vector containing climate model names. 
#' @param CCscenarios is a string vector containing climate scenarios names. 
#' @param out_path is a string vector containing the user's working directory name 
#'  
#' @return NULL. Plots are saved in the @param out_path 
#'
#' @examples
#' # CMIP5(climateIndices =climateIndices, models="CSIRO-QCCCE/CSIRO-Mk3-6-0", CCscenarios="historical", out_path = "/home/me")
#'
#########################################################################

CMIP5.plotIndex <- function (climateIndices, models, CCscenarios, out_path){
  
setwd(paste(out_path, sep=""))  

DF <- data.frame(count = numeric(), model = factor(), scenario = numeric(),
                 month = factor(), type = factor())

for (climateIndex in climateIndices){

	for (model in models){

		setwd(out_path)
		files <- list.files(); files <- strsplit(files, "_")
		vec   <- numeric(length(files))
		
		if (length(vec) > 0) for (v in 1:length(vec)) vec[v] <- ("historical" %in%files[[v]] & "climateindices.RData" %in% files[[v]])
		files      <- list.files()[which(vec==1)]

        load(files)
		ts   <- ClimateIndexTS[[model]][[climateIndex]]
		if(anyDuplicated(ts)>0) ts <- aggregate(ts, time(ts), mean) 

		t_ind  <- index(ts) 
		ts     <- as.numeric(ts)
		temp   <- ts
		scenario_ind <- rep("historical", length(ts))

		for (scenario in CCscenarios[-1]){
		files <- list.files(); files <- strsplit(files, "_")
		vec   <- numeric(length(files))
				
		if (length(vec) > 0) for (v in 1:length(vec)) vec[v] <- (scenario %in%files[[v]] & "climateindices.RData" %in% files[[v]])
		files      <- list.files()[which(vec==1)]
		
		load(files)
		ts   <- ClimateIndexTS[[model]][[climateIndex]]
		if(anyDuplicated(ts)>0) ts <- aggregate(ts, time(ts), mean) 
		
		t_ind  <- c(t_ind, index(ts))
		ts     <- as.numeric(ts)
		temp   <- c(temp, ts) 
		scenario_ind <- c(scenario_ind, rep(scenario, length(ts)))
		}

		dummy <- data.frame(count 	 = as.numeric(temp),
                    scenario = scenario_ind,
                    model    = model,
                    month    = as.POSIXct(t_ind),
                    type     = as.factor(climateIndex))
		DF <- rbind(DF, dummy)
	}  
}
DF$null <- ""

dsumm <- ddply(DF, .(scenario, month, type), summarise, m = median(count,na.rm=TRUE), max = max(count,na.rm=TRUE), min = min(count, na.rm=TRUE))

g <- ggplot(DF, aes(month, count)) + theme_bw() +
  facet_grid(type ~ null, scales = "free_y")
g <- g + geom_line(data = dsumm, aes( y = m, color = scenario) )
g <- g + geom_ribbon(data = dsumm, aes( color=scenario, fill=scenario, y=NULL, 
     ymin = min, ymax = max ), alpha=0.2) 
g <- g + ylab("Indices (K)") + xlab("Date") 
ggsave(g, filename = "plot.pdf", width=15, height=11) 

g <- ggplot(DF, aes(month, count)) + theme_bw() +
     facet_grid(type ~ null, scales = "free_y")
g <- g + geom_line(data = dsumm, aes( y = m, color = scenario) )
g <- g + ylab("Indices (K)") + xlab("Date") + 
     scale_color_manual(values=c("dark gray", "green", "red"))
ggsave(g, filename = "plot2.pdf", width=15, height=11) 

}
