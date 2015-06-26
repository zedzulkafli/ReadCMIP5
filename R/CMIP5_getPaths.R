#' CMIP5_getPaths identifies relevant GCM output files on the data directory 
#' based on the user preferences 
#'
#' @author Zed Zulkafli
#'
#' @description This function scans the CMIP5 data directory and returns the 
#' names of GCM output files that contain the variables, scenarios, ensembles, 
#' and models of the user's interest 
#'
#' @param vars is a string vector containing climate variable names. 
#' @param ensemble is a string vector containing model ensemble names. 
#' @param models is a string vector containing climate model names. 
#' @param CCscenarios is a string vector containing climate scenarios names. 
#' @param data_path is a string vector containing the CMIP5 data directory name  
#'  
#' @return CMIP5_paths is a 3-tiered list containing climate model filenames, 
#' arranged by climate variables, climate scenarios, and climate model names 
#' 
#' @examples
#' # CMIP5_paths <- CMIP5.getpaths(	vars		= "tos", ensemble	= "r1i1p1", models  	= "CSIRO-QCCCE/CSIRO-Mk3-6-0", CCscenarios	= "historical", data_path	= "/var/data/CMIP5/")
#'
#######################################################################################

CMIP5.getpaths <-
function(vars, ensemble, models, CCscenarios, data_path){

CMIP5_paths <- list()

for (var in vars){
  CMIP5_paths[[var]] <- list()
  
  for (scenario in CCscenarios) {

		p <- list()

		for(model in models) {
		modelpath <- paste(data_path,  "/", scenario, "/", var,  "/", model ,  "/", sep="")
		setwd(modelpath)

		files <- list.files(); files <- strsplit(files, "_")
		vec   <- numeric(length(files))
		
		if (length(vec) > 0) for (v in 1:length(vec)) vec[v] <- (var %in% files[[v]] & scenario %in% files[[v]] & ensemble %in% files[[v]])
		
		files      <- list.files()[which(vec==1)]
		p[[model]] <- paste(modelpath,files,sep="")
		}

	CMIP5_paths[[var]][[scenario]] <- p
	rm(p)
   }
}

return(CMIP5_paths)
}
