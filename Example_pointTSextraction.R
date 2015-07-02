#CMIP5 time series extraction and visualization of P and T anomalies
#Author: Zed Zulkafli 

rm(list=ls())

#set data and working directories
out_path    <- "/home/zed/CMIP5/ReadCMIP5_example/PointExtraction/" 
data_path   <- "/var/data/CMIP5/"

#call libraries
#install_github("zedzulkafli/ReadCMIP5") #require devtools
library(ReadCMIP5)

########################################################
# 1. Initialisation 								                   #
########################################################

##specify climate variable(s), e.g. pr for precipitation
vars <- c("pr","tas") 

##specify CC scenario
CCscenario    <- c("rcp45" , "rcp85" ) 

##specify GCMs from ensemble  
models <- c(
#		"BCC/bcc-csm1-1-m", 		
#		"BNU/BNU-ESM",
#		"CCCma/CanESM2", 		
#		"CSIRO-BOM/ACCESS1-0", 
#		"CSIRO-BOM/ACCESS1-3", 
#		"CSIRO-QCCCE/CSIRO-Mk3-6-0",
#		"INM/inmcm4",			
# 		"IPSL/IPSL-CM5A-LR", 
#		"IPSL/IPSL-CM5A-MR", 
#		"IPSL/IPSL-CM5B-LR",
		"MIROC/MIROC5", 
#		"MIROC/MIROC-ESM", 
#		"MIROC/MIROC-ESM-CHEM",
		"MOHC/HadGEM2-CC",
# 		"MOHC/HadGEM2-ES",
#		"MRI/MRI-CGCM3",
#		"NOAA-GFDL/GFDL-ESM2G",
		"NOAA-GFDL/GFDL-ESM2M")

## scan data directory for GCM files associated with variable, scenario and model(s) 
CMIP5_paths <- CMIP5.getpaths(vars, ensemble = "r1i1p1", models, c("historical", CCscenario), data_path)


#Specify spatial domain 						   #
bboxes  <- list()
bboxes[["Quito"]] <- c( (-78.47 + 360) , -0.18)
#bboxes[["La Paz"]] <- c( (-68.15 + 360) , -16.5)
#bboxes[["Huaraz"]] <- c( (-77.53 + 360) , -9.53)

########################################################
# 2. Extraction GCM historical and future projections	 #
#    Output are saved to out_path as Rdata files       #
########################################################


for(var in vars){
## get historical 
CMIP5.getIndex(var, "historical" , models, startyear=1971, endyear=2000, CMIP5_paths, bboxes, out_path)

## get future projections
CMIP5.getIndex(var, CCscenario, models, startyear=2071, endyear=2100, CMIP5_paths, bboxes, out_path)
}

########################################################
# 3. View time series                                  #
#                                                      #
########################################################

#list.files(out_path)
#[1] "CMIP5_historical_1971_to_2000_climateindices.RData"
#[2] "CMIP5_rcp45_2071_to_2100_climateindices.RData"     
#[3] "CMIP5_rcp85_2071_to_2100_climateindices.RData" 
#[4] "pr_historical_timeindex.rda"                       
#[5] "pr_rcp45_timeindex.rda"                            
#[6] "pr_rcp85_timeindex.rda"  

TS <- get(load("CMIP5_historical_1971_to_2000_climateindices.RData"))
summary(TS)
plot(TS[["MIROC/MIROC5"]][["Quito"]])
