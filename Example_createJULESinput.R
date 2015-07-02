#CMIP5 time series extraction and creation of JULES input files
#Author: Zed Zulkafli 

rm(list=ls())

#set data and working directories
out_path    <- "/home/zed/CMIP5/ReadCMIP5_example/CreateJULES/" 
data_path   <- "/var/data/CMIP5/"

#call libraries
#install_github("zedzulkafli/ReadCMIP5") #require devtools
library(ReadCMIP5)
require(ncdf)
require(gstat)

########################################################
# 1. Initialisation   							                   #
########################################################

#specify variable
vars <- c("tas", "pr", "huss", "psl", "rsds", "rlds", "sfcWind", "uas", "vas") 

#specify CC scenarios
CCscenario    <- c("rcp45", "rcp85")  

##specify ensemble
ensemble <- "r1i1p1"

#specify models  
models <- c(	  
#		"BCC/bcc-csm1-1-m", 		### uav and vas (rcp45 and rcp85)
#		"BNU/BNU-ESM",
#		"CCCma/CanESM2", 		### uav and vas (rcp45 only)
# 		"CSIRO-BOM/ACCESS1-0", 
#		"CSIRO-BOM/ACCESS1-3", 
#		"CSIRO-QCCCE/CSIRO-Mk3-6-0",
#		"INM/inmcm4",			### uav and vas
# 		"IPSL/IPSL-CM5A-LR", 
#		"IPSL/IPSL-CM5A-MR", 
#		"IPSL/IPSL-CM5B-LR",
#		"MIROC/MIROC4h",
#		"MIROC/MIROC5", 
#		"MIROC/MIROC-ESM", 
#		"MIROC/MIROC-ESM-CHEM",
#		"MOHC/HadGEM2-CC",
# 		"MOHC/HadGEM2-ES",
#		"MRI/MRI-CGCM3",
#		"NCC/NorESM1-M",
#		"NOAA-GFDL/GFDL-ESM2G",
		"NOAA-GFDL/GFDL-ESM2M")

## scan data directory for GCM files associated with variable, scenario and model(s) 
CMIP5_paths <- CMIP5.getpaths(vars, ensemble = "r1i1p1", models, c("historical", CCscenario), data_path)

## define JULES domain and modelling resolution
ModelDomain <- Define.Domain(lat_range = c(-11,1), c(-80, -70), res = 0.125)

########################################################
# 2. Data extraction and NCDF creation                 #
########################################################

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# all models uses sfcWind unless indicated in the models list
# you will need to adjust the loop accordingly
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

for(var in vars[1:7]){
  ## get historical 
  for (year in seq(2000,2000,1)) CMIP5.regridTS(var, "historical",
                  models, year, CMIP5_paths, ModelDomain, out_path)

  ## get projections
  for (year in seq(2006,2006,1)) CMIP5.regridTS(var, CCscenario, 
                  models, year, CMIP5_paths,  ModelDomain, out_path)
}



