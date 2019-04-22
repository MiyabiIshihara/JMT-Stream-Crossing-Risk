# Import PRISM temperature data 
# Data obtained from: http://prism.oregonstate.edu/recent/

library(prism)
library(raster)
library(lubridate)
library(tidyverse)

source("scripts/googledrive_read_write_functions.R")

#JMT extent  
  jmt_clipper <- extent(-120, -118, 36, 38)

#PRISM options and path  
options(prism.path = "scripts/precipitation/")

get_prism_precip <- function(date){
  get_prism_dailys(type = "ppt", dates = date, keepZip = F)
  
  precip_rast <- raster(paste0("scripts/temperature/PRISM_ppt_stable_4kmD2_", 
                               gsub("-", "", as.character(date)), 
                               "_bil", "/PRISM_ppt_stable_4kmD2_", 
                               gsub("-", "", as.character(date)), "_bil.bil"))
  
  clipped_precip_rast <- crop(precip_rast, jmt_clipper)
  
  return(clipped_precip_rast)
  
}

#2015 PRISM precipitation ###########

# download daily 2015 precipitation data from PRISM website 
prism_jmt_ppt_2015 <- lapply(seq.Date(ymd("2015-01-01"), ymd("2015-12-31"), "day"),
                              get_prism_precip)

saveRDS(prism_jmt_ppt_2015, file = "scripts/Shiny_App/Data/prism_ppt_jmt_clip_2015.rds")
