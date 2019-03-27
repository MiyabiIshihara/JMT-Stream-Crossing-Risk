source("scripts/snow/snodas_functions.R")

require(lubridate)
require(googledrive)
require(raster)
require(rwrfhydro)

#Get all snodas surfaces from 2015

dates_2015 <- seq(ymd("2015-01-01"), ymd("2015-12-31"), "days")

for(i in 1:length(dates_2015)){
  write_snodas_to_GoogleDrive(dates_2015[i],
                              var_name = "snowDepth", 
                              temp_directory = normalizePath("scripts/snow/temp_snow"),
                              GDfolder_id = "1_IxGme096iUx6JJQY0nhONKSzBWaiI3k")
  print(i)
}
