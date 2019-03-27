source("scripts/snow/snodas_functions.R")

require(lubridate)
require(googledrive)
require(raster)
require(rwrfhydro)

#Get all snodas surfaces from 2015

dates_2015 <- seq(ymd("2015-01-01"), ymd("2015-12-31"), "days")

#Snow Depth
  for(i in 1:length(dates_2015)){
    write_snodas_to_GoogleDrive(dates_2015[i],
                                var_name = "snowDepth", 
                                temp_directory = normalizePath("scripts/snow/temp_snow"),
                                GDfolder_id = "1_IxGme096iUx6JJQY0nhONKSzBWaiI3k")
    print(i)
  }

#Snow water equivalent
  for(i in 1:length(dates_2015)){
    write_snodas_to_GoogleDrive(dates_2015[i],
                                var_name = "SWE", 
                                temp_directory = normalizePath("scripts/snow/temp_snow"),
                                GDfolder_id = "1JPXf6Pq9Ki9zjTSctvO2UxfRn2KTUiq4")
    print(i)
  }
