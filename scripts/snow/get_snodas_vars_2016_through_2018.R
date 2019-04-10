source("scripts/snow/snodas_functions.R")

require(lubridate)
require(googledrive)
require(raster)
require(rwrfhydro)

#Get all snodas surfaces from 2015

dates_2016_2018 <- seq(ymd("2016-01-01"), ymd("2018-12-31"), "days")

#Snow Depth
  for(i in 1:length(dates_2016_2018)){
    write_snodas_to_GoogleDrive(dates_2016_2018[i],
                                var_name = "snowDepth", 
                                temp_directory = normalizePath("scripts/snow/temp_snow"),
                                GDfolder_id = "1_IxGme096iUx6JJQY0nhONKSzBWaiI3k")
    print(i)
    
  #Delete intermediate files as it goes along  
    if(i %% 50 == 0 | i == length(dates_2016_2018)){
      do.call(file.remove, list(list.files("scripts/snow/temp_snow/", full.names = TRUE)))
    }
  }

#Snow water equivalent
  for(i in 1:length(dates_2016_2018)){
    write_snodas_to_GoogleDrive(dates_2016_2018[i],
                                var_name = "SWE", 
                                temp_directory = normalizePath("scripts/snow/temp_snow"),
                                GDfolder_id = "1JPXf6Pq9Ki9zjTSctvO2UxfRn2KTUiq4")
    print(i)
    
  #Delete intermediate files as it goes along  
    if(i %% 50 == 0 | i == length(dates_2016_2018)){
      do.call(file.remove, list(list.files("scripts/snow/temp_snow/", full.names = TRUE)))
    }

  }
