
require(leaflet)
require(sp)
require(sf)
require(raster)
require(htmltools)
require(lubridate)
require(stringr)
require(tidyverse)

source("scripts/googledrive_read_write_functions.R")

# Get vector data from google drive to plot ###############
  # Actual JMT trail vector and entry trails
    jmt_trail <- load_rgdal_from_googledrive("1RJPvOwVY1mcfjKh1eVybZHfWuIX1jE23", "Trail Edges")
      jmt_trail <- spTransform(jmt_trail, "+proj=longlat +ellps=GRS80 +no_defs") %>% st_as_sf() %>% 
        st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>% 
        mutate(edge_id = row_number())
      
  #Subset trails to main JMT or access trails    
    jmt_access <- jmt_trail[which(jmt_trail$Type == "Access"),]
      saveRDS(jmt_access, "scripts/Shiny_App/Data/jmt_access_trails.rds") 
      
    jmt_main <- jmt_trail[which(jmt_trail$Type == "Main"),]
      saveRDS(jmt_main, "scripts/Shiny_App/Data/jmt_main_trail.rds")  
    
  # JMT Stream crossing points are extensively processed in crossings_add_field.R, this just for interactive use with other layers created, edited here
    jmt_crossings <- load_rgdal_from_googledrive("1klB4m5GQVIv7sVaZnZzbbomnkqpfDih2")
      jmt_crossings_df <- jmt_crossings@data %>% 
        mutate(crossing_id = row_number())

  # Watersheds upstream of stream crossings  
    jmt_watersheds <- load_rgdal_from_googledrive("1yB7ww8YgWCAOHjeuCa4Xu6vIZPthO3aD")
      jmt_watersheds <- spTransform(jmt_watersheds, "+proj=longlat +ellps=GRS80 +no_defs") %>% 
        st_as_sf() %>% 
        st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
      saveRDS(jmt_watersheds, "scripts/Shiny_App/Data/jmt_watersheds.rds")  
  
# Trail routes for drop down list
  trail_routes <- load_csv_from_googledrive("1L4JVtsC1Jz9_T7wFtxNnDyYq5R65Lou_")
    trailheads <- unique(trail_routes$`entry trailhead`)
  
      saveRDS(trail_routes, "scripts/Shiny_App/Data/trail_routes.rds")
      saveRDS(trailheads, "scripts/Shiny_App/Data/jmt_trailheads.rds")  

# Time series of SWE in watersheds and associated risk for time series viz
  #Get time series of snodas data in JMT watersheds from GoogleDrive
  jmt_swe_2015 <- load_csv_from_googledrive("1Py8svcq-YMBUlhKYJR_HnfVGf41dkFyp")
  jmt_swe_2016 <- load_csv_from_googledrive("15vpYxV75PpQWGEmhEoTkSvkx07vp5H5P")
  jmt_swe_2017 <- load_csv_from_googledrive("1bjlEhCZ9ghEoLtUXmjiOJQKkxKDOk4zS")
  jmt_swe_2018 <- load_csv_from_googledrive("1OwX0bljESiE6UCDO3m6J_NlRGlkPpQ9a")

  jmt_swe15_18 <- rbind(jmt_swe_2015, jmt_swe_2016, jmt_swe_2017, jmt_swe_2018) %>% 
    gather("watershed", "SWE", -Date) %>% 
    mutate(Year = year(Date),
           year_day = yday(Date)) %>% 
    group_by(watershed) %>% 
    mutate(last_swe = dplyr::lag(SWE, order_by = watershed),
           SWE_melt = -(SWE - last_swe),
           melt_risk = if_else(SWE_melt * swe_change_coef > 0, SWE_melt * swe_change_coef, 0)) %>% 
    ungroup() %>% 
    left_join(jmt_crossings_df %>% 
                select(JMT_Cross, crossing_id), 
              by = c("watershed" = "JMT_Cross"))
  
    mid_risk <- quantile(jmt_swe15_18$melt_risk, 0.90, na.rm = T)
    hi_risk <- quantile(jmt_swe15_18$melt_risk, 0.99, na.rm = T)
    
    jmt_swe15_18 <- jmt_swe15_18 %>% 
      mutate(risk_score = case_when(melt_risk <= mid_risk ~ 1,
                                    melt_risk >= mid_risk & melt_risk <= hi_risk~ 2,
                                    melt_risk >= hi_risk ~ 3))
  
    saveRDS(jmt_swe15_18, "scripts/Shiny_App/Data/swe_risk_2015_2018.rds")

#Get risk model object for estimate of SEW-driven change in risk
  load("scripts/risk_model_object.Rdata")
  
  swe_change_coef <- crossing_difficulty_lin_mod_adj$coefficients["peak_SWE_melt"]
    
      
#Download and process snowdepth rasters ##########
# Get data frame of googledrive ids for all the snowdepth rasters
  snodas_gd_depth <- drive_ls(as_id("1_IxGme096iUx6JJQY0nhONKSzBWaiI3k"))
      
#JMT extent  
  jmt_clipper <- extent(-120, -118, 36, 38)
      
#function to get snowdepth geotiff on particular day
get_snodas_tif <- function(date){
    snodas_id <- snodas_gd_depth %>% 
      slice(grep(date, name)) %>% 
      pull(id)
  
    snodas_data <- load_geotiff_from_googledrive(snodas_id)

    return(snodas_data)
}

# Function to download snowdepth raster and clip it
  snodas_jmt_clip <- function(date){
    to_clip <- get_snodas_tif(date)
    
    clipped <- crop(to_clip, jmt_clipper)
    
    return(clipped)
  }
  
# 2015 dates snow depth  
  dates_2015 <- seq(ymd("2015-01-01"), ymd("2015-12-31"), "days")
  
  snow_depth_2015_jmt <- lapply(dates_2015, snodas_jmt_clip)
  
  saveRDS(snow_depth_2015_jmt, "scripts/Shiny_App/Data/snow_depth_2015.rds")    

# 2016 dates snow depth  
  dates_2016 <- seq(ymd("2016-01-01"), ymd("2016-12-31"), "days")
  
  snow_depth_2016_jmt <- lapply(dates_2016, snodas_jmt_clip)
  
  saveRDS(snow_depth_2016_jmt, "scripts/Shiny_App/Data/snow_depth_2016.rds")    

# 2017 dates snow depth  
  dates_2017 <- seq(ymd("2017-01-01"), ymd("2017-12-31"), "days")
  
  snow_depth_2017_jmt <- lapply(dates_2017, snodas_jmt_clip)
  
  saveRDS(snow_depth_2017_jmt, "scripts/Shiny_App/Data/snow_depth_2017.rds")    

# 2018 dates snow depth  
  dates_2018 <- seq(ymd("2018-01-01"), ymd("2018-12-31"), "days")
  
  snow_depth_2018_jmt <- lapply(dates_2018, snodas_jmt_clip)
  
  saveRDS(snow_depth_2018_jmt, "scripts/Shiny_App/Data/snow_depth_2018.rds")    
      
        
#Get precip data ########
# Get data frame of googledrive ids for all the snowdepth rasters
  gd_precip <- drive_ls(as_id("16TsvJGV4YNxG6EER2RokJTOZMFurhsX4"))
      
# Function to download snowdepth raster and clip it
  precip_jmt_clip <- function(date){
    #Convert date input to character with no dashes
    date_char <- gsub("-", "", as.character(date))
    
    #Find folder within googledrive that contains date
    bil_id <- gd_precip$id[grepl(date_char, gd_precip$name)]
    
    print(c(date_char, bil_id))
    
    #Download raster from .bil in above folder
    to_clip <- load_raster_from_bil_googledrive(bil_id)
    
    #clip raster and return
    clipped <- crop(to_clip, jmt_clipper)
    
    return(clipped)
  }
  
# 2015 precipitation
  precip_2015_jmt <- lapply(dates_2015, precip_jmt_clip)
  
  saveRDS(precip_2015_jmt, "scripts/Shiny_App/Data/prism_ppt_jmt_clip_2015.rds")    
