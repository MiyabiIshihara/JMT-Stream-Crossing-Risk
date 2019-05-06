source("functions.R")

jmt_crossings <- readRDS("Data/crossing_points.rds")
jmt_all <- readRDS("Data/jmt_all_trails.rds")
swe_risk_2015_2018 <- readRDS("Data/swe_risk_2015_2018.rds")

route <- compileRoute('Happy Isles Trailhead','Whitney Portal', as.Date(c("2019-05-22")), as.Date(c("2019-06-22")), jmt_crossings, jmt_all, swe_risk_2015_2018) 
crossings <- route$crossings
crossing <- selectCrossingFromTable('Ireland Creek', crossings)

snow_depth_2015_jmt <- readRDS("Data/snow_depth_2015.rds")
precip_2015_jmt <- readRDS("Data/prism_ppt_jmt_clip_2015.rds")
raster_data <- list(
  "snow_depth" = snow_depth_2015_jmt, 
  "precip" = precip_2015_jmt
)

selectRaster(crossing, raster_data, 'snow_depth')
