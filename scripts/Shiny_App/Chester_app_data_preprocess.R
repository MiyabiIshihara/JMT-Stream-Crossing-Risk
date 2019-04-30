# Load and save all trails as an RDS
jmt_all <- readOGR(dsn = '/Users/chesterharvey/odrive/Google Drive (Berkeley)/DS421_Spring2019/Team 2 - JMT Stream Crossing Risk/Project_Data/Trail Data', layer = "Trail Edges")
jmt_all <- spTransform(jmt_all, "+proj=longlat +ellps=GRS80 +no_defs") %>% st_as_sf() %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
saveRDS(jmt_all, "scripts/Shiny_App/Data/jmt_all_trails.rds")

# Load and save route info as an RDS
route_info <- read_csv('/Users/chesterharvey/odrive/Google Drive (Berkeley)/DS421_Spring2019/Team 2 - JMT Stream Crossing Risk/Project_Data/Trail Data/route_info.csv')
saveRDS(route_info, "scripts/Shiny_App/Data/route_info.rds")