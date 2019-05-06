# Load and save all trails as an RDS
jmt_all <- readOGR(dsn = '/Users/chesterharvey/odrive/Google Drive (Berkeley)/DS421_Spring2019/Team 2 - JMT Stream Crossing Risk/Project_Data/Trail Data', layer = "Trail Edges")
# Project into WGS84
jmt_all <- spTransform(jmt_all, "+proj=longlat +ellps=GRS80 +no_defs") %>% st_as_sf() %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# Save to RDS
saveRDS(jmt_all, "scripts/Shiny_App/Data/jmt_all_trails.rds")

# Load route info
route_info <- read_csv('/Users/chesterharvey/odrive/Google Drive (Berkeley)/DS421_Spring2019/Team 2 - JMT Stream Crossing Risk/Project_Data/Trail Data/route_info.csv')
# Make an id column starting at 0
route_info$route_id <- 0:(nrow(route_info)-1)
# Load the crossing positions
crossing_positions <- read_csv('/Users/chesterharvey/odrive/Google Drive (Berkeley)/DS421_Spring2019/Team 2 - JMT Stream Crossing Risk/Project_Data/JMT Survey/route_crossings.csv')
# Parse the edge IDs for each route as vectors
parse_vector <- function(string_vector) {
  elements <- substr(string_vector, 2, nchar(string_vector)-1)
  elements <- as.integer(strsplit(elements, ", ")[[1]])
  return(elements)
}
route_info$segment_ids <- route_info$edge_ids %>% lapply(parse_vector)
# Retrieve crossing IDs
filtered_crossings <- function(id) {
  crossings <- crossing_positions %>%
    filter(route_id==id) %>%
    arrange(crossing_position)
}
collect_crossing_ids <- function(id) {
  return(filtered_crossings(id)$crossing_id)
}
route_info$crossing_ids <- route_info$route_id %>% sapply(collect_crossing_ids)
# Retrieve crossing positions
collect_crossing_positions <- function(id) {
  return(filtered_crossings(id)$crossing_position)
}
route_info$crossing_positions <- route_info$route_id %>% sapply(collect_crossing_positions)
# Drop column storing unordered stream ids
route_info <- route_info %>% select(-streams_crossed, -edge_ids)
# Save to RDS
saveRDS(route_info, "scripts/Shiny_App/Data/route_info.rds")


