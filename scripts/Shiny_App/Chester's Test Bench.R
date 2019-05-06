source("functions.R")

jmt_crossings <- readRDS("Data/crossing_points.rds")
jmt_all <- readRDS("Data/jmt_all_trails.rds")
swe_risk_2015_2018 <- readRDS("Data/swe_risk_2015_2018.rds")

route <- compileRoute('Agnew Meadows','Whitney Portal', as.Date(c("2019-05-22")), as.Date(c("2019-06-22")), jmt_crossings, jmt_all, swe_risk_2015_2018) 

View(route$crossings)

crossings <- route$crossings

get_risk_score <- function(id, crossing_day_of_year) {
  # Retrieve risk scores for the crossing on this
  crossing_scores <- swe_risk_2015_2018 %>% filter(crossing_id==id & year_day==crossing_day_of_year)
  return(max(crossing_scores$risk_score))
}
crossings$risk_score <- map2_dbl(crossings$id, crossings$crossing_day_of_year, get_risk_score)

View(crossings)