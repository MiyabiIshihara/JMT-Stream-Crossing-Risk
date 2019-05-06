# UI component to select trip by start and end point/date
tripSelector <- function(route_info){
  fluidRow(
    column(
      8,
      style='padding:0px;',
      selectInput(
        inputId = "start_th",
        label = "Starting Trailhead",
        choices = sort(unique(route_info$`entry trailhead`)),
        selected = "Happy Isles Trailhead",
      ),
      selectInput(
        inputId="end_th",
        label="Ending Trailhead",
        choices=sort(unique(route_info$`exit trailhead`)),
        selected="Whitney Portal",
      )
    ),
    column(
      4,
      style='padding:0px;',
      dateInput(
        inputId="start_date",
        label="Start Date",
        value=today("PMT"),
        format = "M dd, yyyy"
      ),
      dateInput(
        inputId="end_date",
        label="End Date",
        value=(today("PMT") + 21),
        format = "M dd, yyyy"
      )
    )
  )
}

# Dynamic UI component to select crossing of interest based on previously selected route
crossingSelector <- function(route) {
  return(
    renderUI({
      # slider_values <- c(
      #   route()$start,
      #   as.character(route()$crossing$Crossing),
      #   route()$end
      # )
      slider_values <- route()$crossing$Crossing
      fluidRow(
        sliderTextInput(
          inputId = 'selected_crossing',
          label = "Scroll through stream crossings along your trip:",
          choices = slider_values,
          selected = slider_values[c(1)],
          width = '100%',
          force_edges = TRUE,
          hide_min_max = TRUE
        )
      )
    })
  )
}

# Filter the crossings table to get only the selected crossing
selectCrossingFromTable <- function(selected_crossing, crossings) {
  return(crossings %>% filter(Crossing==selected_crossing) %>% as_tibble())
}

# Filter crossings based on their risk
filterCrossingRisk <- function(crossings, risk) {
  return(crossings %>% filter(risk_score==risk) %>% as_tibble())
}

# Select the raster to display based on the selector
selectRaster <- function(crossing, raster_data, raster_selection) {
  selected_day = crossing$crossing_day_of_year
  selected_raster <- raster_data[[paste0(raster_selection)]]
  selected_raster <- selected_raster[[selected_day]]
  return(selected_raster)
}

# UI Component to select the snow depth vs precip raster
rasterSelector <- function(){
  fluidRow(
    radioButtons(
      inputId = "raster_selection", 
      label = "Chose reference layer:",
      choices = c(
        "Snow Depth" = "snow_depth", 
        "Precipitation" = "precip"
      ), 
      selected = "snow_depth",
      width = 320
    )
  )
}

# Define a leaflet icon
icon <- function(url, retinaUrl) {
  return(
    makeIcon(
      iconUrl = url,
      # iconRetinaUrl = retinaUrl,
      shadowUrl = "www/icon_shadow.png",
      iconHeight = 35,
      iconWidth = 25,
      shadowHeight = 35,
      shadowWidth = 35,
      iconAnchorX = 12,
      iconAnchorY = 35,
      shadowAnchorX = 6,
      shadowAnchorY = 32,
      popupAnchorX = 1,
      popupAnchorY = -35
    )
  )
}

small_icon <- function(url, retinaUrl) {
  return(
    makeIcon(
      iconUrl = url,
      # iconRetinaUrl = retinaUrl,
      shadowUrl = "www/icon_shadow.png",
      iconHeight = 18,
      iconWidth = 13,
      shadowHeight = 18,
      shadowWidth = 18,
      iconAnchorX = 6,
      iconAnchorY = 18,
      shadowAnchorX = 3,
      shadowAnchorY = 16,
      popupAnchorX = 1,
      popupAnchorY = -18
    )
  )
}

# Set up a number of leaflet icons
blueIcon <- icon("www/icon.png", "www/icon2x.png")
blueIconSmall <- small_icon("www/icon_small.png", "www/icon2x.png")
redIcon <- icon("www/icon_red.png", "www/icon2x_red.png")
redIconSmall <- small_icon("www/icon_red_small.png", "www/icon2x_red.png")
yellowIcon <- icon("www/icon_yellow.png", "www/icon2x_yellow.png")
yellowIconSmall <- small_icon("www/icon_yellow_small.png", "www/icon2x_yellow.png")
greenIcon <- icon("www/icon_green.png", "www/icon2x_green.png")
greenIconSmall <- small_icon("www/icon_green_small.png", "www/icon2x_green.png")
blueIconSemi <- icon("www/icon_semi.png", "www/icon2x_semi.png")
redIconSemi <- icon("www/icon_red_semi.png", "www/icon2x_red_semi.png")
yellowIconSemi <- icon("www/icon_yellow_semi.png", "www/icon2x_yellow_semi.png")
greenIconSemi <- icon("www/icon_green_semi.png", "www/icon2x_green_semi.png")


# Compile the route between start and end trailheads
compileRoute <- function(start_th, end_th, start_date, end_date, jmt_crossings, jmt_all, swe_risk_2015_2018, route_info) {
  # Get shortest routes with that start and end
  route <- route_info %>%
    filter(`entry trailhead`==start_th & `exit trailhead`==end_th) %>%
    filter(length == min(length))
  # Select the segments involved with this route
  segment_ids <- route$segment_ids[[1]] + 1
  segments = jmt_all %>% slice(segment_ids)
  # Select the crossings involved with this route
  # Create a copy of the crossings table specific to this route; include row numbers
  crossings <- jmt_crossings %>% mutate(id = row_number())
  
  # Restrict to crossings along this route
  crossing_ids <- route$crossing_ids[[1]] + 1
  crossings <- crossings %>% slice(crossing_ids)
  
  # Match the dataframe order to the predefined crossing order
  crossings <- crossings[match(route$crossing_ids[[1]] + 1, crossings$id),]
  
  # Add crossing distances
  crossings$crossing_dist <- route$crossing_positions[[1]]
  
  # Calculate crossing days
  crossing_day <- function(dist) {
    days <- end_date - start_date + 1
    crossing_days <- floor(dist / route$length * days)
    crossing_date <- start_date + crossing_days
    return(crossing_date)
  }
  crossings$crossing_date <- crossings$crossing_dist %>% sapply(crossing_day) %>% as.Date()
  
  # Calculate the crossing's day of the year
  day_of_year <- function(date) {
    day <- yday(date)
    return(day)
  }
  crossings$crossing_day_of_year <- crossings$crossing_date %>% sapply(day_of_year)
  
  # Retrieve crossing risk scores based on day of year and crossing ids
  get_risk_score <- function(id, crossing_day_of_year) {
    # Retrieve risk scores for this crossing on this day of the year
    crossing_scores <- swe_risk_2015_2018 %>% filter(crossing_id==id & year_day==crossing_day_of_year)
    # Take the max acrossa available years
    return(max(crossing_scores$risk_score))
  }
  crossings$risk_score <- map2_dbl(crossings$id, crossings$crossing_day_of_year, get_risk_score)

  return(
    list(
      'segment_geoms'=segments,
      'crossings'=crossings,
      'bounds'=st_bbox(segments),
      'length'=route$length,
      'start'=route$`entry trailhead`,
      'end'=route$`exit trailhead`,
      'id'=route$route_id
    )
  )
}



