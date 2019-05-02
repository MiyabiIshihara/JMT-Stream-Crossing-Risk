## To dos
## Change layout of the dashboard so that plot is bigger

## app.R ##
library(shinydashboard)
library(shiny)
library(usmap)
library(ggplot2)
library(leaflet)
library(raster)
library(rgdal)
library(viridis)
library(lubridate)
library(tidyverse)
library(htmltools)
library(dplyr)
library(markdown)

# colors <- list(
#   logo = "",
#   logoHover = "",
#   collapseButton = "",
#   collapseButtonHover = "",
#   topBar = "",
#   sideBar = "",
#   
#   
# )

# ------------------------------- # 
#         import data             # 
# ------------------------------- # 
# Load data ###########
jmt_crossings_simplify <- readRDS("Data/crossing_points.rds")
jmt_all <- readRDS("Data/jmt_all_trail.rds")
jmt_main <- jmt_all %>% filter(Type == 'Main')
jmt_access <- jmt_all %>% filter(Type == 'Access')
jmt_watersheds <- readRDS("Data/jmt_watersheds.rds")
snow_depth_2015_jmt <- readRDS("Data/snow_depth_2015.rds")
snow_depth_2017_jmt <- readRDS("Data/snow_depth_2017.rds")
precip_2015_jmt <- readRDS("Data/prism_ppt_jmt_clip_2015.rds")
route_info <- readRDS("Data/route_info.rds")

# Combine multiple data into a list
data <- list("snow_depth" = snow_depth_2017_jmt, 
             "precip" = precip_2015_jmt)


# Make icon for stream crossings ############      
crossingIcon <- makeIcon(
  iconUrl = "www/icon.png",
  iconRetinaUrl = "www/icon2x.png",
  shadowUrl = "www/icon_shadow.png",
  # shadowRetinaUrl = "wwwn/icon2x_shadow.png",
  iconHeight = 35,
  iconWidth = 25,
  shadowHeight = 35,
  shadowWidth = 35,
  iconAnchorX = 12,
  iconAnchorY = 35,
  shadowAnchorX = 6,
  shadowAnchorY = 32,
  popupAnchorX = 6,
  popupAnchorY = -35
)


# ------------------------------- # 
#         ui components           # 
# ------------------------------- # 

date_selector <- function(){
  sliderInput(
    inputId = 'time', 
    label = '', 
    min = as.Date("2015-01-01"), 
    max = as.Date("2015-12-31"), 
    value = as.Date(c("2015-05-20")), 
    timeFormat = '%Y-%m-%d'
  )
}

trip_selector <- function(){
  column(
    12,
    fluidRow(
      column(
        8,
        style='padding:0px;',
        selectInput(
          inputId = "start_th",
          label = "Starting Trailhead:",
          choices = sort(unique(route_info$`entry trailhead`)),
          selected = "Happy Isles Trailhead"
        )
      ),
      column(
        4,
        style='padding:0px;',
        dateInput(
          inputId="start_date",
          label="Start Date:",
          value=today("PMT")
        )
      )
    ),

    fluidRow(
      column(
        8,
        style='padding:0px;',
        selectInput(
          inputId="end_th",
          label="Ending Trailhead:",
          choices=sort(unique(route_info$`exit trailhead`)),
          selected="Whitney Portal"
        )
      ),
      column(
        4,
        style='padding:0px;',
        dateInput(
          inputId="end_date",
          label="End Date:",
          value=(today("PMT") + 21)
        )
      )
    )
  )
}

raster_selector <- function(){
  radioButtons(
    inputId = "variable", 
    label = "",
    choices = c(
      "Snow Depth" = "snow_depth", 
      "Precipitation" = "precip"
    ), 
    selected = "snow_depth"
  )
}




# ------------------------------- # 
#                ui               # 
# ------------------------------- # 
#### Header content #### 
header <- dashboardHeader(
  title = "John Muir Trail Hazard",
  titleWidth = 400)


#### Sidebar content #### 
sidebar <- dashboardSidebar(
  width = 400,

  # Navigation
  column(
    12,
    style = "background-color:#4372a7;",
    
    # Header
    HTML(
      markdownToHTML(
        fragment.only=TRUE,
        text=c(
          "### Pages"
        )
      )
    ),
    
    # Menu Items
    sidebarMenu(
      menuItem(
        "Stream Crossing Map", 
        tabName="planning"#,
        # icon=icon("dashboard")
      ),
      menuItem(
        "Historical Stream Flow", 
        tabName="risk_cause"#,
        # icon=icon("th")
      ),
      menuItem(
        "Forecasted Stream Flow", 
        tabName="current_conditions"
      ),
      menuItem(
        "Stream Crossing Information", 
        tabName="about_us"
      ),
      menuItem(
        "About This Project", 
        tabName="about_us"
      )
    )
  ),
  
  # Spacing the same color as the Navigation
  column(
    12,
    style = "background-color:#4372a7;",
    HTML(
      markdownToHTML(
        fragment.only=TRUE, 
        text=c(
          "<br>"
        )
      )
    )
  ),
  
  # Spacing without a color
  column(
    12,
    HTML(
      markdownToHTML(
        fragment.only=TRUE, 
        text=c(
          "<br>"
        )
      )
    )
  ),
  
  # Trip Planner
  column(
    12,
    style = "background-color:#4372a7;",
    HTML(
      markdownToHTML(
        fragment.only=TRUE, 
        text=c(
          "### Trip Planner"
        )
      )
    ),
    date_selector(),
    trip_selector(),
    raster_selector()
  )
)

main_map <- function(){
  leafletOutput(
    "main_map",
  )
}

#### Body content ####
body <- dashboardBody(
  
  tabItems(
    # 1st tab content -- Planning Crossings
    tabItem(
      tabName = "planning",
      main_map()#,
      # map_option_panel()
    ),
    
    # 2nd tab content -- What causes risk?
    tabItem(
      tabName="risk_cause",
      includeMarkdown("docs/risk_cause.md"),
      plotOutput("time_series")
    ),
    
    # 3rd tab content -- Current conditions
    tabItem(
      tabName = "current_conditions"
    ),
    
    # 4th tab content -- About Us
    tabItem(
      tabName="about_us",
      includeMarkdown("docs/about_us.md")
    )
  ),
  
  # Force the map to fill the whole dashboard body
  tags$style(type = "text/css", "#main_map {height: calc(100vh - 80px) !important;}"),
  
  # Custom UI colors
  tags$head(
    tags$style(
      HTML(
        '
        /* logo */
        .skin-blue .main-header .logo {
        background-color: #173d69;
        }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
        background-color: #1a487d;
        }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
        background-color: #4372a7;
        }

        /* main sidebar */
        .skin-blue .main-sidebar {
        background-color: #173d69;
        }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #ffffff;
        }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
        background-color: #598db8;
        color: #000000;
        }

        /* other links in the sidebarmenu when hovered */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
        background-color: #66a1d2;
        }

        /* toggle button when hovered  */
        .skin-blue .main-header .navbar .sidebar-toggle:hover{
        background-color: #173d69;
        }
        '
      )
    )
  )
)

  
  


ui <- dashboardPage(
  title = "JMT WebApp", 
  skin = "blue", 
  header, 
  sidebar, 
  body
)



# ------------------------------- # 
#              server             # 
# ------------------------------- # 

server <- function(input, output) {
  
  # subset data
  selectedData <- reactive({
    selectedData <- data[[paste0(input$variable)]]
    selectedData <- selectedData[[yday(input$time)]]
    selectedData
  })
  
  compileRoute <- function(start, end) {
    # Get routes with that start and end
    routes <- route_info %>% filter(`entry trailhead`==start & `exit trailhead`==end)
    # Identify the shortest of these routes
    route <- routes %>% filter(length == min(length))
    # Get the segment indices associated with this route
    segment_ids <- route$edge_ids
    segment_ids <- substr(segment_ids, 2, nchar(segment_ids)-1)
    segment_ids <- as.integer(strsplit(segment_ids, ", ")[[1]])
    # Add one to account for R indices starting at 1 instead of 0
    segment_ids <- segment_ids + 1
    # Select the segments involves with this route
    segments = jmt_all  %>% slice(segment_ids)
    # Get the crossing indices associated with this route
    crossing_ids <- route$streams_crossed
    crossing_ids <- substr(crossing_ids, 2, nchar(crossing_ids)-1)
    crossing_ids <- as.integer(strsplit(crossing_ids, ", ")[[1]])
    # Add one to account for R indices starting at 1 instead of 0
    crossing_ids <- crossing_ids + 1
    # Select the crossings involved with this route
    crossings <- jmt_crossings_simplify %>% slice(crossing_ids)
    # Return just the segments associated with the route
    return(list('segments'=segments, 'crossings'=crossings, 'bounds'=st_bbox(segments)))
  }
  route <- reactive({compileRoute(input$start_th, input$end_th)})
  
  output$main_map <- renderLeaflet({
    pal <- colorNumeric(
      # "viridis",
      palette = colorRamp(c("#FFFFFF", "#014175"), interpolate = "spline"),
      domain=c(0,6000),
      na.color="transparent"
    )
    
    leaflet() %>%
      # Fit to the bounds of the selected route
      fitBounds(
        as.numeric(route()$bounds$xmin), 
        as.numeric(route()$bounds$ymin), 
        as.numeric(route()$bounds$xmax), 
        as.numeric(route()$bounds$ymax)) %>%
      
      ### Make basemap selector
      
      # # Load basemap
      # addProviderTiles(provider = "Esri.WorldTopoMap") %>%
      # Load basemap
      addProviderTiles(provider = providers$OpenStreetMap.Mapnik) %>%
      # Load raster layers
      addRasterImage(
        selectedData(),
        # colors = pal,
        colors = pal,
        opacity = 0.5,
        maxBytes = 10 * 1024 * 1024,
        group = "Snow Depth",
      ) %>%
      
      addLegend(
        pal = pal,
        values = values(selectedData()),
        title = "Snow Depth (mm)"
      ) %>% ## CHANGE
      
      # Map access trails
      addPolylines(
        data = jmt_access,
        color = "#202020",
        label = ~htmlEscape(Name),
        weight = 1,
        opacity = 0.9,
        group = "JMT Access Trails"
      ) %>%
      
      # Map main JMT trail
      addPolylines(
        data = jmt_main,
        color = "#202020",
        label = ~htmlEscape(Name),
        weight = 3,
        opacity = 0.9,
        group = "JMT Main Trail"
      ) %>%
      
      # Map stream Crossings
      addMarkers(
        # data = jmt_crossings_simplify,
        data = route()$crossings,
        label = ~htmlEscape(Crossing),
        icon = ~crossingIcon,
        popup = ~htmlEscape(popup_field),
        group = "JMT Main Stream Crossings"
      ) %>%
      
      # Map watersheds
      addPolygons(
        data = jmt_watersheds,
        color = "#253d66",
        opacity = 0.9,
        weight = 1,
        fillOpacity = 0.2,
        group = "Main Crossing Watersheds"
      ) %>%
      
      # Map selected route
      addPolylines(
        data = route()$segments,
        color = "#126b20",
        label = ~htmlEscape(Name),
        weight = 6,
        opacity = 1,
        group = "Selected Route"
      ) %>%
      
      addLayersControl(overlayGroups = c("Snow Depth",
                                         "JMT Access Trails",
                                         "JMT Main Trail",
                                         "JMT Main Stream Crossings",
                                         "Main Crossing Watersheds",
                                         "Selected Route"))
  }) # end of leaflet function
  
#Plot to show historical data at selected crossing(s) ####### 
# Select date range based on inputs
  date_range <- reactive({
    date_range <- list("start_day" = yday(input$start_date), 
                       "end_day" = yday(input$end_date))
    
    date_range
  })

  output$time_series <- renderPlot({
    swe_risk_2015_2018 %>% 
      mutate(Year = as.factor(Year)) %>% 
      filter(watershed %in% route()$crossings$Crossing) %>% # Filter crossings here, currently based on crossings generated in route selection
      gather("variable", "value", SWE, melt_risk) %>% 
        ggplot(aes(x = year_day, y = value, lty = Year, col = watershed)) + # Currently symbolizing year with linetype and crossing with color. If we get to a point where only selecting one crossing, should switch year to color and get rid of linetype (lty) argument
          annotate("rect", xmin = date_range()$start_day, xmax = date_range()$end_day,
                   ymin = 0, ymax = Inf,
                   fill = "grey20", alpha = 0.25) +
          geom_line() + 
          facet_grid(variable~., scales = "free_y") +
          theme_classic() +
          theme(legend.position = "bottom") +
          labs(x = "Day of the year", 
               title = "Snow Water Equivalent and Associated Risk",
               subtitle = "2015-2018 historical data")

  })
  
}


shinyApp(ui, server)

