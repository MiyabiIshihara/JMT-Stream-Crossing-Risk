library(shiny)
library(usmap)
library(ggplot2)
library(leaflet)
library(raster)
library(rgdal)
library(viridis)
library(tidyverse)

#Use directory path relative to location of shiny app
load("shiny_data.Rdata")

# ----------------- # 
ui <- pageWithSidebar(
  headerPanel('Snow Depth'),
  sidebarPanel(
    sliderInput(inputId = 'time', label = 'Time Range', 
                min = as.Date("2015-01-01"), max = as.Date("2018-12-31"), 
                value = as.Date(c("2015-01-01")), 
                timeFormat = '%Y-%m-%d')
  ),
  mainPanel(
    leafletOutput('plot1')
  )
)



# ----------------- # 

server <- function(input, output, session) {

  # subset data 
  selectedData <- reactive({
    get_snodas_tif(input$time)
  })
  
  output$plot1 <- renderLeaflet({
    pal <- colorNumeric("viridis", domain = c(0,50000),
           na.color = "transparent")
  leaflet() %>% 
    setView(lng = -118.869194, lat = 37.235921, zoom = 8) %>% 
    addProviderTiles(provider = "Esri.WorldTopoMap") %>% 
    addRasterImage(selectedData(), colors = pal, opacity = 0.4) %>% 
    addLegend(pal = pal, values = values(selectedData()), title = "Snow Depth") %>% 
    addPolylines(data = jmt_access, color = "green", label = ~htmlEscape(Name),
                 weight = 2, opacity = 0.75) %>% 
    addPolylines(data = jmt_main, color = "brown", label = ~htmlEscape(Name),
                 weight = 4, opacity = 0.9) %>%
    addMarkers(data = jmt_crossings_simplify, 
               label = ~htmlEscape(Crossing), icon = ~crossingIcon,
               popup = ~htmlEscape(popup_field)) %>% 
    addPolygons(data = jmt_watersheds, color = "blue",
                opacity = 0.9, weight = 1, fillOpacity = 0.2)
  })

  
}

shinyApp(ui, server)
