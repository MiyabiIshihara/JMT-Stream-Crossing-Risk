# 
# Visualization of precipitation

library(shiny)
library(usmap)
library(ggplot2)
library(leaflet)
library(raster)
library(rgdal)
library(tidyverse)
library(plyr)

# data <- load("data/precip_data.RData")

# ----------------- # 
ui <- pageWithSidebar(
  headerPanel('Precipitation'),
  sidebarPanel(
    sliderInput(inputId = 'time', label = 'Time Range', 
                min = as.Date("2015-01-01"), max = as.Date("2015-12-31"), 
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
    data[[which(names(data) %in% gsub("-", "", input$time))]]
  })
  
  output$plot1 <- renderLeaflet({
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFFF"), 0:10,
           na.color = "transparent")
    leaflet() %>% addTiles() %>% 
      addRasterImage(selectedData(), colors = pal, opacity = 0.8) %>% 
      addLegend(pal = pal, values = values(selectedData()), title = "precipitation")
  })

  
}

shinyApp(ui, server)



