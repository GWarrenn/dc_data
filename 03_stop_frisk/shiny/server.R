## Author: August Warren
## Description: Shiny Map of DC Stop and Frisk Data
## Date: 3/31/2018
## Status: Draft/Unpublished
## Specs: R version 3.3.2 (2016-10-31)

###################################
##
## Load packages
##
###################################

library(htmltools)
library(leaflet)
library(shiny)
library(rgdal)

sf_map_shiny <- readOGR(dsn = "C:/Users/augus/OneDrive/Documents/GitHub/dc_data/data/shapefiles",
          layer="sf_map_shiny")

function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    colorBy <- input$Race
    colorData <- sf_map_shiny[[colorBy]]
    pal <- colorBin("YlOrRd", colorData, 7, pretty = FALSE)

      labels <- sprintf(
        "<strong>%s</strong><br/>Total Stop & Frisk: %g
        <br/>White: %g
        <br/>Black: %g
        <br/>Hispanic: %g",
        sf_map_shiny$NBH_NAM, sf_map_shiny$Total,
        sf_map_shiny$White,sf_map_shiny$Black,sf_map_shiny$Hspnc.L
      ) %>% lapply(htmltools::HTML)
    
    leaflet(sf_map_shiny) %>% addTiles() %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(colorData),
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomright") %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setView(lng = -77.0369, lat = 38.9072, zoom = 12)
  })
  
}