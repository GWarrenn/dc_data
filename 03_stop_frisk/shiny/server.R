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


function(input, output, session) {
  
  dataInput <- reactive({
    readOGR(dsn = ".",
            layer="sf_map_shiny")
    
    if (input$Geography == "Neighborhood") {
      readOGR(dsn = ".",
              layer="sf_map_shiny")

    }
    else if (input$Geography == "Census Tract") {
      readOGR(dsn = ".",
              layer="tract_sf_map_shiny")
    }
  })
  
  labels <- reactive({
    if (input$Geography == "Neighborhood") {
      sprintf(
        "<strong>%s</strong><br/>Total Stop & Frisk: %g
        <br/>Juvenile: %g
        <br/>White: %g
        <br/>Black: %g
        <br/>Hispanic: %g",
        dataInput()$NBH_NAM, dataInput()$Total,dataInput()$Juvenil,
        dataInput()$White,dataInput()$Black,dataInput()$Hspnc.L
      ) %>% lapply(htmltools::HTML)
      
    }
    else if (input$Geography == "Census Tract") {
      labels <- sprintf(
        "<strong>Census Tract %s</strong><br/>Total Stop & Frisk: %g
        <br/>Juvenile: %g
        <br/>White: %g
        <br/>Black: %g
        <br/>Hispanic: %g",
        dataInput()$tract, dataInput()$Total,dataInput()$Juvenil,
        dataInput()$White,dataInput()$Black,dataInput()$Hspnc.L
      ) %>% lapply(htmltools::HTML)
    }
  })

  output$map <- renderLeaflet({
    
    colorBy <- input$Race
    colorData <- dataInput()[[colorBy]]
    pal <- colorBin("YlOrRd", colorData, 7, pretty = FALSE)
    
    leaflet(dataInput()) %>% addTiles() %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                 fillColor = ~pal(colorData),
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),
                  label = labels(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomright") %>% 
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
      setView(lng = -77.0369, lat = 38.9072, zoom = 12)
  })
  
}