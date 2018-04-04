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

demo_vars <- c(
  "Total" = "Total",
  "Juvenile" = "Juvenil",
  "White" = "White",
  "Black" = "Black",
  "Hispanic/Latino" = "Hspnc.L")

geo_vars <- c(
  "Neighborhood" = "Neighborhood",
  "Census Tract" = "Census Tract")

bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("Geography", "Geography", geo_vars),
                selectInput("Race", "Race", demo_vars)
                
                
  ),
  h2("DC Stop & Frisk 2012-2017")
)
