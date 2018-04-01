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

vars <- c(
  "Total" = "Total",
  "White" = "White",
  "Black" = "Black",
  "Hispanic/Latino" = "Hispanic/Latino")

bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("Race", "Race", vars)
  ),
  h2("DC Stop & Frisk 2012-2017")
)
