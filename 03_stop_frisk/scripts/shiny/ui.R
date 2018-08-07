library(DT)  
library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Stop & Frisk and 2010 Census Comparison by Neighborhood"),
  mainPanel("Stop & Frisk data collected from 2012 - 2017 by DC Metropolitan Police Department\n"),
  DTOutput('tbl')
  
) #Fluidpage
) #ShinyUI