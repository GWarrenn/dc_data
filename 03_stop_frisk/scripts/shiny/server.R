library(shiny)
library(DT)  

shinyServer(function(input, output) {
  
  filedata <- read.csv("sf_nbh_summary.csv")
  
  format_cols <- c("Black.Diff","Hispanic.Latino.Diff","Juvenile.Diff","White.Diff")
  numeric_cols <- c("Black.stop_and_frisk","Black.census","Black.Diff",
                "Hispanic.Latino.stop_and_frisk","Hispanic.Latino.census","Hispanic.Latino.Diff",
                "Juvenile.stop_and_frisk","Juvenile.census","Juvenile.Diff","White.stop_and_frisk",
                "White.census","White.Diff")
  
  output$tbl = renderDT(
    datatable(filedata,rownames = FALSE, options = list(
      columnDefs = list(list(className = 'dt-center', targets = 0:13)))) %>%
      formatStyle(format_cols,
        backgroundColor = styleInterval(0, c('lightpink', 'lightgreen'))) %>%
      formatPercentage(numeric_cols, 1)
  )
})

