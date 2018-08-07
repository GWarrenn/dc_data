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
    datatable(filedata,rownames = FALSE, extensions ="FixedColumns",options = list(
      scrollX=TRUE,
      scrollY=500,
      fixedColumns = list(leftColumns = 2),
      autoWidth = TRUE,
      columnDefs = list(list(width = '250px', targets = c(1)),
                        list(className = 'dt-center', targets = 0:13),
                        list(visible=FALSE, targets=c(0))))) %>%
      formatStyle(format_cols,
        backgroundColor = styleInterval(0, c('lightpink', 'lightgreen'))) %>%
      #formatStyle("neighborhood","white-space"="nowrap") %>%
      #formatStyle(columns = c(2), width='200%') %>%
      formatPercentage(numeric_cols, 1)
  )
})
# columnDefs = list(list(visible=FALSE, targets=c(4)
# 