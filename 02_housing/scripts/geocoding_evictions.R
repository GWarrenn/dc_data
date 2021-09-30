library(tidyverse)
library(httr)
library(readxl)

eviction_data <- read_excel("/Users/augustwarren/Documents/evictions.xlsx")

processed_data <- data.frame()

for(record in eviction_data$`Defendant Address`){
 
  if(record == "2660 DOUGLASS ROAD, #102 SE 20020"){
    record <- "2660 DOUGLASS PL, #102 SE 20020"
  }
   
  address <- gsub(pattern = " ",replacement = "%20",record)
  address <- gsub(pattern = ",",replacement = "",address)
  address <- gsub(pattern = "#",replacement = "",address)
  
  request <- GET(paste0("https://citizenatlas.dc.gov/newwebservices/locationverifier.asmx/findLocation2?str=",address,"&f=json"))
  
  lookup_df <- fromJSON(rawToChar(request$content))$returnDataset$Table1
  
  ## check for any imperfect matches, like 2660 douglass road fixed above
  
  if(nrow(lookup_df) > 1){
    print(record)
    print(request)
    print(lookup_df)
  }

  if(record == "2660 DOUGLASS PL, #102 SE 20020"){
    record <- "2660 DOUGLASS ROAD, #102 SE 20020"
  }
  
  lookup_df$original_address <- record
  
  processed_data <- rbind(processed_data,lookup_df)
  
}

eviction_geocoded <- merge(processed_data,eviction_data,by.x = "original_address",by.y = "Defendant Address")

write_csv(eviction_geocoded,"/Users/augustwarren/github/dc_data/data/evictions_geocoded.csv")
