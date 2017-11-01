
library(geosphere)
library(sp)
library(reshape2)
library(dplyr)
library(ggplot2)

######################################################
##
## LAT/LONG LOOKUP USING GOOGLE API
##
######################################################

geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

######################################################
##
## GETTING LAT/LONG FROM HOUSING ADDRESS
##
######################################################

housing <- read.csv("September2017Sales.csv")

housing_geos <- data.frame(Address=character(),lat_long=character())
df <- data.frame(Address=character(),lat=double(),long=double())

for (h in housing$Address) {
  lat_long <- geocodeAdddress(h)
  lat <- lat_long[2]
  long <- lat_long[1]
  Address <- h
  df <- cbind(Address,lat,long)
  housing_geos <- rbind(housing_geos,df)
}

housing_latlong <- merge(housing,housing_geos,ID="Address",how="inner")
housing_latlong<-housing_latlong[complete.cases(housing_latlong), ]

sp.housing_latlong <- housing_latlong
sp.housing_latlong<-sp.housing_latlong[complete.cases(sp.housing_latlong), ]
coordinates(sp.housing_latlong) <- ~long+lat

## adding in metro data

data <- read.csv("Metro_Station_Entrances_in_DC.csv")

combined <- merge(data,housing_latlong,by=NULL) 

combined$lat <- as.numeric(as.character(combined$lat), digits=15)
combined$long <- as.numeric(as.character(combined$long), digits=15)

## calculating distances from each house to each metro stop

combined$distance <- distHaversine(combined[, 24:23],  combined[, 1:2]) / 1609
combined$distance <- combined$distance * 5280 #convert to feet

combined <- arrange(combined,Address,desc(distance))
closest_metro <- combined %>% group_by(Address) %>% arrange(distance) %>% slice(1)

closest_metro$adj_price <- as.numeric(gsub("\\$|,","",closest_metro$Close.Price)) / (as.numeric(closest_metro$BRs) + 1)

closest_metro <- closest_metro[(closest_metro$distance<10000),]

ggplot(closest_metro,aes(x=as.numeric(closest_metro$distance),y=closest_metro$adj_price)) + geom_point() 

