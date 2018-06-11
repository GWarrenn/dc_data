## Author: August Warren
## Description: Un-used analysis for DC Stop and Frisk Data
## Date: 5/22/2018
## Status: Draft/Unpublished
## Specs: R version 3.3.2 (2016-10-31)

###################################
##
## Load packages
##
###################################

library(ggplot2)
library(dplyr)
library(gdata)
library(rgdal)
library(sp)
library(rgeos)
library(geosphere)
library(ggmap)
library(reshape2)
library(ggthemes)
library(zoo)
library(ggridges)
library(lubridate)
library(fuzzyjoin)
library(tidyverse)
library(tweenr)


###################################
##
## Prep map data for shiny
##
###################################

## neighborhoods

nbh_sf_demos_census$adj_arrests_pop <- (nbh_sf_demos_census$n / nbh_sf_demos_census$pop) * 100

nbh_sf_demos_wide <- dcast(nbh_sf_demos_census, neighborhood ~ subgroup , value.var=c("n"))
nbh_sf_demos_wide_adj <- dcast(nbh_sf_demos_census, neighborhood ~ subgroup , value.var=c("adj_arrests_pop"))

nbh_sf_demos_wide <- merge(nbh_sf_demos_wide,nbh_sf_demos_wide_adj,by = "neighborhood",suffixes=c("_unadj","_adj"))

nbh_sf_demos_wide <- merge(nbh_sf_demos_wide,additional_cluster_info,by.x="neighborhood",by.y="NBH_NAMES")

poly_df <- as.data.frame(dc_neighborhoods)

poly_df <- merge(nbh_sf_demos_wide,poly_df,
                 by.x="NAME",
                 by.y="NAME")

dc_neighborhoods <- dc_neighborhoods[as.numeric(as.character(dc_neighborhoods$OBJECTID)) < 40,]
row.names(dc_neighborhoods@data) <- NULL
row.names(dc_neighborhoods@data) <- as.character(dc_neighborhoods$NBH_NAMES)

poly_df <- poly_df[order(as.numeric(as.character(poly_df$OBJECTID))),] 

row.names(poly_df) <- NULL
row.names(poly_df) <- 0:38

names <- levels(dc_neighborhoods$NBH_NAMES)

i <- 1

for (n in names) {
  slot(slot(dc_neighborhoods, "polygons")[[i]], "ID") = names[i]
  i <- i + 1
}

sf_map_shiny <- SpatialPolygonsDataFrame(dc_neighborhoods, poly_df,match.ID = FALSE)

writeOGR(sf_map_shiny, "03_stop_frisk/shiny", "sf_map_shiny", driver="ESRI Shapefile",overwrite_layer = TRUE)

## census tracts

tracts_sf_demos_wide <- dcast(tracts_sf_demos, tract ~ subgroup , value.var="n")

tract_poly_df <- as.data.frame(dc_census_tracts)

tract_poly_df <- merge(tracts_sf_demos_wide,tract_poly_df,
                       by.x="tract",
                       by.y="TRACT",all.y = TRUE)

tract_poly_df$OBJECTID <- as.numeric(as.character(tract_poly_df$OBJECTID))
tract_poly_df <- tract_poly_df[order(as.numeric(as.character(tract_poly_df$OBJECTID))),] 

row.names(tract_poly_df) <- NULL
row.names(tract_poly_df) <- 0:178

names <- levels(dc_census_tracts$TRACT)

i <- 1

for (n in names) {
  slot(slot(dc_census_tracts, "polygons")[[i]], "ID") = names[i]
  i <- i + 1
}

tract_sf_map_shiny <- SpatialPolygonsDataFrame(dc_census_tracts, tract_poly_df,match.ID = FALSE)

writeOGR(tract_sf_map_shiny, "03_stop_frisk/shiny", "tract_sf_map_shiny", driver="ESRI Shapefile",
         overwrite_layer = TRUE)

###################################
##
## calculating average time of "nearest" crime
##
###################################

sf_2016 <- as.data.frame(stop_frisk_matched) %>%
  filter(as.numeric(as.character(Year)) >= 2016)

sf_2016$Report_taken_date_EST <- gsub(pattern = "/16",replacement = "/2016",x = sf_2016$Report_taken_date_EST)
sf_2016$sf_posix_datetime <- as.POSIXct(strptime(sf_2016$Report_taken_date_EST, "%m/%d/%Y %I:%M %p"))

sf_2016$id <- seq.int(nrow(sf_2016))

sf_2016 <- sf_2016 %>%
  rename(LONGITUDE.y = ï..X)

crime_data <- readOGR("data/shapefiles/Crime_Incidents_in_2016.shp",
                      layer="Crime_Incidents_in_2016")

crime_data <- as.data.frame(crime_data)

crime_data$REPORT_DAT <- gsub("T"," ",crime_data$REPORT_DAT)

crime_data$clean_date <- strftime(crime_data$REPORT_DAT, "%Y-%m-%d %H:%M:%S")
crime_data$month <- format(as.Date(crime_data$REPORT_DAT,"%Y-%m-%d %H:%M:%S"), "%m")

crime_data$arrest_posix_datetime <- as.POSIXct(strptime(crime_data$REPORT_DAT, "%Y-%m-%d %H:%M:%S"))

fuzzy <- geo_inner_join(sf_2016,crime_data, by = c("LONGITUDE","LATITUDE"), 
                        method = "haversine", max_dist = .5,
                        unit = c("miles"))

fuzzy$time_diff <- difftime(fuzzy$sf_posix_datetime,
                            fuzzy$arrest_posix_datetime,
                            units="mins")

fuzzy <- fuzzy[ , !duplicated(colnames(fuzzy))]

fuzzy <- fuzzy %>%
  filter(time_diff >= 0)

fuzzy$distance <- distHaversine(fuzzy[, 20:21],  fuzzy[, 45:44]) / 1609
fuzzy$distance <- fuzzy$distance * 5280 #convert to feet

closest_crimes <- fuzzy %>% group_by(id) %>% arrange(time_diff,distance) %>% slice(1)

mean_by_race <- closest_crimes %>%
  group_by(race_ethn) %>%
  summarise(mean = sum((mean(time_diff)/60)))

closes_crime_plot <- ggplot(closest_crimes,aes(x=as.numeric((time_diff)/60))) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = mean(as.numeric((closest_crimes$time_diff)/60))) +
  labs(x="Hours",y="Frequency") +
  ggtitle("Hours Before Stop & Frisk of Closest Reported Crime") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0,150))

ggsave(plot = closest_crime, "03_stop_frisk/images/closes_crime_plot.png", w = 10.67, h = 8,type = "cairo-png")

# %15 of all stop & frisk incidents occured within an hour of crime incident 
