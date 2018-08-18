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

## census tracts

dc_census_tracts <- readOGR("data/shapefiles",
                            layer="Census_Tracts_in_2010")

coordinates(stop_frisk_matched) <- ~ LONGITUDE + LATITUDE

tracts <- levels(dc_census_tracts$TRACT)

tracts_sf_df <- data.frame()

for (t in tracts) {
  
  print(paste("Classifying stop and frisk incidents in",t))
  
  test <- data.frame()
  
  tract <- dc_census_tracts[dc_census_tracts$TRACT == t , ]
  
  proj4string(stop_frisk_matched) <- proj4string(tract)
  
  test <- stop_frisk_matched[complete.cases(over(stop_frisk_matched, tract)), ]
  test_df <- as.data.frame(test)
  try(test_df$tract <- t)
  
  tracts_sf_df <- rbind(tracts_sf_df,test_df)
  
}


## tracts

tracts_sf_tot <- tracts_sf_df %>%
  group_by(tract) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n)) 

tracts_sf_tot$demo_group <- "Total"
tracts_sf_tot$subgroup <- "Total"

tracts_sf_race <- tracts_sf_df %>%
  group_by(tract,race_ethn) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n)) %>%
  rename(subgroup = race_ethn)

tracts_sf_race$demo_group <- "Race/Ethnicity"

tracts_sf_age <- tracts_sf_df %>%
  group_by(tract,juvenile) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n)) %>%
  rename(subgroup = juvenile) %>%
  subset(subgroup %in% c("Juvenile"))

tracts_sf_age$demo_group <- "Age"

tracts_sf_demos <- dplyr::bind_rows(tracts_sf_tot,tracts_sf_age,tracts_sf_race)


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


## Arrest data

arrests <- read.csv("data/crime/arrest_charges_anon.csv",stringsAsFactors = F)

arrests <- arrests %>% filter(!is.na(latitude))

dc_neighborhoods <- readOGR("data/shapefiles",
                            layer="Neighborhood_Clusters")

coordinates(arrests) <- ~ longitude + latitude

neighborhoods <- levels(dc_neighborhoods$NBH_NAMES)

nbh_arrests_df <- data.frame()

for (n in neighborhoods) {
  
  print(paste("Classifying stop and frisk incidents in",n))
  
  test <- data.frame()
  
  cluster <- dc_neighborhoods[dc_neighborhoods$NBH_NAMES == n , ]
  
  proj4string(arrests) <- proj4string(cluster)
  
  test <- arrests[complete.cases(over(arrests, cluster)), ]
  test_df <- as.data.frame(test)
  try(test_df$neighborhood <- n)
  
  nbh_arrests_df <- rbind(nbh_arrests_df,test_df)
  
}

## pulling in neighborhood-level census

census_nbh_pct_black <- census_data %>%
  filter(variable == "Black")

census_nbh_pct_black$bins <- cut(census_nbh_pct_black$census_value, c(0,10,40,60,80,100))

nbh_arrests_df$race_ethn <- ifelse(as.character(nbh_arrests_df$ethnicity)=='Hispanic Or Latino','Hispanic/Latino',
                                   as.character(nbh_arrests_df$race))

nbh_arrests_df$race_ethn[nbh_arrests_df$ethnicity == "Hispanic Or Latino"] <- "Hispanic/Latino" 

## calculate neighborhood-level arrests by race

arrests_by_race_nbh <- nbh_arrests_df %>%
  group_by(neighborhood,race_ethn) %>%
  summarise(arrests=n()) %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino"))

## calculate neighborhood-level stop and firsks by race

stops_by_race_nbh <- nbh_sf_df %>%
  filter(Year == 2016) %>%
  group_by(neighborhood,race_ethn) %>%
  summarise(stop_frisks=n()) %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino"))

## merge arrests & stop and frisk then census

stops_arrests_nbh <- merge(arrests_by_race_nbh,stops_by_race_nbh,by=c("neighborhood","race_ethn"),
                           all = T)

additional_cluster_info <- read.csv("data/shapefiles/Neighborhood_Clusters.csv")

stops_arrests_nbh <- merge(stops_arrests_nbh,additional_cluster_info,by.x="neighborhood",by.y="NBH_NAMES")

stops_arrests_tracts_nbh <- merge(stops_arrests_nbh,census_nbh_pct_black,by.x="NAME",by.y="CLUSTER_TR2000")

stops_arrests_tracts_nbh$adj_sf <- (stops_arrests_tracts_nbh$stop_frisks / stops_arrests_tracts_nbh$pop) * 100
stops_arrests_tracts_nbh$adj_arrests <- (stops_arrests_tracts_nbh$arrests / stops_arrests_tracts_nbh$pop) * 100

## roll up neighborhoods and aggregate stop & frisk and arrests based on racial bins

stops_arrests_nbh_census_bins <- stops_arrests_tracts_nbh %>%
  replace(is.na(.), 0) %>%
  group_by(bins,race_ethn) %>%
  summarise(total_arrests = sum(adj_arrests),
            total_sf = sum(adj_sf))

## calculate arest to stop & frisk ratio

stops_arrests_nbh_census_bins$arrest_to_stops <- stops_arrests_nbh_census_bins$total_sf /
  stops_arrests_nbh_census_bins$total_arrests

stops_arrest_ratio <- ggplot(stops_arrests_nbh_census_bins,aes(x=bins,y=arrest_to_stops,color=race_ethn,group=as.character(race_ethn))) + 
  geom_line(size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Stops to Arrests Ratio') + xlab("Neighborhood Racial Composition") + 
  scale_x_discrete(labels = c("< 10% black","10 - 40% black","40 - 60% black","> 60% black","80 - 100% black")) +
  scale_color_discrete(name="Legend") +
  scale_y_continuous(limits = c(0,2.5)) +
  ggtitle("Stop & Frisk to Arrest Ratio (2016 Only)")

ggsave(plot = stops_arrest_ratio, "03_stop_frisk/images/stops_arrest_ratio.png", w = 10.67, h = 8,type = "cairo-png")

