## Author: August Warren
## Description: Analysis of DC Stop and Frisk Data
## Date: 3/25/2018
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

###################################
##
## Load Stop and Frisk Data
## Provided by MPD: https://mpdc.dc.gov/publication/stop-and-frisk-data-and-explanatory-notes
##
###################################

stop_frisk_total <- read.xls("data/crime/SF_Field Contact_02202018.xlsx")

###################################
##
## Research questions (subject to change)
##
###################################

## what are the historical patterns of stop and frisk? -- weekly ts

stop_frisk_total$year_month <- format(as.Date(stop_frisk_total$Report_taken_date_EST), "%Y-%m")

stop_frisk_monthly <- stop_frisk_total %>%
  group_by(year_month) %>%
  summarise (n = n())

ggplot(stop_frisk_monthly,aes(x=year_month,y=n,group=1)) + 
  geom_line(size=2) 
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Number of Stop and Frisk') + xlab("Month") + ggtitle("Total Number of Stop and Frisk per Month") +
  scale_x_date()

## when do most stop and frisks occur? -- hourly ts
  
stop_frisk_total$time_of_day <- format(strptime(stop_frisk_total$Report_taken_date_EST, "%Y-%m-%d %H:%M:%S"), "%H")
  
  
## where do most stop and frisks occur? -- bar graph of neighborhood
## what are the most cited reasons for stop and frisk?

stop_frisk <- stop_frisk_total[stop_frisk_total$Year==2016,]
stop_frisk_total$id <- seq.int(nrow(stop_frisk_total))


###################################
##
## Matching incident block address in stop & frisk file to DC block data 
## in order to obtain incident lattitude and longitude
## h/t to Mahkah Wu for the idea
##
###################################

# extracting street name from block name

stop_frisk_total$street_name <- trimws(gsub(pattern = "^.* B/O",
                               replacement = "",
                               x = stop_frisk_total$Block.Address))

bad <- c(" ST "," AVE "," RD "," PL "," DR ","THS "," ST$")
good <- c(" STREET "," AVENUE "," ROAD "," PLACE "," DRIVE "," TH "," STREET")
i <- 1

for (b in bad) {
  
  stop_frisk_total$street_name <- gsub(pattern = b,
                                       replacement = good[i],
                                       x = stop_frisk_total$street_name)
  i <- i + 1
}

# extracting block number from block name

stop_frisk_total$block_number <- gsub(pattern = "B/O.*$",
                               replacement = "",
                               x = stop_frisk_total$Block.Address)

# merge stop and frisk to block data with lat/lon

block_data$ONSTREETDISPLAY <- trimws(block_data$ONSTREETDISPLAY)

combined <- merge(stop_frisk_total,block_data,
                  by.x = "street_name", 
                  by.y = "ONSTREETDISPLAY",
                  all.x = TRUE)

# use street number to filter down to correct block

combined <- combined %>%
  filter(as.numeric(block_number) - 5 < as.numeric(HIGHER_RANGE) &
          as.numeric(block_number) >= as.numeric(LOWER_RANGE))

# remove duplicates that occer from multiple intersections

combined <- combined[!duplicated(combined$id), ]

unmatched <- merge(stop_frisk_total,select(combined,id:ESRI_OID),by = "id",all.x=TRUE)

unmatched <- unmatched %>%
            filter(is.na(LATITUDE))

block_data$new_match_field <- paste(block_data$FROMSTREETDISPLAY,"/",block_data$TOSTREETDISPLAY)

second_merge <- merge(select(unmatched,id:block_number.x),block_data,by.x = "street_name", by.y = "new_match_field",all.x=TRUE)

matched <- second_merge %>%
  filter(!is.na(LATITUDE))

colnames(matched)[colnames(matched)=="block_number.x"] <- "block_number"
colnames(combined)[colnames(combined)=="new_match_field"] <- "ONSTREETDISPLAY"

stop_frisk_new <- rbind(combined,matched)

# get lat long of stop & frisk addresses via google maps for remaining ~5,000 unmatched address 

for(i in 1:nrow(stop_frisk)) {
  result <- geocode(paste(stop_frisk$Block.Address[i],"Washington DC"), output = "latlona", source = "google")
  stop_frisk$lon[i] <- as.numeric(result[1])
  stop_frisk$lat[i] <- as.numeric(result[2])
  try(stop_frisk$geoAddress[i] <- as.character(result[3]))
  Sys.sleep(0.5)  # API only allows a certain amount of requests per second
}

###################################
##
## Matching incidents to neighborhoods using DC neighborhood shapefile 
## provided by DC OPenData
## h/t: https://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile
##
###################################

dc_neighborhoods <- readOGR("Neighborhood_Clusters.shp",
                            layer="Neighborhood_Clusters")

coordinates(stop_frisk_new) <- ~ LONGITUDE + LATITUDE

neighborhoods <- levels(dc_neighborhoods$NBH_NAMES)

new_df <- data.frame()

for (n in neighborhoods) {
  
  print(paste("Classifying stop and frisk incidents in",n))
  
  test <- data.frame()
  
  cluster <- dc_neighborhoods[dc_neighborhoods$NBH_NAMES == n, ]

  proj4string(stop_frisk_new) <- proj4string(cluster)
  
  test <- stop_frisk_new[complete.cases(over(stop_frisk_new, cluster)), ]
  test_df <- as.data.frame(test)
  try(test_df$neighborhood <- n)
  
  new_df <- rbind(new_df,test_df)

}

###################################
##
## Calculate race/age breakdowns of stop ad frisk at neighborhood level
##
###################################

new_df$race_ethn <- ifelse(new_df$Subject_Ethnicity=='Hispanic Or Latino','Hispanic/Latino',
                           new_df$Subject_Race)

new_df$race_ethn <- as.character(new_df$Subject_Race)
new_df$race_ethn[new_df$Subject_Ethnicity == "Hispanic Or Latino"] <- "Hispanic/Latino" 

new_df$juvenile <- ifelse(new_df$Age == "Juvenile","Juvenile","Adult")
new_df$juvenile[new_df$Age == "Unknown" | new_df$Age == ""] <- "Unknown" 

neighborhood_stop_frisk_race <- new_df %>%
  group_by(neighborhood,race_ethn) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n)) %>%
  rename(subgroup = race_ethn)

neighborhood_stop_frisk_race$demo_group <- "Race/Ethnicity"

neighborhood_stop_frisk_age <- new_df %>%
  group_by(neighborhood,juvenile) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n)) %>%
  rename(subgroup = juvenile)
  
neighborhood_stop_frisk_age$demo_group <- "Age"

neighborhood_stop_frisk_demos <- rbind(neighborhood_stop_frisk_age,neighborhood_stop_frisk_race)

additional_cluster_info <- read.csv("data/shapefiles/Neighborhood_Clusters.csv")

neighborhood_stop_frisk_demos <- merge(neighborhood_stop_frisk_demos,additional_cluster_info,by.x="neighborhood",by.y="NBH_NAMES")

###################################
##
## Process Census data 
##
###################################

census_data <- read.csv("data/census/comp_table_cltr00_pop.csv")

census_data <- census_data %>%
  select(CLUSTER_TR2000,TotPop_2010,PctPopUnder18Years_2010,
         PctBlackNonHispBridge_2010,PctWhiteNonHispBridge_2010,PctHisp_2010)

census_data <- melt(census_data, id.vars = c("CLUSTER_TR2000","TotPop_2010"))

census_data <- census_data %>%
  mutate(variable = recode(census_data$variable, PctPopUnder18Years_2010 = "Juvenile",
                           PctBlackNonHispBridge_2010 = "Black",
                           PctWhiteNonHispBridge_2010 = "White",
                           PctHisp_2010 = "Hispanic/Latino")) %>%
  rename(census_value = value)

###################################
##
## Merge neighborhood-level census data to stop and frisk data
##
###################################

nbh_sf_demos_census <- merge(neighborhood_stop_frisk_demos,census_data,by.x=c("NAME","subgroup"),by.y=c("CLUSTER_TR2000","variable"))

###################################
##
## Plot neighborhood-level stop and frisk & census data
##
###################################

ggplot(data=filter(nbh_sf_demos_census,subgroup %in% c("Black")),aes(x=freq,y=census_value)) + 
  geom_point(aes(size=n)) + scale_x_continuous(limits = c(0, 1),labels = scales::percent) + scale_y_continuous(limits = c(0, 100)) +
  geom_abline(intercept = 0,slope = 100) + geom_hline(yintercept = 50) + geom_vline(xintercept = .5) +
  theme_fivethirtyeight() +
  labs(x = "Percent of Stop and Frisk", y = "Percent of Neighborhood Black Residents") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

nbh_sf_demos_census$diff <-  (nbh_sf_demos_census$census_value/100) - nbh_sf_demos_census$freq

ggplot(data=filter(nbh_sf_demos_census,subgroup %in% c("Black")), 
       aes(x = reorder(neighborhood, diff), 
           y = as.numeric(diff),fill=diff)) + 
  geom_bar(stat = "identity",show.legend=FALSE) + coord_flip() +
  ggtitle("Neighborhood Difference in Stop & Frisk Rate & Population") + 
  #geom_text(data=filter(nbh_sf_demos_census,subgroup %in% c("Black")),aes(label=nbh_sf_demos_census$n), vjust=0,hjust=-0.1, position=position_dodge(.5), size=3,) + 
  theme_fivethirtyeight() +
  labs(x = "Neighborhood", y = "Stop & Frisk - Population") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size=10)) +
  scale_fill_gradient(low = "red", high = "green", limits=c(min(nbh_sf_demos_census$diff),max(nbh_sf_demos_census$diff))) +
  scale_y_continuous(labels=scales::percent) 
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 80))



###################################
##
## Tying in crime data
##
###################################
years = c(2012,2013,2014,2015,2016)

crime_all_years = data.frame()

for (y in years) {
  
  crime <- readOGR(paste("Crime_Incidents_in_",y,".shp",sep = ""),
                              layer=paste("Crime_Incidents_in_",y,"",sep = ""))
  
  neighborhoods <- levels(dc_neighborhoods$NBH_NAMES)
  
  new_df <- data.frame()
  
  for (n in neighborhoods) {
    
    print(paste("Classifying stop and frisk incidents in",n))
    
    test <- data.frame()
    
    cluster <- dc_neighborhoods[dc_neighborhoods$NBH_NAMES == n, ]
    
    proj4string(crime) <- proj4string(cluster)
    
    test <- crime[complete.cases(over(crime, cluster)), ]
    test_df <- as.data.frame(test)
    try(test_df$neighborhood <- n)
    
    new_df <- rbind(new_df,test_df)
    
  }
  
  crime_neighborhood <- new_df %>%
    group_by(neighborhood) %>%
    summarise (arrests = n()) %>%
    mutate(arrest_pct =n/sum(n))
  
  crime_neighborhood$year <- y
  
  crime_all_years <- rbind(crime_all_years,crime_neighborhood)

}

neighborhood_stop_frisk_yearly <- new_df %>%
  group_by(Year,neighborhood) %>%
  summarise (stop_frisks = n()) 

neighborhood_stop_frisk_yearly <- merge(neighborhood_stop_frisk_yearly,crime_all_years,
                                        by.x = c("neighborhood","Year"),
                                        by.y = c("neighborhood","year"))

ggplot(data=filter(neighborhood_stop_frisk_yearly,neighborhood %in% c("Columbia Heights, Mt. Pleasant, Pleasant Plains, Park View"))) + 
  geom_line(aes(x=Year,y=stop_frisks,group=1)) +
  geom_line(aes(x=Year,y=n/5,group=1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Arrests")) +
  theme_fivethirtyeight() +
  scale_colour_manual(values = c("blue", "red")) +
  labs(x = "Year", y = "Stop & Frisk", colour = "Legend") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

ggplot(data=neighborhood_stop_frisk_yearly) + 
  geom_line(aes(x=Year,y=stop_frisks,group=neighborhood)) +
  theme_fivethirtyeight() +
  labs(x = "Year", y = "Stop & Frisk", colour = "Legend") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

## total stop and frisk counts by neighborhood race from census
