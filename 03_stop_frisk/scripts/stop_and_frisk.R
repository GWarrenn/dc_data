## Author: August Warren
## Description: Analysis of DC Stop and Frisk Data
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
library(scales)


source("03_stop_frisk/scripts/mygg_animate.r")

###################################
##
## Load Stop and Frisk Data
## Provided by MPD: https://mpdc.dc.gov/publication/stop-and-frisk-data-and-explanatory-notes
##
###################################

stop_frisk_1 <- read.xls("data/crime/SF_Field Contact_02202018.xls",sheet = 1)

stop_frisk_1$REASON.FOR.STOP <- "N/A"

stop_frisk_1$time_of_day <- format(round(strptime(stop_frisk_1$Report_taken_date_EST, "%m/%d/%Y %I:%M %p"), units="hours"), format="%H:%M")
stop_frisk_1$day_of_month <- format(strptime(stop_frisk_1$Report_taken_date_EST, "%m/%d/%Y %I:%M %p"), "%d")
stop_frisk_1$month <- format(as.Date(stop_frisk_1$Report_taken_date_EST,"%m/%d/%Y"), "%m")
stop_frisk_1$year_month <- as.Date(paste0(stop_frisk_1$month,"/","01/",as.numeric(stop_frisk_1$Year),sep=""),"%m/%d/%Y")

stop_frisk_2 <- read.xls("data/crime/SF_Field Contact_02202018.xlsx",sheet = 2, quote = "")

stop_frisk_2 <- as.data.frame(sapply(stop_frisk_2, function(x) gsub("\"", "", x)))

cols <- colnames(stop_frisk_2)

for (c in cols) {
  
  fixed <- gsub(pattern = "X.|.$",
                replacement = "",
                x = c)
  names(stop_frisk_2)[names(stop_frisk_2)==c] <- fixed
  
}

stop_frisk_2 <- stop_frisk_2 %>%
  rename(Report_taken_date_EST = Report.taken.date,
         Incident_Type = FIELD.CONTACT.TYPE,
         Subject_Race = Race,
         Subject_Sex = Sex,
         Subject_Ethnicity = Ethnicity,
         Incident.Location.PSA = PSA,
         Incident.Location.District = District,
         Block.Address = Block.address)

stop_frisk_2$time_of_day <- format(round(strptime(stop_frisk_2$Report_taken_date_EST, "%Y-%m-%d %H:%M"),units="hours"), "%H:%M")
stop_frisk_2$day_of_month <- format(strptime(stop_frisk_2$Report_taken_date_EST, "%Y-%m-%d %H:%M"), "%d")
stop_frisk_2$month <- format(as.Date(stop_frisk_2$Report_taken_date_EST,"%Y-%m-%d"), "%m")
stop_frisk_2$year_month <- as.Date(paste0(stop_frisk_2$month,"/","01/",as.numeric(as.character(stop_frisk_2$Year)),sep=""),"%m/%d/%Y")

stop_frisk_total <- rbind(stop_frisk_1,stop_frisk_2)

stop_frisk_total$id <- seq.int(nrow(stop_frisk_total))

###################################
##
## Matching incident block address in stop & frisk file to DC block data 
## in order to obtain incident lattitude and longitude
## h/t to Mahkah Wu for the idea and doing the hard work of finding
## the many typos in the address names
##
###################################

# extracting street name from block name

stop_frisk_total$Block.Address <- gsub(pattern = " BLK | BLOCK OF ",
                                       replacement = " B/O ",
                                       x = stop_frisk_total$Block.Address)

stop_frisk_total$street_name <- trimws(gsub(pattern = "^.* B/O",
                               replacement = "",
                               x = stop_frisk_total$Block.Address))

# fixing errors in street names

errors <- c("CAPTIOL","CAPITAL","ILINOI","/ SCAPITOL","13'TH","EAST CAP ST","E CAPITOL","MLK JR",
  "CAPITOL / 295N","MLKJR","MT PLEASANT","MARTIN LUTHER KING AV","MLK AV","4ST","7TH T",
  "V STNW","N CAPITOL ST","RI AV","N / W","$GA ","MD AV","AVENW","PA AV","STNW",
  "NORTH CAPITOL NE","19THST","7TH T","NEW YORK AVENE NE","ST;NW","13 TH","N CAP ST",
  "ECAPITAL ST",' ALY ',' AVE ',' AV ',' BLVD ',' BRG ',' CIR ',' CT ',' CRES ',' DR ',
  ' EXPY ',' FWY ',' GDN ',' GDNS ',' GRN ',' KYS ',' LN ',' LOOP ',' MEWS ',' PKWY ',
  ' PL ',' PLZ ',' RD ',' ROW ',' SQ ',' ST ',' TER ',' TR ',' WALK ',' WAY ',' ALY$',
  ' AVE$',' AV$',' BLVD$',' BRG$',' CIR$',' CT$',' CRES$',' DR$',' EXPY$',' FWY$',' GDN$',
  ' GDNS$',' GRN$',' KYS$',' LN$',' LOOP$',' MEWS$',' PKWY$',' PL$',' PLZ$',' RD$',' ROW$',
  ' SQ$',' ST$',' TER$',' TR$',' WALK$',' WAY$','WEST VA', ' MARYLAD ',' MD ',' MASS ', '[.]',
  'THS ',' THS',' IDEPENDENCE ')

fix <- c("CAPITOL","CAPITOL","ILLINOIS","SOUTH CAPITOL","13TH","EAST CAPITOL ST","EAST CAPITOL",
  "MARTIN LUTHER KING JR","CAPITOL STREET","MARTIN LUTHER KING JR","MOUNT PLEASANT",
  "MARTIN LUTHER KING JR AV","MARTIN LUTHER KING JR AV","4TH STREET","7TH STREET",
  "V ST NW","NORTH CAPITOL ST","RHODE ISLAND AV","NW","GEORGIA ","MARYLAND AV",
  "AVE NW","PENNSYLVANIA AV","ST NW","NORTH CAPITOL STREET","19TH STREET","7TH STREET",
  "NEW YORK AVENUE NE","ST NW","13TH","NORTH CAPITOL ST","EAST CAPITOL ST",' ALLEY ',
  ' AVENUE ',' AVENUE ',' BOULEVARD ',' BRIDGE ',' CIRCLE ',' COURT ',' CRESCENT ',' DRIVE ',
  ' EXPRESSWAY ',' FREEWAY ',' GARDENS ',' GARDENS ',' GREEN ',' KEYS ',' LANE ',' LOOP ',
  ' MEWS ',' PARKWAY ',' PLACE ',' PLAZA ',' ROAD ',' ROW ',' SQUARE ',' STREET ',' TERRACE ',
  ' TERRACE ',' WALK ',' WAY',' ALLEY',' AVENUE',' AVENUE',' BOULEVARD',' BRIDGE',
  ' CIRCLE',' COURT',' CRESCENT',' DRIVE',' EXPRESSWAY',' FREEWAY',' GARDENS',
  ' GARDENS',' GREEN',' KEYS',' LANE',' LOOP',' MEWS',' PARKWAY',' PLACE',
  ' PLAZA',' ROAD',' ROW',' SQUARE',' STREET',' TERRACE',' TERRACE',' WALK',' WAY','WEST VIRGINIA',
  ' MARYLAND ',' MARYLAND ',' MASSACHUSETTS ','','TH ',' TH ',' INDEPENDENCE ')

i <- 1

for (e in errors) {
  stop_frisk_total$street_name <- gsub(pattern = e,
                                       replacement = fix[i],
                                       x = stop_frisk_total$street_name)
  i <- i + 1
} 

stop_frisk_total$street_name <- gsub(pattern = "\\\\ |^OF ",
                                     replacement = "",
                                     x = stop_frisk_total$street_name)

## SOUTH/NORTH CAPITOL STREETS HAVE NE/SW/NE/SE BUT NOT IN BLACK CENTROIDS -- DROPPING

quads <- c("NE","NW","SW","SE")

for (q in quads) {
  
  cardinals <- c("NORTH","SOUTH")
  
  for (cards in cardinals) {
    stop_frisk_total$street_name <- gsub(pattern = paste(cards," CAPITOL STREET ",q,sep=""),
                                         replacement = paste(cards," CAPITOL STREET ",sep=""),
                                         x = stop_frisk_total$street_name)
  }
}

# extracting block number from block name

stop_frisk_total$block_number <- gsub(pattern = "B/O.*$",
                    replacement = "",
                    x = stop_frisk_total$Block.Address)

# merge stop and frisk to block data with lat/lon

block_data <- read.csv("data/shapefiles/Block_Centroids.csv")

block_data$ONSTREETDISPLAY <- trimws(block_data$ONSTREETDISPLAY)

combined <- merge(stop_frisk_total,block_data,
                  by.x = "street_name", 
                  by.y = "ONSTREETDISPLAY",
                  all.x = TRUE)

# use street number to filter down to correct block

combined <- combined %>%
  filter(as.numeric(block_number) - 5 < as.numeric(HIGHER_RANGE) &
          as.numeric(block_number) + 5 >= as.numeric(LOWER_RANGE))

# remove duplicates that occur from multiple intersections

combined <- combined[!duplicated(combined$id), ]

# finding cases that did not match the initial merge (have addresses in "STREET 1 / STREET 2" format)

unmatched <- merge(stop_frisk_total,select(combined,id:ESRI_OID),by = "id",all.x=TRUE)
unmatched <- unmatched %>%
            filter(is.na(LATITUDE))

unmatched$street_name <- gsub(pattern = " AND | & ",
                              replacement = " / ",
                              x = unmatched$street_name)

block_data$new_match_field <- paste(block_data$FROMSTREETDISPLAY,"/",block_data$ONSTREETDISPLAY)

second_merge <- merge(select(unmatched,id:block_number.x),block_data,
                      by.x = "street_name", by.y = "new_match_field",all.x=TRUE)

second_match <- second_merge %>%
  filter(!is.na(LATITUDE))

# finding cases that did not match the initial or second merge (have addresses in "13TH / STREET 2" format)

block_data$new_match_field <- paste(block_data$FROMSTNAME,"/",block_data$ONSTREETDISPLAY)

unmatched <- second_merge %>%
  filter(is.na(LATITUDE))

third_merge <- merge(select(unmatched,street_name:block_number.x),block_data,
                      by.x = "street_name", by.y = "new_match_field",all.x=TRUE)

third_match <- third_merge %>%
  filter(!is.na(LATITUDE))

colnames(second_match)[colnames(second_match)=="block_number.x"] <- "block_number"
colnames(third_match)[colnames(third_match)=="block_number.x"] <- "block_number"
colnames(combined)[colnames(combined)=="new_match_field"] <- "ONSTREETDISPLAY"

combined$ONSTREETDISPLAY <- ""

stop_frisk_matched <- rbind(combined,second_match,third_match)

unmatched <- third_merge %>%
  filter(is.na(LATITUDE))

# get lat long of stop & frisk addresses via google maps for remaining ~5,000 unmatched address 

#for(i in 1:nrow(stop_frisk)) {
#  result <- geocode(paste(stop_frisk$Block.Address[i],"Washington DC"), output = "latlona", source = "google")
#  stop_frisk$lon[i] <- as.numeric(result[1])
#  stop_frisk$lat[i] <- as.numeric(result[2])
#  try(stop_frisk$geoAddress[i] <- as.character(result[3]))
#  Sys.sleep(0.5)  # API only allows a certain amount of requests per second
#}


###################################
##
## Research questions (subject to change)
##
###################################

stop_frisk_total$race_ethn <- ifelse(stop_frisk_total$Subject_Ethnicity=='Hispanic Or Latino','Hispanic/Latino',
                                     stop_frisk_total$Subject_Race)

stop_frisk_total$race_ethn <- as.character(stop_frisk_total$Subject_Race)
stop_frisk_total$race_ethn[stop_frisk_total$Subject_Ethnicity == "Hispanic Or Latino"] <- "Hispanic/Latino" 

stop_frisk_total$juvenile <- ifelse(stop_frisk_total$Age == "Juvenile","Juvenile","Adult")
stop_frisk_total$juvenile[stop_frisk_total$Age == "Unknown" | stop_frisk_total$Age == ""] <- "Unknown" 

stop_frisk_matched$race_ethn <- ifelse(stop_frisk_matched$Subject_Ethnicity=='Hispanic Or Latino','Hispanic/Latino',
                                       stop_frisk_matched$Subject_Race)

stop_frisk_matched$race_ethn <- as.character(stop_frisk_matched$Subject_Race)
stop_frisk_matched$race_ethn[stop_frisk_matched$Subject_Ethnicity == "Hispanic Or Latino"] <- "Hispanic/Latino" 

stop_frisk_matched$juvenile <- ifelse(stop_frisk_matched$Age == "Juvenile","Juvenile","Adult")
stop_frisk_matched$juvenile[stop_frisk_matched$Age == "Unknown" | stop_frisk_matched$Age == ""] <- "Unknown" 

## what are the historical patterns of stop and frisk? -- monthly ts

stop_frisk_monthly <- stop_frisk_total %>%
  group_by(year_month) %>%
  summarise (n = n()) %>%
  arrange(year_month) %>%
  mutate(monthly = rollsum(n, k = 12, na.pad = TRUE, align = "right")) 

ggplot(stop_frisk_monthly,aes(x=year_month,y=monthly,group=1)) + 
  geom_line(size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Number of Stop and Frisk') + xlab("Month") + ggtitle("Total Number of Stop and Frisk Yearly Rolling Sum") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%d/%Y")

monthly_sf <- ggplot(stop_frisk_monthly,aes(x=year_month,y=n,group=1)) + 
  geom_point(size=2) +
  geom_smooth(method = lm,size=2) +
  #geom_vline(aes(xintercept = as.numeric(as.Date(dmy("2/1/2015")))), col = "black") +
  #stat_smooth(aes(x=year_month, y=n), method = lm, formula = y ~ poly(x, 10), se = TRUE,size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Number of Stop and Frisk') + xlab("Month") + ggtitle("Total Number of Stop and Frisk per Month") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y")

ggsave(plot = monthly_sf, "03_stop_frisk/images/01_monthly_sf.png", w = 10.67, h = 8,type = "cairo-png")

## hourly ts by race percentage

stop_frisk_hourly <- stop_frisk_total %>%
  filter(!is.na(stop_frisk_total$time_of_day)) %>%
  group_by(time_of_day) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n))

stop_frisk_hourly$race_ethn <- "Total"

stop_frisk_hourly <- stop_frisk_hourly[c("race_ethn", "time_of_day", "n","freq")]

stop_frisk_hourly_race <- stop_frisk_total %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino")) %>%
  group_by(race_ethn,time_of_day) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n)) 

stop_frisk_hourly_comb <- rbind(as.data.frame(stop_frisk_hourly_race),as.data.frame(stop_frisk_hourly))

stop_frisk_hourly_comb_plot <- ggplot(stop_frisk_hourly_comb, aes(x=time_of_day, y=freq, color=race_ethn,group=race_ethn)) + 
  geom_line(size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Percentage of Stop & Frisk Incidents') + xlab("Time of Day") + ggtitle("Total Stop and Frisk Incidents by Time of Day & Race") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_discrete(name="Legend")

ggsave(plot = stop_frisk_hourly_comb_plot, "03_stop_frisk/images/time_of_day_sf.png", w = 10.67, h = 8,type = "cairo-png")

## average age of stop and frisk by race 

stop_frisk_total$juvenile <- ifelse(stop_frisk_total$Age == "Juvenile","Juvenile",
                                                  "Adults")

sf_age_race <- stop_frisk_total %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino")) %>%
  group_by(race_ethn,juvenile) %>%
  summarise(count = n()) %>%
  mutate(freq=count/sum(count))

sf_age_race$race_ethn <- factor(sf_age_race$race_ethn,levels=c("White","Black","Hispanic/Latino"))

sf_race_youths <- ggplot(sf_age_race,aes(x=juvenile,y=freq,fill=juvenile)) + 
                  geom_bar(stat="identity") +
                  facet_wrap(~race_ethn) +
                  geom_text(aes(x=juvenile,y=freq,label=percent(round(freq,2))),data=sf_age_race, 
                              position=position_dodge(width=0.9), vjust=-0.5,size=5) +
                  theme_fivethirtyeight() +
                  scale_y_continuous(labels=scales::percent,limits=c(0,1)) +
                  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
                  labs(title = "Proportion of Juvenile/Adult Stops by Race/Ethnicity",
                       x = 'Race',
                       y ="",
                       fill="Legend")

ggsave(plot = sf_race_youths, "03_stop_frisk/images/sf_race_youths.png", w = 10.67, h = 8,type = "cairo-png")

sf_ages <- stop_frisk_total %>%
  filter(Age != "Juvenile")

sf_ages$Age <- as.double(as.character(sf_ages$Age)) 

sf_age_stats <- sf_ages %>%
  filter(!is.na(sf_ages$Age)) %>%
  group_by(race_ethn) %>%
  summarise(mean = mean(Age),
            median = median(Age),
            min = min(Age),
            max = max(Age),
            iqr = IQR(Age))

sf_age_dist <- ggplot(filter(sf_ages, race_ethn %in% c("White","Black","Hispanic/Latino")), 
       aes(x = age_numeric, y = race_ethn)) + geom_density_ridges() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  labs(x="Age",
       y="",
       title="Age of Stop & Frisk Incidents by Race",
       subtitle="Among Adults")

ggsave(plot = sf_age_dist, "03_stop_frisk/images/sf_age_dist.png", w = 10.67, h = 8,type = "cairo-png")

## what are the most cited reasons for stop and frisk?

stop_frisk_total$reason <- ifelse(grepl("Suspicious",stop_frisk_total$REASON.FOR.STOP),
                                "Suspicious Vehicles/Persons/Activities",stop_frisk_total$REASON.FOR.STOP)

reasons_for_stop <- stop_frisk_total %>%
  filter(!reason %in% c("N/A") & race_ethn %in% c("White","Black","Hispanic/Latino","Asian")) %>%
  group_by(race_ethn,reason) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

reasons_for_stop <- reasons_for_stop %>%
  filter(race_ethn %in% c("White", "Black", "Hispanic/Latino"))

reason_for_stop_plot <- ggplot(reasons_for_stop,aes(x=reason, y=freq)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label=percent(round(freq,2))), 
            hjust=-.1, position=position_dodge(.5)) +
  coord_flip() +
  theme_fivethirtyeight() +
  facet_wrap(~race_ethn) +
  scale_y_continuous(labels=scales::percent,limits=c(0,.5)) +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title = "Reason for Field Contact Report by Race",
       subtitle = "among all non-forcible stops", 
       x = 'Reason For Stop',
       y ="")

ggsave(plot = reason_for_stop_plot, "03_stop_frisk/images/reason_for_stop.png", w = 10.67, h = 8,type = "cairo-png")

stop_frisk_total$contact_type <- ifelse(stop_frisk_total$REASON.FOR.STOP=="N/A","Forcible","Non-forcible")

race_contact <- stop_frisk_total %>%
  group_by(contact_type,race_ethn) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino"))

race_contact_plot <- ggplot(race_contact,aes(x=race_ethn, y=freq,fill=race_ethn)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=percent(freq)), 
            vjust=-.5, position=position_dodge(.5), size=5) +
  theme_fivethirtyeight() +
  facet_grid(~contact_type) +
  scale_y_continuous(labels=scales::percent,limits=c(0,1)) +
  scale_x_discrete(limits=c("White","Black","Hispanic/Latino")) +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  xlab("") + ylab("") + ggtitle("Subject Race/Ethnicity by Contact Type")+
  scale_fill_discrete(name="Legend",limits=c("White","Black","Hispanic/Latino")) 

ggsave(plot = race_contact_plot, "03_stop_frisk/images/race_contact.png", w = 10.67, h = 8,type = "cairo-png")

gender_race <- stop_frisk_total %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino")) %>%
  group_by(race_ethn,contact_type,Subject_Sex) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))

gender_race_plot <- ggplot(gender_race,aes(x=Subject_Sex, y=freq,fill=Subject_Sex)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=percent(freq)), 
            vjust=-.5, position=position_dodge(.5), size=5) +
  theme_fivethirtyeight() +
  facet_grid(contact_type ~ race_ethn) +
  scale_y_continuous(labels=scales::percent,limits=c(0,1)) +
  scale_x_discrete(limits=c("Male","Female")) +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  xlab("") + ylab("") + ggtitle("Subject Sex by Race/Ethnicity & Contact Type")+
  scale_fill_discrete(name="Legend",limits=c("Male","Female")) 

ggsave(plot = gender_race_plot, "03_stop_frisk/images/gender_race_contact.png", w = 10.67, h = 8,type = "cairo-png")

###################################
##
## Matching incidents to neighborhoods & census tracts using DC neighborhood shapefile 
## provided by DC OpenData
## h/t: https://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile
##
###################################

## neighborhoods 

dc_neighborhoods <- readOGR("data/shapefiles",
                            layer="Neighborhood_Clusters")

coordinates(stop_frisk_matched) <- ~ LONGITUDE + LATITUDE

neighborhoods <- levels(dc_neighborhoods$NBH_NAMES)

nbh_sf_df <- data.frame()

for (n in neighborhoods) {
  
  print(paste("Classifying stop and frisk incidents in",n))
  
  test <- data.frame()
  
  cluster <- dc_neighborhoods[dc_neighborhoods$NBH_NAMES == n , ]

  proj4string(stop_frisk_matched) <- proj4string(cluster)
  
  test <- stop_frisk_matched[complete.cases(over(stop_frisk_matched, cluster)), ]
  test_df <- as.data.frame(test)
  try(test_df$neighborhood <- n)
  
  nbh_sf_df <- rbind(nbh_sf_df,test_df)

}

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

###################################
##
## Calculate race/age breakdowns of stop and frisk at neighborhood & tract level
##
###################################

## neighborhoods

nbh_sf_tot <- nbh_sf_df %>%
  group_by(neighborhood) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n)) 

nbh_sf_tot$demo_group <- "Total"
nbh_sf_tot$subgroup <- "Total"

nbh_sf_race <- nbh_sf_df %>%
  group_by(neighborhood,race_ethn) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n)) %>%
  rename(subgroup = race_ethn)

nbh_sf_race$demo_group <- "Race/Ethnicity"

nbh_sf_age <- nbh_sf_df %>%
  group_by(neighborhood,juvenile) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n)) %>%
  rename(subgroup = juvenile)
  
nbh_sf_age$demo_group <- "Age"

nbh_sf_demos <- dplyr::bind_rows(nbh_sf_tot,nbh_sf_age,nbh_sf_race)

additional_cluster_info <- read.csv("data/shapefiles/Neighborhood_Clusters.csv")

nbh_sf_demos <- merge(nbh_sf_demos,additional_cluster_info,by.x="neighborhood",by.y="NBH_NAMES")

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
## Process Census data 
##
###################################

census_data_original <- read.csv("data/census/comp_table_cltr00_pop.csv")

census_data_tot <- census_data_original %>%
  select(CLUSTER_TR2000,TotPop_2010) %>%
  melt(id.vars = c("CLUSTER_TR2000"))

census_data_tot$TotPop_2010 <- census_data_tot$value

census_data <- census_data_original %>%
  select(CLUSTER_TR2000,TotPop_2010,PctPopUnder18Years_2010,
         PctBlackNonHispBridge_2010,PctWhiteNonHispBridge_2010,PctHisp_2010)

census_data <- melt(census_data, id.vars = c("CLUSTER_TR2000","TotPop_2010"))

census_data <- rbind(census_data,census_data_tot)

census_data$pop <- ifelse(census_data$variable == "TotPop_2010",census_data$TotPop_2010,
                          census_data$TotPop_2010 * (census_data$value/100))

census_data <- census_data %>%
  mutate(variable = recode(census_data$variable, PctPopUnder18Years_2010 = "Juvenile",
                           PctBlackNonHispBridge_2010 = "Black",
                           PctWhiteNonHispBridge_2010 = "White",
                           PctHisp_2010 = "Hispanic/Latino",
                           TotPop_2010 ="Total")) %>%
  rename(census_value = value)

###################################
##
## Merge neighborhood-level census data to stop and frisk data
##
###################################

nbh_sf_demos_census <- merge(nbh_sf_demos,census_data,by.x=c("NAME","subgroup"),by.y=c("CLUSTER_TR2000","variable"))

###################################
##
## Plot stop and frisk and census
##
###################################

# overall neighborhood stop and frisk 

nbh_sf_totals_census_adj <- nbh_sf_demos_census %>%
  filter(demo_group == "Total") %>%
  mutate(adj_sf = (n/pop) * 10000,
         group = 1:39) %>%
  select(group,adj_sf) %>%
  rename(value = adj_sf)

nbh_names <- nbh_sf_demos_census %>%
  filter(demo_group == "Total") %>%
  mutate(group = 1:nrow(nbh_sf_totals_census_adj)) %>%
  select(neighborhood,group)

nbh_sf_totals_census <- nbh_sf_demos_census %>%
  filter(demo_group == "Total") %>%
  mutate(group = 1:nrow(nbh_sf_totals_census_adj)) %>%
  select(group,n) %>%
  rename(value = n)
         #group = neighborhood)

nbh_sf_totals_census$frame <- 1
nbh_sf_totals_census_adj$frame <- 2

# Interpolate data with tweenr
ts <- list(nbh_sf_totals_census, nbh_sf_totals_census_adj,nbh_sf_totals_census_adj,nbh_sf_totals_census)
tf <- tween_states(ts, tweenlength = 0.02, statelength = 0.001, ease = c('cubic-in-out'), nframes = 30)

init_sort <- nbh_sf_totals_census %>%
  arrange(-value) %>%
  mutate(rank = 1:nrow(nbh_sf_totals_census)) %>%
  select(rank,group)

tf <- merge(tf,nbh_names,by="group")
tf <- merge(tf,init_sort,by="group")

## h/t to: https://stackoverflow.com/questions/45569659/plot-titles-when-using-gganimate-with-tweenr

tf$type <- ifelse(tf$frame > 1.5,"per 10,000 people","")

# Make a barplot with frame
p <- ggplot(tf, aes(x=reorder(neighborhood,-rank), y=value, fill=value, frame= .frame,ttl=type)) + 
  geom_bar(stat='identity', position = "identity") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Total Stop & frisk') + xlab("Neighborhood") + ggtitle("Total Stop and Frisk")

mygg_animate(p,filename = "03_stop_frisk/images/nbh_sf.gif",interval = 0.1, title_frame=T,ani.height=768,ani.width=1024)

# overall racial sf & census breakdowns

# process 2018 census data from https://www.census.gov/quickfacts/fact/table/DC#viewtop

census_race <- read.csv("data/census/QuickFacts Apr-22-2018.csv",nrows = 20)

census_race <- slice(census_race, 13:19) %>%
  select(Fact,District.of.Columbia) %>%
  rename(value = District.of.Columbia,
         variable = Fact)

census_race$variable <- gsub("([A-Za-z]+).*", "\\1", census_race$variable)
census_race$value <- gsub("%", "", census_race$value)

census_race <- census_race %>%
  filter(variable %in% c("White","Black","Hispanic","Asian"))

census_race$variable[census_race$variable=="Hispanic"] <- "Hispanic/Latino"

sf_race <- stop_frisk_total %>%
  group_by(race_ethn) %>%
  summarise (n = n()) %>%
  mutate(freq=(n/sum(n))*100) %>%
  rename(variable = race_ethn,
         value = freq) %>%
  select(variable,value) %>%
  filter(variable %in% c("White","Black","Hispanic/Latino","Asian"))

sf_race$type <- 'Stop & Frisk'
census_race$type <- 'Census'

census_sf_race <- rbind(sf_race,census_race)
census_sf_race$value <- as.numeric(as.character(census_sf_race$value))/100

census_sf_race$type <- factor(census_sf_race$type, levels=c("Stop & Frisk","Census"))

census_sf_race <- ggplot(census_sf_race,aes(x=variable,y=as.numeric(value),fill=variable)) + 
  geom_bar(stat = "identity",position = "stack") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Stop and Frisk Incidents') + xlab("Race") + ggtitle("Stop & Frisk - Census Racial Comparison") + 
  scale_x_discrete(limits = c("White","Black","Hispanic/Latino","Asian")) +
  scale_y_continuous(labels=scales::percent,limits=c(0,1)) +
  geom_text(aes(x=variable,y=value,label=percent(round(value,2))),data=census_sf_race, 
            position=position_dodge(width=0.9), vjust=-0.5,size=5) +
  scale_fill_discrete(name="Legend") +
  facet_wrap(~type)

ggsave(plot = census_sf_race, "03_stop_frisk/images/03_census_sf_race.png", w = 10.67, h = 8,type = "cairo-png")

## scatter plot of neighborhood racial composition and percent of stop and frisk by race

nbh_sf_demos_census$diffcats <- cut(nbh_sf_demos_census$diff,4)

nbh_sf_race_plot <- ggplot(data=filter(nbh_sf_demos_census,demo_group %in% c("Race/Ethnicity")),aes(x=freq,y=census_value)) + 
  geom_point(aes(size=n,color=subgroup),alpha=.7) + scale_x_continuous(limits = c(0, 1),labels = scales::percent) + 
  scale_y_continuous(limits = c(0, 100)) + geom_abline(intercept = 0,slope = 100) + 
  geom_hline(yintercept = 50) + geom_vline(xintercept = .5) +
  theme_fivethirtyeight() +
  labs(x = "Percent of Stop and Frisk", y = "Percent of Neighborhood Residents") +
  ggtitle("Neighborhood Population v. Neighborhood Stop & Frisk") +
  scale_color_discrete(name="Legend") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text()) +
  scale_size(name   = "Total Stop & Frisk")

ggsave(plot = nbh_sf_race_plot, "03_stop_frisk/images/06_nbh_sf_race.png", w = 10.67, h = 10.67,type = "cairo-png")

## difference of neighborhood racial composition and percent of stop and frisk by race

nbh_sf_demos_census$diff <-  (nbh_sf_demos_census$census_value/100) - nbh_sf_demos_census$freq

nbh_diff_black<- ggplot(data=filter(nbh_sf_demos_census,subgroup %in% c("Black") & demo_group != "Total"), 
       aes(x = reorder(neighborhood, diff), 
           y = as.numeric(diff))) + 
  geom_point(aes(size=n,color=diff),alpha=.5,stat='identity') + coord_flip() +
  geom_hline(yintercept = 0) +
  theme_fivethirtyeight() +
  labs(title = "Neighborhood Difference in Stop & Frisk Rate & Population",
       subtitle = "among Black Residents (2012 - 2017)", 
       y = 'Stop & Frisk - Population',
       x="Neighborhood",size="Total Stop & Frisk") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,size=12),
        text = element_text(size=10)) + guides(color=FALSE) +
  scale_color_gradient(low = "red", high = "green", limits=c(min(nbh_sf_demos_census$diff),max(nbh_sf_demos_census$diff))) +
  scale_y_continuous(labels=scales::percent) +
  geom_segment(aes(y = 0, 
                   x = neighborhood, 
                   yend = diff, 
                   xend = neighborhood), 
               color = "black")

ggsave(plot = nbh_diff_black, "03_stop_frisk/images/07_nbh_diff_black.png", w = 10.67, h = 8,type = "cairo-png")

## total sf by day of month

stop_frisk_dom <- nbh_sf_df %>%  
  group_by(day_of_month) %>%
  summarise (n = n())

ggplot(stop_frisk_dom,aes(x=day_of_month,y=n,group=1)) + 
  geom_line(size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Number of Stop and Frisk') + xlab("Day of Month") + ggtitle("Total Number of Stop and Frisk by Day of Month") 

## total stop and frisk by race & neighborhood "whiteness"

nbh_sf_df <- merge(nbh_sf_df,additional_cluster_info,by.x="neighborhood",by.y="NBH_NAMES")

nbh_sf_df_census <- merge(nbh_sf_df,census_data_original,by.x=c("NAME"),by.y=c("CLUSTER_TR2000"))

nbh_sf_df_census$pct_non_white <- nbh_sf_df_census$PctAsianPINonHispBridge_2010 + 
  nbh_sf_df_census$PctBlackNonHispBridge_2010 +
  nbh_sf_df_census$PctHisp_2010

nbh_sf_df_census$non_white_bins <-cut(nbh_sf_df_census$pct_non_white, c(0,10,20,30,40,50,60,70,80,90,100))

stop_frisk_race <- nbh_sf_df_census %>%
  group_by(non_white_bins,race_ethn) %>%
  summarise (n = n()) %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino"))

total_sf_nbh_race <- nbh_sf_df_census %>%
  group_by(non_white_bins) %>%
  summarise (sf = n())

ggplot(stop_frisk_race,aes(x=non_white_bins,y=n,fill=race_ethn)) + 
  geom_bar(stat = "identity",position = "stack") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Total Stop and stop_frisk_race Incidents') + xlab("Race") + ggtitle("Stop & Frisk by Neighborhood Percentage of Residents of Color") + 
  #geom_text(aes(stop_frisk_race, vjust=-1,hjust=-0.1, position=position_dodge(.5), size=5,)) +
  scale_fill_discrete(name="Legend") 

## hourly stop and frisk by race & neighborhood "whiteness"

stop_frisk_race_tod <- nbh_sf_df_census %>%
  group_by(white_pop_pct,race_ethn,time_of_day) %>%
  summarise (n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino"))

ggplot(stop_frisk_race_tod,aes(x=time_of_day,y=freq,color=race_ethn,group=race_ethn)) + 
  stat_smooth(aes(x=time_of_day, y=freq), method = lm, formula = y ~ poly(x, 10), se = FALSE,size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Percent of Stop and Frisk Incidents') + xlab("Time of Day") + ggtitle("Frequency of Stop and Frisk by Hour") + 
  scale_color_discrete(name="Legend") + scale_y_continuous(labels=scales::percent) +
  facet_wrap(~white_pop_pct)
  
###################################
##
## Tying in crime data
##
###################################

## census tracts

dc_census_tracts <- readOGR("data/shapefiles",
                            layer="Census_Tracts_in_2010")

coordinates(stop_frisk_matched) <- ~ LONGITUDE + LATITUDE

years = c(2012,2013,2014,2015,2016)

crime_all_years = data.frame()

for (y in years) {
  
  crime <- readOGR(paste("data/shapefiles/Crime_Incidents_in_",y,".shp",sep = ""),
                              layer=paste("Crime_Incidents_in_",y,"",sep = ""))
  
  neighborhoods <- levels(dc_neighborhoods$NBH_NAMES)
  
  new_df <- data.frame()
  
  for (n in neighborhoods) {
    
    print(paste("Classifying crimes incidents in",n))
    
    test <- data.frame()
    
    cluster <- dc_neighborhoods[dc_neighborhoods$NBH_NAMES == n, ]
    
    proj4string(crime) <- proj4string(cluster)
    
    test <- crime[complete.cases(over(crime, cluster)), ]
    test_df <- as.data.frame(test)
    try(test_df$neighborhood <- n)
    
    new_df <- rbind(new_df,test_df)
    
  }

  new_df$month <- format(as.Date(new_df$REPORT_DAT,"%Y-%m-%d"), "%m")
  new_df$Year <- format(as.Date(new_df$REPORT_DAT,"%Y-%m-%d"), "%Y")
  new_df$year_month <- as.Date(paste0(new_df$month,"/","01/",as.numeric(new_df$Year),sep=""),"%m/%d/%Y")
  
  crime_neighborhood <- new_df %>%
    group_by(neighborhood,year_month) %>%
    summarise(crime = n())
  
  crime_neighborhood$year <- y
  
  crime_all_years <- rbind(crime_all_years,as.data.frame(crime_neighborhood))
  
}

crime_total_years_all_dc <- crime_all_years %>%
  group_by(year) %>%
  summarise(total_crime = sum(crime))

crime_avg_years <- crime_all_years %>%
  group_by(year,neighborhood) %>%
  summarise(tot_crime_yr = sum(crime))

crime_avg_years <- crime_avg_years %>%
  group_by(neighborhood) %>%
  summarise(crime = mean(tot_crime_yr))

## By year

nbh_sf_yearly <- nbh_sf_df %>%
  group_by(Year,neighborhood) %>%
  summarise (stop_frisks = n()) 

nbh_sf_yearly$prev_year <- as.numeric(as.character(nbh_sf_yearly$Year)) - 1

nbh_sf_yearly <- merge(nbh_sf_yearly,crime_all_years,
                        by.x = c("neighborhood","prev_year"),
                        by.y = c("neighborhood","year"))

## relationship between previous year in crime and current year stop and frisk

crime_frisk_yearly <- ggplot(data=nbh_sf_yearly,aes(x=stop_frisks,y=crime)) + 
  geom_point() + 
  geom_smooth(method='glm',formula=y~x) +
  theme_fivethirtyeight() +
  labs(x = "Current Year Stop and Frisk", y = "Previous Year Crime Reported") +
  ggtitle("Previous Year Crime Incidents v. Current Year Stop & Frisk") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

ggsave(plot = crime_frisk_yearly, "03_stop_frisk/images/crime_frisks.png", w = 10.67, h = 8,type = "cairo-png")

## crime modelling

nbh_sf_avg <- nbh_sf_yearly %>%
  group_by(neighborhood) %>%
  summarise(avg_sf = mean(stop_frisks),
            avg_prev_yr_crime = mean(crime))

par(mfrow=c(2,2)) 

nbh_sf_avg <- merge(nbh_sf_avg,additional_cluster_info,by.y="NBH_NAMES",by.x="neighborhood")
nbh_sf_avg <- merge(nbh_sf_avg,census_data_original,by.x="NAME",by.y="CLUSTER_TR2000")

nbh_sf_avg$pct_non_white <- nbh_sf_avg$PctAsianPINonHispBridge_2010 + 
  nbh_sf_avg$PctBlackNonHispBridge_2010 +
  nbh_sf_avg$PctHisp_2010

nbh_sf_avg$non_white_bins <-cut(nbh_sf_avg$pct_non_white, c(0,10,20,30,40,50,60,70,80,90,100))

yearly_model <- lm(formula = avg_sf ~ avg_prev_yr_crime,
   data = nbh_sf_avg)

stargazer(yearly_model,align = TRUE, out="03_stop_frisk/images/models.htm")

nbh_sf_avg$predicted <- predict(yearly_model)
nbh_sf_avg$residuals <- residuals(yearly_model)

ggplot(nbh_sf_avg, aes(x = avg_prev_yr_crime, y = avg_sf)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = avg_prev_yr_crime, yend = predicted), alpha = .2, size=2) +
  geom_point(aes(color = residuals), size=4) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  theme_fivethirtyeight() +
  labs(x = "Previous Year Total Crimes Reported", y = "Current Year Stop and Frisk") +
  ggtitle("Average Crime Only Model Residuals by Neighborhood Racial Composition") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

residuals_nbh_race <- nbh_sf_avg %>%
  group_by(pct_non_white) %>%
  summarise(avg_residuals = mean(residuals))

crime_model_residuals <- ggplot(residuals_nbh_race, aes(x = pct_non_white, y = avg_residuals)) +
  geom_smooth() +
  geom_point() +  
  geom_hline(yintercept = 0) +
  theme_fivethirtyeight() +
  labs(x = "Neighborhood Percent of Residents of Color", y = "Previous Year Crime Model Residual") +
  ggtitle("Average Crime-Only Model Residuals by Neighborhood Racial Composition") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

ggsave(plot = crime_model_residuals, "03_stop_frisk/images/crime_model_residuals.png", w = 10.67, h = 8,type = "cairo-png")

## total stop and frisk counts by neighborhood race from census

total_sf_nbh_race <- nbh_sf_df_census %>%
  group_by(Year,non_white_bins) %>%
  summarise (sf = n())

avg_sf_nbh_race <- total_sf_nbh_race %>%
  group_by(non_white_bins) %>%
  summarise (avg_sf = mean(sf))

crime_w_census <- merge(crime_avg_years,additional_cluster_info,by.y="NBH_NAMES",by.x="neighborhood")
crime_w_census <- merge(crime_w_census,census_data_original,by.x="NAME",by.y="CLUSTER_TR2000")

crime_w_census <- crime_w_census %>%
  select(NAME,neighborhood,PctBlackNonHispBridge_2010,PctHisp_2010,
         PctAsianPINonHispBridge_2010,TotPop_2010,crime)

crime_w_census$pct_non_white <- crime_w_census$PctAsianPINonHispBridge_2010 + 
  crime_w_census$PctBlackNonHispBridge_2010 +
  crime_w_census$PctHisp_2010

crime_w_census$non_white_bins <-cut(crime_w_census$pct_non_white, c(0,10,20,30,40,50,60,70,80,90,100))

crime_w_census <- crime_w_census %>%
  group_by(non_white_bins) %>%
  summarise (crime = mean(crime),pop=mean(TotPop_2010))

crime_sf_race_bins <- merge(crime_w_census,avg_sf_nbh_race,by="non_white_bins")

crime_sf_race_bins$adj_sf <- (crime_sf_race_bins$avg_sf / crime_sf_race_bins$pop) * 100
crime_sf_race_bins$adj_crime <- (crime_sf_race_bins$crime / crime_sf_race_bins$pop) * 100

crime_sf_race_l <- melt(crime_sf_race_bins, id.vars=c("non_white_bins")) %>%
  subset(variable %in% c("adj_sf","adj_crime"))

levels(crime_sf_race_l$variable) <- c("crime","pop","","Average Stop & Frisk","Average Crime")

sf_crime_nbh_race <- ggplot(crime_sf_race_l,aes(x=as.numeric(non_white_bins)*10,y=value,color=variable,group=as.character(variable))) + 
  geom_line(size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Crime/Stop & Frisk per 100 people') + xlab("Neighborhood Percent of Residents of Color") + 
  scale_color_discrete(name="Legend") +
  ggtitle("Average Yearly Stop & Frisk/Crime by \nNeighborhood Percentage of Residents of Color") 

ggsave(plot = sf_crime_nbh_race, "03_stop_frisk/images/10_sf_crime_nbh_race.png", w = 10.67, h = 8,type = "cairo-png")

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

## crime data w/ race

crimes_race <- read.csv("data/crime/crimes_anon.csv",stringsAsFactors = F)

crimes_race <- crimes_race %>% filter(!is.na(longitude))

dc_neighborhoods <- readOGR("data/shapefiles",
                            layer="Neighborhood_Clusters")

coordinates(crimes_race) <- ~ longitude + latitude

neighborhoods <- levels(dc_neighborhoods$NBH_NAMES)

nbh_crimes_df <- data.frame()

for (n in neighborhoods) {
  
  print(paste("Classifying crimes incidents in",n))
  
  test <- data.frame()
  
  cluster <- dc_neighborhoods[dc_neighborhoods$NBH_NAMES == n , ]
  
  proj4string(crimes_race) <- proj4string(cluster)
  
  test <- crimes_race[complete.cases(over(crimes_race, cluster)), ]
  test_df <- as.data.frame(test)
  try(test_df$neighborhood <- n)
  
  nbh_crimes_df <- rbind(nbh_crimes_df,test_df)
  
}

## pulling in neighborhood-level census

census_nbh_pct_black <- census_data %>%
  filter(variable == "Black")

census_nbh_pct_black$bins <- cut(census_nbh_pct_black$census_value, c(0,10,40,60,80,100))

nbh_crimes_df$race_ethn <- ifelse(as.character(nbh_crimes_df$ethnicity)=='Hispanic Or Latino','Hispanic/Latino',
                                   as.character(nbh_crimes_df$race))

nbh_crimes_df$race_ethn[nbh_crimes_df$ethnicity == "Hispanic Or Latino"] <- "Hispanic/Latino" 

## calculate neighborhood-level crimes by race

crimes_by_race_nbh <- nbh_crimes_df %>%
  group_by(neighborhood,race_ethn) %>%
  summarise(crimes=n()) %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino"))

## calculate neighborhood-level stop and firsks by race

stops_by_race_nbh <- nbh_sf_df %>%
  filter(Year == 2016) %>%
  group_by(neighborhood,race_ethn) %>%
  summarise(stop_frisks=n()) %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino"))

## merge arrests & stop and frisk then census

stops_crimes_nbh <- merge(crimes_by_race_nbh,stops_by_race_nbh,by=c("neighborhood","race_ethn"),
                           all = T)

additional_cluster_info <- read.csv("data/shapefiles/Neighborhood_Clusters.csv")

stops_crimes_nbh <- merge(stops_crimes_nbh,additional_cluster_info,by.x="neighborhood",by.y="NBH_NAMES")

stops_crimes_tracts_nbh <- merge(stops_crimes_nbh,census_nbh_pct_black,by.x="NAME",by.y="CLUSTER_TR2000")

stops_crimes_tracts_nbh$adj_sf <- (stops_crimes_tracts_nbh$stop_frisks / stops_crimes_tracts_nbh$pop) * 100
stops_crimes_tracts_nbh$adj_crimes <- (stops_crimes_tracts_nbh$crimes / stops_crimes_tracts_nbh$pop) * 100

## roll up neighborhoods and aggregate stop & frisk and arrests based on racial bins

stops_crimes_nbh_census_bins <- stops_crimes_tracts_nbh %>%
  replace(is.na(.), 0) %>%
  group_by(bins,race_ethn) %>%
  summarise(total_crimes = sum(adj_crimes),
            total_sf = sum(adj_sf))

## calculate crime to stop & frisk ratio

stops_crimes_nbh_census_bins$arrest_to_stops <- stops_crimes_nbh_census_bins$total_sf /
  stops_crimes_nbh_census_bins$total_crimes

stops_crimes_ratio <- ggplot(stops_crimes_nbh_census_bins,aes(x=bins,y=arrest_to_stops,color=race_ethn,group=as.character(race_ethn))) + 
  geom_line(size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Stop & Frisk to Crimes Ratio') + xlab("Neighborhood Racial Composition") + 
  scale_x_discrete(labels = c("< 10% black","10 - 40% black","40 - 60% black","> 60% black","80 - 100% black")) +
  scale_color_discrete(name="Legend") +
  #scale_y_continuous(limits = c(0,2.5)) +
  ggtitle("Stop & Frisk to Crimes Ratio (2016 Only)")

ggsave(plot = stops_crimes_ratio, "03_stop_frisk/images/stops_crimes_ratio.png", w = 10.67, h = 8,type = "cairo-png")

## re-creating stop and frisk by neighborhood percent using crime instead

nbh_crimes_race <- nbh_crimes_df %>%
  group_by(neighborhood,race_ethn) %>%
  summarise (crime_n = n()) %>%
  mutate(crime_freq=crime_n/sum(crime_n)) %>%
  rename(subgroup = race_ethn)

nbh_crime_race <- merge(nbh_crimes_race,nbh_sf_race,by=c("neighborhood","subgroup"),all=T)

nbh_crime_race <- nbh_crime_race %>%
  replace(., is.na(.), 0)

# adding neighborhood census data

nbh_crime_race <- merge(additional_cluster_info,nbh_crime_race,by.y="neighborhood",by.x="NBH_NAMES")
nbh_crime_race <- merge(nbh_crime_race,census_data,by.x=c("NAME","subgroup"),by.y=c("CLUSTER_TR2000","variable"),all.x=T)

nbh_crime_sf_race_scatter <- ggplot(data=filter(nbh_crime_race,subgroup %in% c("White","Black","Hispanic/Latino")),aes(x=freq,y=crime_freq)) + 
  #geom_point(aes(size=n,color=subgroup),alpha=.7) +
  geom_point(aes(size=census_value,color=subgroup),alpha=.7) +
  scale_x_continuous(limits = c(0, 1),labels = scales::percent) + 
  scale_y_continuous(limits = c(0, 1.0),labels = scales::percent) + 
  geom_abline(intercept = 0,slope = 1) + 
  geom_hline(yintercept = .5) + geom_vline(xintercept = .5) +
  theme_fivethirtyeight() +
  labs(x = "Percent of Stop and Frisk", y = "Percent of Crime") +
  ggtitle("Neighborhood Crime v. Stop & Frisk") +
  scale_color_discrete(name="Legend") +
  #scale_size_continuous(name="Total Stop & Frisk") +
  scale_size_continuous(name="Neighborhood Population") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text()) 

ggsave(plot = nbh_crime_sf_race_scatter, "03_stop_frisk/images/11_nbh_crime_sf_race_scatter_for_mahkah.png", w = 10.67, h = 10.67,type = "cairo-png")

nbh_crime_race$diff <-  (nbh_crime_race$crime_freq) - nbh_crime_race$freq

nbh_diff_crimes <- ggplot(data=filter(nbh_crime_race,subgroup %in% c("White","Black","Hispanic/Latino")), 
                        aes(x = reorder(neighborhood, diff), 
                            y = as.numeric(diff))) + 
  geom_point(aes(size=n,color=subgroup),alpha=.7,stat='identity') + coord_flip() +
  ggtitle("Neighborhood Difference in Stop & Frisk Rate & Crime Rate\nby Race (2016)") + 
  geom_hline(yintercept = 0) +
  theme_fivethirtyeight() +
  labs(x = "Neighborhood", y = "Stop & Frisk - Population") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size=10)) +
  scale_y_continuous(labels=scales::percent) +
  guides(color=guide_legend(title="Race/Ethnicity"),
         size=guide_legend(title="Total Stop & Frisk")) 

ggsave(plot = nbh_diff_crimes, "03_stop_frisk/images/12_nbh_diff_crimes.png", w = 10.67, h = 8,type = "cairo-png")

## poisson modelling

stops_crimes_tracts_nbh$race_ethn <- factor(stops_crimes_tracts_nbh$race_ethn,levels = c("White","Black","Hispanic/Latino"))

stop_model <- glm(stop_frisks ~ race_ethn + bins, family=quasipoisson,
                  offset=log(crimes),data = stops_crimes_tracts_nbh,subset=crimes>0 & stop_frisks>0)

stops_crimes_tracts_nbh <- stops_crimes_tracts_nbh %>%
  filter(crimes>0 & stop_frisks>0)

stop_model_test <- glm(avg_sf ~ non_white_bins, family=quasipoisson,
                  offset=log(avg_prev_yr_crime),data = nbh_sf_avg)

nbh_sf_avg$residuals_new <- residuals(stop_model_test)

ggplot(nbh_sf_avg, aes(x = non_white_bins, y = residuals_new)) +
  geom_smooth(method="loess") +
  geom_point() +  
  geom_hline(yintercept = 0) +
  theme_fivethirtyeight() +
  labs(x = "Neighborhood Percent of Residents of Color", y = "Poisson Crime Model Residual") +
  ggtitle("Average Crime-Only Model Residuals by Neighborhood Racial Composition") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

stops_crimes_tracts_nbh$residuals <- residuals(stop_model)

poisson_residuals <- ggplot(stops_crimes_tracts_nbh, aes(x = census_value, y = residuals)) +
  geom_smooth(method="loess") +
  geom_point(aes(color=stops_crimes_tracts_nbh$race_ethn)) +  
  geom_hline(yintercept = 0) +
  theme_fivethirtyeight() +
  labs(x = "Neighborhood Percent of Residents of Color", 
       y = "Poisson Crime Model Residual",
       color="Legend") +
  ggtitle("Poisson Model Residuals by Neighborhood Racial Composition") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

ggsave(plot = poisson_residuals, "03_stop_frisk/images/poisson_residuals.png", w = 10.67, h = 8,type = "cairo-png")

summary(stop_model)

stargazer(stop_model,align = TRUE, out="03_stop_frisk/images/poisson.htm")

coefs <- data.frame(stop_model$coefficients,check.rows = T)

Black = c(exp(coefs[1,1] + coefs[2,1] + 0), 
          exp(coefs[1,1] + coefs[2,1] + coefs[4,1]), 
          exp(coefs[1,1] + coefs[2,1] + coefs[5,1]),
          exp(coefs[1,1] + coefs[2,1] + coefs[6,1]),
          exp(coefs[1,1] + coefs[2,1] + coefs[7,1])) 

White = c(exp(coefs[1,1] + 0 + 0),
          exp(coefs[1,1] + 0 + coefs[4,1]), 
          exp(coefs[1,1] + 0 + coefs[5,1]),
          exp(coefs[1,1] + 0 + coefs[6,1]),
          exp(coefs[1,1] + 0 + coefs[7,1])) 

Hispanic = c(exp(coefs[1,1] + coefs[3,1] + 0),
             exp(coefs[1,1] + coefs[3,1] + coefs[4,1]), 
             exp(coefs[1,1] + coefs[3,1] + coefs[5,1]),
             exp(coefs[1,1] + coefs[3,1] + coefs[6,1]),
             exp(coefs[1,1] + coefs[3,1] + coefs[7,1]))

labels <- c("0-10% black","10-40% black","40-60% black","60-80% black","80-100% black")

results <- data.frame(labels, Black, White,Hispanic) 

results_l <- melt(results)

poisson_plot <- ggplot(results_l,aes(x=labels,y=value,color=variable,group=as.character(variable))) + 
  geom_line(size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title = "Estimated Stop & Frisk by Neighborhood Racial Composition",
       subtitle = "Poisson Regression results using constant term + ethnicity parameters for each neighborhood composition", 
       y = 'Stop & Frisks compared to Crime',
       x="Neighborhood Racial Composition") + 
  scale_x_discrete(labels = c("< 10% black","10 - 40% black","40 - 60% black","> 60% black","80 - 100% black")) +
  scale_color_discrete(name="Legend") +
  scale_y_continuous(trans = "log",breaks = c(0,.1,.2,.5,1,2))

ggsave(plot = poisson_plot, "03_stop_frisk/images/13_poisson_plot.png", w = 10.67, h = 8,type = "cairo-png")
