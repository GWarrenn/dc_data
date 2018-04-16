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
library(ggthemes)
library(zoo)
library(ggridges)

###################################
##
## Load Stop and Frisk Data
## Provided by MPD: https://mpdc.dc.gov/publication/stop-and-frisk-data-and-explanatory-notes
##
###################################

stop_frisk_1 <- read.xls("data/crime/SF_Field Contact_02202018.xls",sheet = 1)

stop_frisk_1$REASON.FOR.STOP <- "N/A"

stop_frisk_1$time_of_day <- format(strptime(stop_frisk_1$Report_taken_date_EST, "%m/%d/%Y %I:%M %p"), "%H")
stop_frisk_1$day_of_month <- format(strptime(stop_frisk_1$Report_taken_date_EST, "%m/%d/%Y %I:%M %p"), "%d")

stop_frisk_2 <- read.xls("data/crime/SF_Field Contact_02202018.xlsx",sheet = 2, quote = "")
stop_frisk_2 <- stop_frisk_2[!is.na(stop_frisk_2$Year),]

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

stop_frisk_2$time_of_day <- format(strptime(stop_frisk_2$Report_taken_date_EST, "%Y-%m-%d %H:%M"), "%H")
stop_frisk_2$day_of_month <- format(strptime(stop_frisk_2$Report_taken_date_EST, "%Y-%m-%d %H:%M"), "%d")

stop_frisk_total <- rbind(stop_frisk_1,stop_frisk_2)

stop_frisk_total$id <- seq.int(nrow(stop_frisk_total))

## when do most stop and frisks occur? -- hourly ts

## where do most stop and frisks occur? -- bar graph of neighborhood
## what are the most cited reasons for stop and frisk?
## Add in Police Service area to shiny map
## Stop & Frisk by time of day by neighborhood racial composition
## Stop & Frisk incidents by day of month -- are more people frisked at end of the month for quotas


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

stop_frisk_total$street_name <- gsub(pattern = "\\\\",
                                     replacement = "",
                                     x = stop_frisk_total$street_name)

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

stop_frisk_new <- rbind(combined,second_match,third_match)

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

stop_frisk_new$race_ethn <- ifelse(stop_frisk_new$Subject_Ethnicity=='Hispanic Or Latino','Hispanic/Latino',
                                   stop_frisk_new$Subject_Race)

stop_frisk_new$race_ethn <- as.character(stop_frisk_new$Subject_Race)
stop_frisk_new$race_ethn[stop_frisk_new$Subject_Ethnicity == "Hispanic Or Latino"] <- "Hispanic/Latino" 

stop_frisk_new$juvenile <- ifelse(stop_frisk_new$Age == "Juvenile","Juvenile","Adult")
stop_frisk_new$juvenile[stop_frisk_new$Age == "Unknown" | stop_frisk_new$Age == ""] <- "Unknown" 

## what are the historical patterns of stop and frisk? -- monthly ts

stop_frisk_total$month <- format(as.Date(stop_frisk_total$Report_taken_date_EST,"%m/%d/%Y"), "%m")
stop_frisk_total$year_month <- as.Date(paste0(stop_frisk_total$month,"/","01/",as.numeric(stop_frisk_total$Year),sep=""),"%m/%d/%Y")

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
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y")

ggplot(stop_frisk_monthly,aes(x=year_month,y=n,group=1)) + 
  geom_point(size=2) +
  geom_smooth(method = lm,size=2) +
  #stat_smooth(aes(x=year_month, y=n), method = lm, formula = y ~ poly(x, 10), se = TRUE,size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Number of Stop and Frisk') + xlab("Month") + ggtitle("Total Number of Stop and Frisk per Month") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y")

stop_frisk_monthly$month <- format(as.Date(stop_frisk_monthly$year_month,"%m/%d/%Y"), "%m")

stop_frisk_monthly_avg <- stop_frisk_monthly %>%
  group_by(month) %>%
  summarise(mean = mean(n))

ggplot(stop_frisk_monthly_avg,aes(x=month,y=mean)) + geom_bar(stat='identity',fill="dark blue") +
  # scale_x_discrete(limits = c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  xlab("") + ylab("Average Stop & frisk Incidents") + theme_fivethirtyeight() +
  geom_text(aes(label=round(stop_frisk_monthly_avg$mean,0)), 
            vjust=-0.5, position=position_dodge(.5), size=5) +
  labs(caption="DC OpenData Portal: http://opendata.dc.gov/datasets/crashes-in-dc")

## average age of stop and frisk by race 

stop_frisk_new$age_numeric <- as.numeric(ifelse(stop_frisk_new$Age == "Juvenile",16,
                                                  as.numeric(as.character(stop_frisk_new$Age))))

sf_age_race <- stop_frisk_new %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino","Asian")) %>%
  group_by(race_ethn) %>%
  summarise(mean = mean(age_numeric, na.rm = TRUE))

ggplot(filter(stop_frisk_total, race_ethn %in% c("White","Black","Hispanic/Latino","Asian")), 
       aes(x = age_numeric, y = race_ethn)) + geom_density_ridges() +
  xlab("Subject Age") + ylab("Subject Race") +
  theme_fivethirtyeight() 

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

coordinates(stop_frisk_new) <- ~ LONGITUDE + LATITUDE

neighborhoods <- levels(dc_neighborhoods$NBH_NAMES)

nbh_sf_df <- data.frame()

for (n in neighborhoods) {
  
  print(paste("Classifying stop and frisk incidents in",n))
  
  test <- data.frame()
  
  cluster <- dc_neighborhoods[dc_neighborhoods$NBH_NAMES == n , ]

  proj4string(stop_frisk_new) <- proj4string(cluster)
  
  test <- stop_frisk_new[complete.cases(over(stop_frisk_new, cluster)), ]
  test_df <- as.data.frame(test)
  try(test_df$neighborhood <- n)
  
  nbh_sf_df <- rbind(nbh_sf_df,test_df)

}

## census tracts

dc_census_tracts <- readOGR("data/shapefiles",
                            layer="Census_Tracts_in_2010")

coordinates(stop_frisk_new) <- ~ LONGITUDE + LATITUDE

tracts <- levels(dc_census_tracts$TRACT)

tracts_sf_df <- data.frame()

for (t in tracts) {
  
  print(paste("Classifying stop and frisk incidents in",t))
  
  test <- data.frame()
  
  tract <- dc_census_tracts[dc_census_tracts$TRACT == t , ]
  
  proj4string(stop_frisk_new) <- proj4string(tract)
  
  test <- stop_frisk_new[complete.cases(over(stop_frisk_new, tract)), ]
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
## Plot stop and frisk and census
##
###################################

## total sf (race stacked) by neighborhood whiteness decile

stop_frisk_dom <- nbh_sf_df %>%  
  group_by(day_of_month) %>%
  summarise (n = n())

ggplot(stop_frisk_dom,aes(x=day_of_month,y=n,group=1)) + 
  geom_line(size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Number of Stop and Frisk') + xlab("Day of Month") + ggtitle("Total Number of Stop and Frisk by Day of Month") 

nbh_sf_df <- merge(nbh_sf_df,additional_cluster_info,by.x="neighborhood",by.y="NBH_NAMES")

nbh_sf_df_census <- merge(nbh_sf_df,census_data_original,by.x=c("NAME"),by.y=c("CLUSTER_TR2000"))

nbh_sf_df_census$pct_non_white <- nbh_sf_df_census$PctAsianPINonHispBridge_2010 + 
                                  nbh_sf_df_census$PctBlackNonHispBridge_2010 +
                                  nbh_sf_df_census$PctHisp_2010

nbh_sf_df_census$non_white_bins <-cut(nbh_sf_df_census$pct_non_white, c(0,10,20,30,40,50,60,70,80,90,100))

## total stop and frisk by race & neighborhood "whiteness"

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
          
## scatter plot of neighborhood racial composition and percent of stop and frisk by race

nbh_sf_demos_census$diffcats <- cut(nbh_sf_demos_census$diff,4)

nbh_sf_race <- ggplot(data=filter(nbh_sf_demos_census,demo_group %in% c("Race/Ethnicity")),aes(x=freq,y=census_value)) + 
  geom_point(aes(size=n,color=subgroup)) + scale_x_continuous(limits = c(0, 1),labels = scales::percent) + 
  scale_y_continuous(limits = c(0, 100)) + geom_abline(intercept = 0,slope = 100) + 
  geom_hline(yintercept = 50) + geom_vline(xintercept = .5) +
  theme_fivethirtyeight() +
  labs(x = "Percent of Stop and Frisk", y = "Percent of Neighborhood Residents") +
  ggtitle("Neighborhood Population v. Stop & Frisk") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text()) 

ggsave(plot = nbh_sf_race, "03_stop_frisk/images/nbh_sf_race.png", w = 10.67, h = 10.67,type = "cairo-png")

## difference of neighborhood racial composition and percent of stop and frisk by race

nbh_sf_demos_census$diff <-  (nbh_sf_demos_census$census_value/100) - nbh_sf_demos_census$freq

ggplot(data=filter(nbh_sf_demos_census,subgroup %in% c("Black") & demo_group != "Total"), 
       aes(x = reorder(neighborhood, diff), 
           y = as.numeric(diff))) + 
  #geom_bar(stat = "identity",show.legend=FALSE) + 
  geom_point(aes(size=n,color=diff),stat='identity') + coord_flip() +
  ggtitle("Neighborhood Difference in Stop & Frisk Rate & Population \n among Black Residents (2012 - 2017)") + 
  geom_hline(yintercept = 0) +
  #geom_text(data=filter(nbh_sf_demos_census,subgroup %in% c("Black")),aes(label=nbh_sf_demos_census$n), vjust=0,hjust=-0.1, position=position_dodge(.5), size=3,) + 
  theme_fivethirtyeight() +
  labs(x = "Neighborhood", y = "Stop & Frisk - Population") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size=10)) + guides(color=FALSE) +
  scale_color_gradient(low = "red", high = "green", limits=c(min(nbh_sf_demos_census$diff),max(nbh_sf_demos_census$diff))) +
  scale_y_continuous(labels=scales::percent) +
  geom_segment(aes(y = 0, 
                 x = neighborhood, 
                 yend = diff, 
                 xend = neighborhood), 
                 color = "black")

ggsave(plot = plot_xx_sf_pop_diff, "images/pop_diff.png", w = 10.67, h = 8,type = "cairo-png")

###################################
##
## Tying in crime data
##
###################################

years = c(2012,2013,2014,2015,2016)

crime_all_years = data.frame()

for (y in years) {
  
  crime <- readOGR(paste("data/shapefiles/Crime_Incidents_in_",y,".shp",sep = ""),
                              layer=paste("Crime_Incidents_in_",y,"",sep = ""))
  
  neighborhoods <- levels(dc_neighborhoods$NBH_NAMES)
  
  new_df <- data.frame()
  
  for (n in neighborhoods) {
    
    print(paste("Classifying arrests incidents in",n))
    
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
    summarise(arrests = n()) %>%
    mutate(arrest_pct =arrests/sum(arrests))
  
  crime_neighborhood$year <- y
  
  crime_all_years <- rbind(crime_all_years,crime_neighborhood)

}

crime_avg_years <- crime_all_years %>%
  group_by(neighborhood) %>%
  summarise(avg_arrests = mean(arrests))

nbh_sf_yearly <- nbh_sf_df %>%
  group_by(Year,neighborhood) %>%
  summarise (stop_frisks = n()) 

nbh_sf_yearly$prev_year <- as.numeric(as.character(nbh_sf_yearly$Year)) - 1

nbh_sf_yearly <- merge(nbh_sf_yearly,crime_all_years,
                        by.x = c("neighborhood","prev_year"),
                        by.y = c("neighborhood","year"))

## relationship between previous year in crime and current year stop and frisk

crime_frisk_yearly <- ggplot(data=nbh_sf_yearly,aes(x=stop_frisks,y=arrests)) + 
  geom_point() + 
  geom_smooth(method='glm',formula=y~x) +
  theme_fivethirtyeight() +
  labs(x = "Total Stop and Frisk", y = "Total Crime Reported") +
  ggtitle("Previous Year Crime Incidents v. Current Year Stop & Frisk") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

ggsave(plot = crime_frisk_yearly, "03_stop_frisk/images/crime_frisks.png", w = 10.67, h = 8,type = "cairo-png")

ggplot(data=filter(nbh_sf_yearly,neighborhood %in% c("Columbia Heights, Mt. Pleasant, Pleasant Plains, Park View"))) + 
  geom_line(aes(x=Year,y=stop_frisks,group=1)) +
  geom_line(aes(x=Year,y=arrests/5,group=1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Arrests")) +
  theme_fivethirtyeight() +
  scale_colour_manual(values = c("blue", "red")) +
  labs(x = "Year", y = "Stop & Frisk", colour = "Legend") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

ggplot(data=nbh_sf_yearly) + 
  geom_line(aes(x=Year,y=stop_frisks,group=neighborhood)) +
  theme_fivethirtyeight() +
  labs(x = "Year", y = "Stop & Frisk", colour = "Legend") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

## total stop and frisk counts by neighborhood race from census

total_sf_nbh_race <- nbh_sf_df_census %>%
  group_by(Year,non_white_bins) %>%
  summarise (sf = n())

avg_sf_nbh_race <- total_sf_nbh_race %>%
  group_by(non_white_bins) %>%
  summarise (avg_sf = mean(sf))

crime_w_census <- merge(crime_all_years,additional_cluster_info,by.y="NBH_NAMES",by.x="neighborhood")
crime_w_census <- merge(crime_w_census,census_data_original,by.x="NAME",by.y="CLUSTER_TR2000")

crime_w_census <- crime_w_census %>%
  select(NAME,neighborhood,PctBlackNonHispBridge_2010,PctHisp_2010,
         PctAsianPINonHispBridge_2010,TotPop_2010)

crime_w_census$pct_non_white <- crime_w_census$PctAsianPINonHispBridge_2010 + 
  crime_w_census$PctBlackNonHispBridge_2010 +
  crime_w_census$PctHisp_2010

crime_w_census$non_white_bins <-cut(crime_w_census$pct_non_white, c(0,10,20,30,40,50,60,70,80,90,100))

arrests_w_census <- crime_w_census %>%
  group_by(non_white_bins) %>%
  summarise (arrests = mean(arrests),pop=mean(TotPop_2010))

arrests_sf_race_bins <- merge(arrests_w_census,avg_sf_nbh_race,by="non_white_bins")

arrests_sf_race_bins$adj_sf <- (arrests_sf_race_bins$avg_sf / arrests_sf_race_bins$pop) * 100
arrests_sf_race_bins$adj_arrest <- (arrests_sf_race_bins$arrests / arrests_sf_race_bins$pop) * 100

arrests_sf_race_l <- melt(arrests_sf_race_bins, id.vars=c("non_white_bins")) %>%
  subset(variable %in% c("adj_sf","adj_arrest"))

ggplot(arrests_sf_race_l,aes(x=as.numeric(non_white_bins)*10,y=value,color=variable,group=as.character(variable))) + 
  geom_line(size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Arrests/Stop & Frisk per 100 people') + xlab("Neighborhood Percent of Residents of Color") + 
  ggtitle("Average Yearly Stop & Frisk/Arrests by \nNeighborhood Percentage of Residents of Color") 

## calculating average time of "nearest" crime

library(fuzzyjoin)

sf_2017 <- stop_frisk_total %>%
  filter(as.numeric(as.character(Year)) >= 2016)


combined <- merge(sf_2017,grocery,by=NULL) 

## calculating distances from each house to each metro stop

combined$distance <- distHaversine(combined[, 46:47],  combined[, 1:2]) / 1609
combined$distance <- combined$distance * 5280 #convert to feet

combined <- arrange(combined,FULLBLOCK,desc(distance))
closest_store <- combined %>% group_by(FULLBLOCK) %>% arrange(distance) %>% slice(1)

