## Author: August Warren
## Description: Analysis of DC Stop and Frisk Data
## Date: 8/18/2018
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
library(stargazer)
library(viridis)
source("03_stop_frisk/scripts/mygg_animate.r")

###################################
##
## Load Stop and Frisk Data
## Provided by MPD: https://mpdc.dc.gov/publication/stop-and-frisk-data-and-explanatory-notes
##
###################################

stop_frisk_1 <- read.xls("data/crime/SF_Field Contact_02202018.xls",sheet = 1)
stop_frisk_2017 <- read.xls("data/crime/SF_Field Contact_CY2017.xls",sheet = 1)

stop_frisk_1 <- rbind(stop_frisk_1,stop_frisk_2017)

stop_frisk_1$REASON.FOR.STOP <- "N/A"

stop_frisk_1$time_of_day <- format(round(strptime(stop_frisk_1$Report_taken_date_EST, "%m/%d/%Y %I:%M %p"), units="hours"), format="%H:%M")
stop_frisk_1$day_of_month <- format(strptime(stop_frisk_1$Report_taken_date_EST, "%m/%d/%Y %I:%M %p"), "%d")
stop_frisk_1$month <- format(as.Date(stop_frisk_1$Report_taken_date_EST,"%m/%d/%Y"), "%m")
stop_frisk_1$year_month <- as.Date(paste0(stop_frisk_1$month,"/","01/",as.numeric(stop_frisk_1$Year),sep=""),"%m/%d/%Y")

## add in 2017 data

stop_frisk_2 <- read.xls("data/crime/SF_Field Contact_02202018.xlsx",sheet = 2, quote = "")
stop_frisk_2017 <- read.xls("data/crime/SF_Field Contact_CY2017.xls",sheet = 2, quote = "")

## add 2017 data

stop_frisk_2 <- rbind(stop_frisk_2,stop_frisk_2017)

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

stop_frisk_total <- rbind(stop_frisk_1)

stop_frisk_total$id <- seq.int(nrow(stop_frisk_total))

###################################
##
## Matching incident block address in stop & frisk file to DC block data 
## in order to obtain incident lattitude and longitude
## h/t to Mahkah Wu for the idea and doing the hard: https://github.com/mahkah/dc_stop_and_frisk
##
###################################

stop_frisk_matched <- read.csv(url("https://raw.githubusercontent.com/mahkah/dc_stop_and_frisk/master/transformed_data/SF_Field%20Contact_locations.csv"))

stop_frisk_matched <- stop_frisk_matched %>%
  filter(block_match == "Matched" & cause == "Unknown") %>%
  rename(Age = subject_age,
          Subject_Ethnicity = subject_ethnicity,
          Subject_Gender = subject_gender,
          Subject_Race = subject_race)

stop_frisk_matched$time_of_day <- format(round(strptime(stop_frisk_matched$incident_date, "%Y-%m-%d %H:%M"), units="hours"), format="%H:%M")
stop_frisk_matched$day_of_month <- format(strptime(stop_frisk_matched$incident_date, "%Y-%m-%d %H:%M"), "%d")
stop_frisk_matched$month <- format(as.Date(stop_frisk_matched$incident_date,"%Y-%m-%d %H:%M"), "%m")
stop_frisk_matched$year_month <- as.Date(paste0(as.numeric(stop_frisk_matched$month),"/","01/",as.numeric(stop_frisk_matched$year),sep=""),"%m/%d/%Y")

###################################
##
## Top-level descriptives
##
###################################

## add in non-forcible for analysis

stop_frisk_all_stops <- rbind(stop_frisk_1,stop_frisk_2)

## standard funciton to add demos to all forcible, matched lat/long forcible, forcible & non-forcible stops

add_demos <- function(data_frame){
 
  data_frame$race_ethn <- ifelse(data_frame$Subject_Ethnicity=='Hispanic Or Latino','Hispanic/Latino',
                                 data_frame$Subject_Race)
  
  data_frame$race_ethn <- as.character(data_frame$Subject_Race)
  data_frame$race_ethn[data_frame$Subject_Ethnicity == "Hispanic Or Latino"] <- "Hispanic/Latino" 
  
  data_frame$juvenile <- ifelse(data_frame$Age == "Juvenile","Juvenile","Adult")
  data_frame$juvenile[data_frame$Age == "Unknown" | data_frame$Age == ""] <- "Unknown" 
  
  return(data_frame)
   
}

stop_frisk_total <- add_demos(stop_frisk_total) ## all forcible stop & frisks
stop_frisk_matched <- add_demos(stop_frisk_matched) ## forcible stops mapped to lat/long
stop_frisk_all_stops <- add_demos(stop_frisk_all_stops) ## both forcible & non-forcible stops

## what are the historical patterns of stop and frisk? -- monthly ts

stop_frisk_monthly <- stop_frisk_total %>%
  group_by(year_month) %>%
  summarise (n = n()) %>%
  arrange(year_month) %>%
  mutate(monthly = rollsum(n, k = 12, na.pad = TRUE, align = "right")) 

monthly_sf <- ggplot(stop_frisk_monthly,aes(x=year_month,y=n,group=1)) + 
  geom_point(size=2) +
  geom_smooth(method = lm,size=2) +
  #geom_vline(aes(xintercept = as.numeric(as.Date(dmy("2/1/2015")))), col = "black") +
  #stat_smooth(aes(x=year_month, y=n), method = lm, formula = y ~ poly(x, 10), se = TRUE,size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Number of Stop and Frisks') + xlab("Month") + ggtitle("Total Number of Stop and Frisks per Month") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%y")

ggsave(plot = monthly_sf, "03_stop_frisk/images/01_monthly_sf.png", w = 10.67, h = 8,type = "cairo-png")

## hourly ts by race percentage

stop_frisk_hourly <- stop_frisk_total %>%
  filter(!is.na(stop_frisk_total$time_of_day)) %>%
  group_by(time_of_day) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n))

stop_frisk_total$hour <- format(round(strptime(stop_frisk_total$Report_taken_date_EST, "%m/%d/%Y %H:%M"),units="hours"), "%H")
stop_frisk_total$mins <- format(round(strptime(stop_frisk_total$Report_taken_date_EST, "%m/%d/%Y %H:%M"),units="mins"), "%M")

stop_frisk_total$mins_past_midnight <- (as.numeric(stop_frisk_total$hour) * 60) + as.numeric(stop_frisk_total$mins)

ggplot(data = filter(stop_frisk_total,race_ethn %in% c("White","Black","Hispanic/Latino")),
       aes(mins_past_midnight,group=race_ethn,fill=race_ethn)) + geom_density(alpha = 0.5)

stop_frisk_hourly$race_ethn <- "Total"

stop_frisk_hourly <- stop_frisk_hourly[c("race_ethn", "time_of_day", "n","freq")]

stop_frisk_hourly_race <- stop_frisk_total %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino")) %>%
  group_by(race_ethn,time_of_day) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n)) 

stop_frisk_hourly_comb <- rbind(as.data.frame(stop_frisk_hourly_race),as.data.frame(stop_frisk_hourly))

stop_frisk_hourly_comb$time_of_day <- factor(stop_frisk_hourly_comb$time_of_day,levels=c("07:00","08:00","09:00","10:00","11:00","12:00","13:00",
                                                                                         "14:00","15:00","16:00","17:00","18:00","19:00","20:00",
                                                                                         "21:00","22:00","23:00","00:00","01:00","02:00","03:00",
                                                                                         "04:00","05:00","06:00"))

stop_frisk_hourly_comb_plot <- ggplot(stop_frisk_hourly_comb, aes(x=time_of_day, y=freq, color=race_ethn,group=race_ethn)) + 
  geom_line(size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Percentage of Stop and Frisk Incidents') + xlab("Time of Day") + 
  ggtitle("Total Stop and Frisk Incidents by Time of Day & Race/Ethnicity") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_discrete(name="Legend")

ggsave(plot = stop_frisk_hourly_comb_plot, "03_stop_frisk/images/02_time_of_day_sf.png", w = 10.67, h = 8,type = "cairo-png")

## percent of juvenile stops by race 

stop_frisk_total$juvenile <- ifelse(stop_frisk_total$Age == "Juvenile","Juveniles",
                                                  "Adults")
sf_age_race <- stop_frisk_total %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino")) %>%
  group_by(race_ethn,juvenile,Subject_Sex) %>%
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
                  theme(axis.title = element_text(),
                        plot.title = element_text(hjust = 0.5),
                        axis.text = element_text(size=12),
                        strip.text = element_text(size=16)) + 
                  labs(title = "Proportion of Juvenile vs. Adult Stops by Race/Ethnicity",
                       x = '',
                       y ="",
                       fill="Legend")

ggsave(plot = sf_race_youths, "03_stop_frisk/images/sf_race_youths.png", w = 10.67, h = 8,type = "cairo-png")

## average age of stop and frisk by race 

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

white <- sf_ages %>%
  filter(race_ethn=="White") ##23

black <- sf_ages %>%
  filter(race_ethn=="Black") #18

hisp <- sf_ages %>%
  filter(race_ethn=="Hispanic/Latino") #18


sf_age_dist$race_ethn <- factor(sf_age_dist$race_ethn,levels=c("White","Black","Hispanic/Latino"))

sf_age_dist <- ggplot(filter(sf_ages, race_ethn %in% c("White","Black","Hispanic/Latino")), 
       aes(x = Age, y = race_ethn)) + geom_density_ridges() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  labs(x="Age",
       y="",
       title="Age of Stop andd Frisk Incidents by Race/Ethnicty among Adults")

ggsave(plot = sf_age_dist, "03_stop_frisk/images/04_sf_age_dist.png", w = 10.67, h = 8,type = "cairo-png")

## what are the most cited reasons for non-forcible stops?

stop_frisk_all_stops$reason <- ifelse(grepl("Suspicious",stop_frisk_all_stops$REASON.FOR.STOP),
                                "Suspicious Vehicles/Persons/Activities",stop_frisk_all_stops$REASON.FOR.STOP)

reasons_for_stop <- stop_frisk_all_stops %>%
  filter(!reason %in% c("N/A") & race_ethn %in% c("White","Black","Hispanic/Latino","Asian")) %>%
  group_by(race_ethn,reason) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

reasons_for_stop <- reasons_for_stop %>%
  filter(race_ethn %in% c("White", "Black", "Hispanic/Latino"))

reasons_for_stop$race_ethn <- factor(reasons_for_stop$race_ethn, levels = c("White", "Black", "Hispanic/Latino"))

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

## race breakdown by forcible/non-forcible stops 

stop_frisk_all_stops$contact_type <- ifelse(stop_frisk_all_stops$REASON.FOR.STOP=="N/A","Forcible","Non-forcible")

race_contact <- stop_frisk_all_stops %>%
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

## gender/race breakdown by forcible/non-forcible

gender_race <- stop_frisk_all_stops %>%
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
  xlab("") + ylab("") + ggtitle("Gender by Race/Ethnicity & Contact Type")+
  scale_fill_discrete(name="Legend",limits=c("Male","Female")) 

ggsave(plot = gender_race_plot, "03_stop_frisk/images/gender_race_contact.png", w = 10.67, h = 8,type = "cairo-png")

###################################
##
## Matching incidents to neighborhoods using DC neighborhood shapefile 
## provided by DC OpenData
## h/t: https://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile
##
###################################

## neighborhoods 

dc_neighborhoods <- readOGR("data/shapefiles",
                            layer="Neighborhood_Clusters")

coordinates(stop_frisk_matched) <- ~ X + Y

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

###################################
##
## Calculate race/age breakdowns of stop and frisk at neighborhood level
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

nbh_racial_summary <- census_data_original %>%
  select(CLUSTER_TR2000,PctBlackNonHispBridge_2010,PctHisp_2010,PctAsianPINonHispBridge_2010) 

nbh_racial_summary$pct_non_white <- nbh_racial_summary$PctBlackNonHispBridge_2010 +
  nbh_racial_summary$PctHisp_2010 +
  nbh_racial_summary$PctAsianPINonHispBridge_2010

nbh_racial_summary$non_white_bins <- cut(nbh_racial_summary$PctBlackNonHispBridge_2010, c(0,25,50,75,95,100))

nbh_racial_summary <- merge(nbh_racial_summary,additional_cluster_info,by.x="CLUSTER_TR2000",by.y="NAME")

levels(nbh_racial_summary$non_white_bins) <- c("0-25%","25-50%","50-75%","75-95%","95-100%")

nbh_black_summary <- ggplot(nbh_racial_summary,aes(x=reorder(NBH_NAMES,PctBlackNonHispBridge_2010),y=PctBlackNonHispBridge_2010/100,fill=non_white_bins)) + 
  geom_bar(stat='identity') +
  geom_text(aes(x=reorder(NBH_NAMES,PctBlackNonHispBridge_2010),y=PctBlackNonHispBridge_2010/100,
                label=percent(round(PctBlackNonHispBridge_2010/100,2))),data=nbh_racial_summary, 
            position=position_dodge(width=0.9), hjust=-0.1,size=3) +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(title = "DC Neighborhood Racial Composition", subtitle="Proportion of Black Residents by Neighborhood as of 2010 Census ", 
       x="",y="Neighborhood Percent of Black Residents",
       fill = "Neighborhood % Black") +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,size = 10)) + 
  scale_y_continuous(labels=scales::percent) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggsave(plot = nbh_black_summary, "03_stop_frisk/images/nbh_racial_profiles.png", w = 10.67, h = 8,type = "cairo-png")

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

overall_change <- stop_frisk_total %>%
  group_by(Year) %>%
  summarise(count = n())

overall_change$Year <- paste0("year_",overall_change$Year)
overall_change$ix <- "Total"
overall_change <- dcast(overall_change, ix ~ Year,value.var = "count")
overall_change$change_2017_2010 <- (overall_change$year_2017 / overall_change$year_2010) - 1

## percent change in stop & frisk by nh racial composition 2010 - 2017

yearly_nbh_counts <- nbh_sf_df %>%
  group_by(year,neighborhood) %>%
  summarise(n=n())

yearly_nbh_counts$year <- paste0("year_",yearly_nbh_counts$year)

# reshape to wide

yearly_nbh_counts_wide <- dcast(yearly_nbh_counts, neighborhood ~ year)

# merge in census data

yearly_nbh_counts_wide <- merge(yearly_nbh_counts_wide,additional_cluster_info,by.x="neighborhood",by.y="NBH_NAMES")
yearly_nbh_counts_wide <- merge(yearly_nbh_counts_wide,census_data_original,by.x=c("NAME"),by.y=c("CLUSTER_TR2000"))

yearly_nbh_counts_wide$pct_non_white <- yearly_nbh_counts_wide$PctAsianPINonHispBridge_2010 + 
  yearly_nbh_counts_wide$PctBlackNonHispBridge_2010 +
  yearly_nbh_counts_wide$PctHisp_2010

yearly_nbh_counts_wide$black_bins <-cut(yearly_nbh_counts_wide$PctBlackNonHispBridge_2010, c(0,25,50,75,95,100))

yearly_nbh_counts_wide$change_2017_2010 <- (yearly_nbh_counts_wide$year_2017 / yearly_nbh_counts_wide$year_2010) - 1

change_by_racial_bins <- yearly_nbh_counts_wide %>%
  group_by(black_bins) %>%
  summarise(avg_change = mean(change_2017_2010),
            n= n())

levels(change_by_racial_bins$black_bins) <- c("0-25%","25-50%","50-75%","75-95%","95-100%")

change_in_sf <- ggplot(change_by_racial_bins,aes(x=black_bins,y=avg_change)) + 
  geom_bar(stat='identity') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Change in Total Stop and Frisk 2010 to 2017') + xlab("Neighborhood Percent of Black Residents") + 
  ggtitle("Average Change in Stop and Frisk 2010 to 2017 by Neighborhood Racial Composition") + 
  geom_text(aes(x=black_bins,y=avg_change,label=percent(round(avg_change,2))),data=change_by_racial_bins, 
            position=position_dodge(width=0.9), vjust=-0.5,size=5) +
  scale_y_continuous(labels=scales::percent)
  
ggsave(plot = change_in_sf, "03_stop_frisk/images/change_in_sf.png", w = 10.67, h = 8,type = "cairo-png")

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
  labs(x="Neighborhood",y="Total Stop & Frisk",title="Total Stop and Frisk",fill="Total Stop & Frisk")

mygg_animate(p,filename = "03_stop_frisk/images/05_nbh_sf.gif",interval = 0.1, title_frame=T,ani.height=768,ani.width=1024)

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

census_sf_race$type <- factor(census_sf_race$type, levels=c("Census","Stop & Frisk"))

census_sf_race_plot <- ggplot(census_sf_race,aes(x=variable,y=as.numeric(value),fill=variable)) + 
  geom_bar(stat = "identity",position = "stack") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size=12)) + 
  ylab("") + xlab("Race") + ggtitle("Stop and Frisk - Census Racial Comparison") + 
  scale_x_discrete(limits = c("White","Black","Hispanic/Latino","Asian")) +
  scale_y_continuous(labels=scales::percent,limits=c(0,1)) +
  geom_text(aes(x=variable,y=value,label=percent(round(value,2))),data=census_sf_race, 
            position=position_dodge(width=0.9), vjust=-0.5,size=5) +
  scale_fill_discrete(name="Legend") +
  facet_wrap(~type)

ggsave(plot = census_sf_race_plot, "03_stop_frisk/images/03_census_sf_race.png", w = 10.67, h = 8,type = "cairo-png")

## scatter plot of neighborhood racial composition and percent of stop and frisk by race

## label petworth?

nbh_sf_race_plot <- ggplot(data=filter(nbh_sf_demos_census,demo_group %in% c("Race/Ethnicity")),aes(x=freq,y=census_value)) + 
  geom_point(aes(size=n,color=subgroup),alpha=.7) + scale_x_continuous(limits = c(0, 1),labels = scales::percent) + 
  geom_point(data=subset(nbh_sf_demos_census,neighborhood == "Columbia Heights, Mt. Pleasant, Pleasant Plains, Park View" &
                           demo_group %in% c("Race/Ethnicity")),
             aes(size=n),colour="black",pch=21) +
  geom_text(data=subset(nbh_sf_demos_census,neighborhood == "Columbia Heights, Mt. Pleasant, Pleasant Plains, Park View" &
                          demo_group %in% c("Race/Ethnicity")),
            aes(label=paste0("Columbia Heights\n(",subgroup,")")),
            hjust="center", vjust = -.5, size=4) +
  scale_y_continuous(limits = c(0, 100)) + geom_abline(intercept = 0,slope = 100) + 
  geom_hline(yintercept = 50) + geom_vline(xintercept = .5) +
  theme_fivethirtyeight() +
  labs(x = "Stop and Frisk Racial Makeup (%)", y = "Neighborhood Racial Makeup (%)") +
  ggtitle("Neighborhood Population vs. Neighborhood Stop and Frisk") +
  scale_color_discrete(name="Race") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text()) +
  scale_size(name   = "Total Stop and Frisk") + theme(legend.position="bottom", legend.box = "horizontal")

ggsave(plot = nbh_sf_race_plot, "03_stop_frisk/images/06_nbh_sf_race.png", w = 10.67, h = 10.67,type = "cairo-png")


nbh_sf_demos_census$diff <-  (nbh_sf_demos_census$census_value/100) - nbh_sf_demos_census$freq

## difference of neighborhood racial composition and percent of stop and frisk by race
plot_diff <- function(racial_group,output_file) {
  
  race_pct <- nbh_sf_demos_census %>%
    filter(subgroup == racial_group) %>%
    select(NAME,neighborhood,census_value) %>%
    rename(pct = census_value) %>%
    mutate(pct = pct/100)
  
  nbh_sf_demos_census <- merge(nbh_sf_demos_census,race_pct)
  
  nbh_diff_race <- ggplot(data=filter(nbh_sf_demos_census,subgroup %in% c(racial_group) & demo_group != "Total"), 
                          aes(x = reorder(neighborhood, -diff), 
                              y = as.numeric(diff))) + 
    geom_point(aes(size=n,color=pct),alpha=.7,stat='identity') + coord_flip() +
    geom_hline(yintercept = 0) +
    theme_fivethirtyeight() +
    labs(title = "Neighborhood Difference in Stop & Frisk Rate & Population",
         subtitle = paste("among", racial_group," Residents (2010 - 2017)"), 
         y = 'Stop & Frisk - Population',
         x="Neighborhood",size="Total Stop & Frisk",color=paste("Neighborhood %",racial_group)) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5,size=12),
          text = element_text(size=10))  +
    scale_y_continuous(labels=scales::percent) +
    scale_color_viridis(labels = percent) +
    geom_segment(aes(y = 0, 
                     x = neighborhood, 
                     yend = diff, 
                     xend = neighborhood), 
                 color = "black")
  
  ggsave(plot = nbh_diff_race, paste0("03_stop_frisk/images/",output_file,".png"), w = 10.67, h = 8,type = "cairo-png")
  
}

plot_diff("Black","07_nbh_diff_black")
plot_diff("White","07a_nbh_diff_white")
plot_diff("Hispanic/Latino","07b_nbh_diff_hisp")

## stop & frisk & census pop among blacks by neighborhood white percentage

nbh_white <- nbh_sf_demos_census %>%
  filter(subgroup %in% c("White") & demo_group != "Total") %>%
  select(neighborhood,census_value) %>%
  rename(nbh_white_pct = census_value)

new_df <- nbh_sf_demos_census %>%
  filter(subgroup %in% c("Black") & demo_group != "Total") %>%
  mutate(adj_census = census_value/100) %>%
  select(neighborhood,adj_census,freq) %>%
  rename(nbh_black_pct = adj_census,
         black_sf = freq)

new_df_l <- melt(new_df)

num_stops <- nbh_sf_demos_census %>%
  filter(subgroup %in% c("Black") & demo_group != "Total") %>%
  select(neighborhood,n) 

new_df_l <- merge(new_df_l,nbh_white,by="neighborhood")
new_df_l <- merge(new_df_l,num_stops,by="neighborhood")

new_df <- merge(new_df,nbh_white,by="neighborhood")

new_df$indicator <- ifelse(new_df$black_sf > new_df$nbh_black_pct,"Higher","Lower")

group.colors <- c("Higher" = "#FF0000", "Lower" = "#008000", "Black Stop & Frisk Percentage" = cols[1], "Black Census Percentage" = cols[3])

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

n = 4
cols = gg_color_hue(n)

levels(new_df_l$variable) <- c("Black Census Percentage","Black Stop & Frisk Percentage")

test <- ggplot(data=new_df_l, 
       aes(x = as.numeric(nbh_white_pct/100), 
           y = as.numeric(value)),
       color = variable) + 
  geom_point(aes(size=n,color=variable),alpha=.5,stat='identity') + 
  coord_flip() +
  geom_segment(data = new_df, aes(y = nbh_black_pct, 
                   x = as.numeric(nbh_white_pct/100), 
                   yend = black_sf, 
                   xend = as.numeric(nbh_white_pct/100),
                   color=indicator),
               size = 1,
               alpha = .2) +
  scale_color_manual(values = group.colors) +
  theme_fivethirtyeight() +
  labs(title = "Neighborhood Difference in Stop & Frisk Rate & Population",
       subtitle = "among Black Residents (2012 - 2017)", 
       y = 'Neighborhood Black Stop & Frisk & Population',
       x="Neighborhood White Percentage",size="Total Stop & Frisk") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text()) +
  scale_x_continuous(limits = c(0, .85),labels = scales::percent) + 
  scale_y_continuous(limits = c(0, 1),labels = scales::percent) 

ggsave(plot = test, "03_stop_frisk/images/test.png", w = 10.67, h = 8,type = "cairo-png")

###################################
##
## Tying in crime data
##
###################################

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
    group_by(neighborhood,Year) %>%
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
  group_by(year,neighborhood) %>%
  summarise (stop_frisks = n()) 

nbh_sf_yearly$prev_year <- as.numeric(as.character(nbh_sf_yearly$year)) - 1

nbh_sf_yearly <- merge(nbh_sf_yearly,crime_all_years,
                        by.x = c("neighborhood","prev_year"),
                        by.y = c("neighborhood","year"))

## relationship between previous year in crime and current year stop and frisk

crime_frisk_yearly <- ggplot(data=nbh_sf_yearly,aes(x=crime,y=stop_frisks)) + 
  geom_point() +
  geom_point(data=subset(nbh_sf_yearly,neighborhood == "Ivy City, Arboretum, Trinidad, Carver Langston" &
                          year == "2014"),
            colour="red") +
  geom_smooth(method='glm',formula=y~x) +
  geom_text(data=subset(nbh_sf_yearly,neighborhood == "Ivy City, Arboretum, Trinidad, Carver Langston" &
                               year == "2014"),
            aes(label=paste0("Ivy City (",year,")")),
            hjust=1.1, size=4,colour="black") +
  theme_fivethirtyeight() +
  labs(y = "Number Stop and Frisks in Subsequent Year", x = "Number of Crimes Reported in Year") +
  ggtitle("Crime Incidents vs. Stop and Frisk Incidents in Subsequent Year\nby Neighborhood") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

ggsave(plot = crime_frisk_yearly, "03_stop_frisk/images/08_crime_frisks.png", w = 10.67, h = 8,type = "cairo-png")

## crime-only modelling

nbh_sf_avg <- nbh_sf_yearly %>%
  group_by(neighborhood) %>%
  summarise(avg_sf = mean(stop_frisks),
            avg_prev_yr_crime = mean(crime))

nbh_sf_avg <- merge(nbh_sf_avg,additional_cluster_info,by.y="NBH_NAMES",by.x="neighborhood")
nbh_sf_avg <- merge(nbh_sf_avg,census_data_original,by.x="NAME",by.y="CLUSTER_TR2000")

nbh_sf_avg$pct_non_white <- nbh_sf_avg$PctAsianPINonHispBridge_2010 + 
  nbh_sf_avg$PctBlackNonHispBridge_2010 +
  nbh_sf_avg$PctHisp_2010

nbh_sf_avg$non_white_bins <-cut(nbh_sf_avg$pct_non_white, c(0,10,20,30,40,50,60,70,80,90,100))
nbh_sf_avg$black_bins <- cut(nbh_sf_avg$PctBlackNonHispBridge_2010, c(0,25,50,75,95,100))

yearly_model <- lm(formula = avg_sf ~ avg_prev_yr_crime,
   data = nbh_sf_avg)

stargazer(yearly_model,align = TRUE, out="03_stop_frisk/images/models.htm")

## crime & race model

nbh_sf_yearly <- merge(nbh_sf_yearly,additional_cluster_info,by.y="NBH_NAMES",by.x="neighborhood")
nbh_sf_yearly <- merge(nbh_sf_yearly,census_data_original,by.x="NAME",by.y="CLUSTER_TR2000")

nbh_sf_yearly$pctnonwhite <- nbh_sf_yearly$PctAsianPINonHispBridge_2010 + 
  nbh_sf_yearly$PctBlackNonHispBridge_2010 +
  nbh_sf_yearly$PctHisp_2010

nbh_sf_avg$coll_bins <-cut(nbh_sf_avg$pct_non_white, c(0,10,40,60,90,100))
nbh_sf_avg$black_bins <- cut(nbh_sf_avg$PctBlackNonHispBridge_2010, c(0,25,50,75,95,100))

yearly_model_w_race <- lm(formula = avg_sf ~ avg_prev_yr_crime + black_bins,
                   data = nbh_sf_avg)

stargazer(yearly_model,yearly_model_w_race,align = TRUE, out="03_stop_frisk/images/models_w_race.htm")

nbh_sf_yearly$predicted_w_race <- predict(yearly_model_w_race)

nbh_sf_yearly$coll_bins <-cut(nbh_sf_yearly$pctnonwhite, c(0,10,40,60,90,100))
nbh_sf_yearly$black_bins <- cut(nbh_sf_yearly$PctBlackNonHispBridge_2010, c(0,25,50,75,95,100))

levels(nbh_sf_yearly$coll_bins) <- c("0-10%","10-40%","40-60%","60-90%","90-100%")
levels(nbh_sf_yearly$black_bins) <- c("0-25%","25-50%","50-75%","75-95%","95-100%")

crime_frisk_yearly_race <- ggplot(data=nbh_sf_yearly,aes(x=crime,y=stop_frisks,color=black_bins)) + 
  geom_point(aes(color=black_bins)) +
  geom_smooth(method = lm,size=2,se = F) +
  geom_point(data=subset(nbh_sf_yearly,neighborhood == "Ivy City, Arboretum, Trinidad, Carver Langston" &
                           year == "2014"),
             colour="red",pch=21) +
  geom_text(data=subset(nbh_sf_yearly,neighborhood == "Ivy City, Arboretum, Trinidad, Carver Langston" &
                          year == "2014"),
            aes(label=paste0("Ivy City (",year,")")),
            hjust=1.1, size=4,colour="black") +
  theme_fivethirtyeight() +
  labs(x = "Number of Crimes Reported in Year", 
       y = "Number Stop and Frisks in Subsequent Year",
       color = "Neighborhood % Black Residents") +
  ggtitle("Crime Incidents vs. Stop and Frisk Incidents\nby Neighborhood Racial Composition") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

ggsave(plot = crime_frisk_yearly_race, "03_stop_frisk/images/crime_frisk_yearly_race.png", w = 10.67, h = 8,type = "cairo-png")

## looking at crime-only residulas by neighborhood racial composition

nbh_sf_avg$predicted <- predict(yearly_model)
nbh_sf_avg$residuals <- residuals(yearly_model)

residuals_nbh_race <- nbh_sf_avg %>%
  group_by(neighborhood,PctBlackNonHispBridge_2010) %>%
  summarise(avg_residuals = mean(residuals))

crime_model_residuals <- ggplot(residuals_nbh_race, aes(x = PctBlackNonHispBridge_2010, y = avg_residuals)) +
  geom_smooth() +
  geom_point() +  
  geom_point(data=subset(residuals_nbh_race,neighborhood == "Ivy City, Arboretum, Trinidad, Carver Langston"),
             colour="red") +
  geom_text(data=subset(residuals_nbh_race,neighborhood == "Ivy City, Arboretum, Trinidad, Carver Langston"),
            aes(label="Ivy City (2010-2017)"),
            hjust=1.1, size=4,colour="black") +
  geom_hline(yintercept = 0) +
  annotate("text", label = "More stop and frisk than model predicted", x = 15, y = 95, size = 4, colour = "red") +
  annotate("text", label = "Less stop and frisk than model predicted", x = 15, y = -65, size = 4, colour = "dark green") +
  theme_fivethirtyeight() +
  labs(x = "Neighborhood Percent of Black Residents", y = "Model Residual") +
  ggtitle("Linear Model Residuals by Neighborhood Racial Composition") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())

ggsave(plot = crime_model_residuals, "03_stop_frisk/images/09_crime_model_residuals.png", w = 10.67, h = 8,type = "cairo-png")

## total stop and frisk counts by neighborhood race from census

nbh_sf_df <- merge(nbh_sf_df,additional_cluster_info,by.x="neighborhood",by.y="NBH_NAMES")

nbh_sf_df_census <- merge(nbh_sf_df,census_data_original,by.x=c("NAME"),by.y=c("CLUSTER_TR2000"))

nbh_sf_df_census$pct_non_white <- nbh_sf_df_census$PctAsianPINonHispBridge_2010 + 
  nbh_sf_df_census$PctBlackNonHispBridge_2010 +
  nbh_sf_df_census$PctHisp_2010

nbh_sf_df_census$non_white_bins <-cut(nbh_sf_df_census$pct_non_white, c(0,10,20,30,40,50,60,70,80,90,100))
nbh_sf_df_census$black_bins <-cut(nbh_sf_df_census$PctBlackNonHispBridge_2010, c(0,25,50,75,95,100))

total_sf_nbh_race <- nbh_sf_df_census %>%
  group_by(year,black_bins) %>%
  summarise (sf = n())

avg_sf_nbh_race <- total_sf_nbh_race %>%
  group_by(black_bins) %>%
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
crime_w_census$black_bins <-cut(crime_w_census$PctBlackNonHispBridge_2010, c(0,25,50,75,95,100))

crime_w_census <- crime_w_census %>%
  group_by(black_bins) %>%
  summarise (crime = mean(crime),pop=mean(TotPop_2010))

crime_sf_race_bins <- merge(crime_w_census,avg_sf_nbh_race,by="black_bins")

crime_sf_race_bins$adj_sf <- (crime_sf_race_bins$avg_sf / crime_sf_race_bins$pop) * 100
crime_sf_race_bins$adj_crime <- (crime_sf_race_bins$crime / crime_sf_race_bins$pop) * 100

crime_sf_race_l <- melt(crime_sf_race_bins, id.vars=c("black_bins")) %>%
  subset(variable %in% c("adj_sf","adj_crime"))

levels(crime_sf_race_l$variable) <- c("crime","pop","","Average Stop and Frisk","Average Crime")
levels(crime_sf_race_l$black_bins) <- c("0-25%","25-50%","50-75%","75-95%","95-100%")

sf_crime_nbh_race <- ggplot(crime_sf_race_l,aes(x=black_bins,y=value,fill=variable,group=as.character(variable))) + 
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label = round(value,2)), position = position_dodge(0.9),vjust=-.5,size=5) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Crime & Stop and Frisk per 100 people') + xlab("Neighborhood Percent of Black Residents") + 
  scale_fill_discrete(name="Legend") +
  ggtitle("Average Yearly Stop and Frisk vs. Crime per 100 Residents") 

ggsave(plot = sf_crime_nbh_race, "03_stop_frisk/images/10_sf_crime_nbh_race.png", w = 10.67, h = 8,type = "cairo-png")

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
  filter(variable %in% c("Black","Hispanic/Latino","White"))

census_nbh_pct_w <- dcast(census_nbh_pct_black,CLUSTER_TR2000 ~ variable,value.var = "census_value")

census_nbh_pct_black$bins <- cut(census_nbh_pct_w$Black, c(0,10,40,60,80,100))

nbh_crimes_df$race_ethn <- ifelse(as.character(nbh_crimes_df$ethnicity)=='Hispanic Or Latino','Hispanic/Latino',
                                   as.character(nbh_crimes_df$race))

nbh_crimes_df$race_ethn[nbh_crimes_df$ethnicity == "Hispanic Or Latino"] <- "Hispanic/Latino" 

## calculate neighborhood-level crimes by race

crimes_by_race_nbh <- nbh_crimes_df %>%
  group_by(neighborhood,race_ethn) %>%
  summarise(crimes=n()) %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino"))

## calculate neighborhood-level stop and frisks by race

stops_by_race_nbh <- nbh_sf_df %>%
  filter(year == 2017) %>%
  group_by(neighborhood,race_ethn) %>%
  summarise(stop_frisks=n()) %>%
  filter(race_ethn %in% c("White","Black","Hispanic/Latino"))

## merge arrests & stop and frisk then census

stops_crimes_nbh <- merge(crimes_by_race_nbh,stops_by_race_nbh,by=c("neighborhood","race_ethn"),
                           all = T)

additional_cluster_info <- read.csv("data/shapefiles/Neighborhood_Clusters.csv")

stops_crimes_nbh <- merge(stops_crimes_nbh,additional_cluster_info,by.x="neighborhood",by.y="NBH_NAMES")

stops_crimes_tracts_nbh <- merge(stops_crimes_nbh,census_nbh_pct_w,by.x="NAME",by.y="CLUSTER_TR2000")

stops_crimes_tracts_nbh$adj_sf <- (stops_crimes_tracts_nbh$stop_frisks / stops_crimes_tracts_nbh$pop) * 100
stops_crimes_tracts_nbh$adj_crimes <- (stops_crimes_tracts_nbh$crimes / stops_crimes_tracts_nbh$pop) * 100

## roll up neighborhoods and aggregate stop & frisk and arrests based on racial bins

stops_crimes_nbh_census_bins <- stops_crimes_tracts_nbh %>%
  replace(is.na(.), 0) %>%
  group_by(bins,race_ethn) %>%
  summarise(total_crimes = sum(crimes),
            total_sf = sum(stop_frisks))

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
  ggtitle("2017 Stop & Frisk to 2016 Crimes Ratio")

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
  geom_point(aes(size=n,color=subgroup),alpha=.7) +
  #geom_point(aes(size=census_value,color=subgroup),alpha=.7) +
  geom_point(data=subset(nbh_crime_race,NBH_NAMES == "Columbia Heights, Mt. Pleasant, Pleasant Plains, Park View" &
                           subgroup %in% c("White","Black")),
             aes(size=n),colour="black",pch=21) +
  geom_text(data=subset(nbh_crime_race,NBH_NAMES == "Columbia Heights, Mt. Pleasant, Pleasant Plains, Park View" &
                          subgroup %in% c("White","Black")),
            aes(label=paste0("Columbia Heights\n(",subgroup,")")),
            hjust="center", vjust = -.5, size=4) +
  geom_point(data=subset(nbh_crime_race,NBH_NAMES == "Columbia Heights, Mt. Pleasant, Pleasant Plains, Park View" &
                           subgroup %in% c("Hispanic/Latino")),
             aes(size=n),colour="black",pch=21) +
  geom_text(data=subset(nbh_crime_race,NBH_NAMES == "Columbia Heights, Mt. Pleasant, Pleasant Plains, Park View" &
                          subgroup %in% c("Hispanic/Latino")),
            aes(label=paste0("Columbia Heights\n(",subgroup,")")),
            hjust=-.1, vjust = .5, size=4) +
  scale_x_continuous(limits = c(0, 1),labels = scales::percent) + 
  scale_y_continuous(limits = c(0, 1.0),labels = scales::percent) + 
  geom_abline(intercept = 0,slope = 1) + 
  geom_hline(yintercept = .5) + geom_vline(xintercept = .5) +
  theme_fivethirtyeight() +
  labs(x = "Neighborhood Stop and Frisk (2017)", y = "Neighborhood Crime (2016)") +
  ggtitle("Neighborhood Crime vs. Neighborhood Stop and Frisk") +
  scale_color_discrete(name="Legend") +
  #scale_size_continuous(name="Total Stop & Frisk") +
  scale_size_continuous(name="Neighborhood Population") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text()) +
  theme(legend.position="bottom", legend.box = "horizontal")

ggsave(plot = nbh_crime_sf_race_scatter, "03_stop_frisk/images/11_nbh_crime_sf_race_scatter.png", w = 10.67, h = 10.67,type = "cairo-png")

######################################
##
## plotting crime by census
##
######################################

nbh_crimes_race <- merge(additional_cluster_info,nbh_crimes_race,by.y="neighborhood",by.x="NBH_NAMES")
nbh_crime_demos_census <- merge(nbh_crimes_race,census_data,by.x=c("NAME","subgroup"),by.y=c("CLUSTER_TR2000","variable"))

nbh_crime_demos_census <- merge(nbh_crime_demos_census,nbh_sf_demos_census[ , c("NAME","subgroup","n")],by=c("NAME","subgroup"))

crime_and_census_plot <- ggplot(data=filter(nbh_crime_demos_census,subgroup %in% c("White","Black","Hispanic/Latino")),aes(x=census_value/100,y=crime_freq)) + 
  geom_point(aes(size=n,color=subgroup),alpha=.7) +
  #geom_point(aes(size=census_value,color=subgroup),alpha=.7) +
  scale_x_continuous(limits = c(0, 1),labels = scales::percent) + 
  scale_y_continuous(limits = c(0, 1.0),labels = scales::percent) + 
  geom_abline(intercept = 0,slope = 1) + 
  geom_hline(yintercept = .5) + geom_vline(xintercept = .5) +
  theme_fivethirtyeight() +
  labs(x = "Neighborhood Population", y = "Percent of Crime") +
  ggtitle("Neighborhood Crime v. Census Population") +
  scale_color_discrete(name="Legend") +
  scale_size_continuous(name="Total Stop & Frisk") +
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text()) 

ggsave(plot = crime_and_census_plot, "03_stop_frisk/images/crime_and_census_plot.png", w = 10.67, h = 10.67,type = "cairo-png")

nbh_crime_race$diff <-  nbh_crime_race$freq -  (nbh_crime_race$crime_freq) 

nbh_diff_crimes <- ggplot(data=filter(nbh_crime_race,subgroup %in% c("White","Black","Hispanic/Latino")), 
                        aes(x = reorder(NBH_NAMES, diff), 
                            y = as.numeric(diff))) + 
  geom_point(aes(size=n,color=subgroup),alpha=.7,stat='identity') + 
  labs(x = "Neighborhood", y = "Stop & Frisk - Population") +
  coord_flip() +
  ggtitle("Difference Between 2017 Stop and Frisk\n & 2016 Crime Rate by Race") + 
  geom_hline(yintercept = 0) +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  scale_y_continuous(labels=scales::percent) +
  guides(color=guide_legend(title="Race/Ethnicity"),
         size=guide_legend(title="Total Stop and Frisk")) 

ggsave(plot = nbh_diff_crimes, "03_stop_frisk/images/12_nbh_diff_crimes.png", w = 10.67, h = 8,type = "cairo-png")

###################################
##
## poisson modelling
##
###################################

stops_crimes_tracts_nbh$race_ethn <- factor(stops_crimes_tracts_nbh$race_ethn,levels = c("White","Black","Hispanic/Latino"))

stops_crimes_tracts_nbh$black_bins <-cut(stops_crimes_tracts_nbh$Black, c(0,25,50,75,95,100))

stop_model <- glm(stop_frisks ~ race_ethn + Black, family=quasipoisson,
                  offset=log(crimes),
                  data = stops_crimes_tracts_nbh,
                  subset=crimes>0 & stop_frisks>0)

summary(stop_model)

stargazer(stop_model,align = TRUE, out="03_stop_frisk/images/poisson.htm")
coefs <- data.frame(stop_model$coefficients,check.rows = T)
coefs$ix <- "index"
coefs$values <- row.names(coefs)

coefs_w <- dcast(coefs,ix ~ values, value.var = "stop_model.coefficients")

neighborhoods_1 <- seq(0, 100, by=1)

all <- expand.grid(neighborhoods_1) 

regression_output <- merge(all,coefs_w,by = NULL)

regression_output$black_value <- regression_output$`(Intercept)` + 
  regression_output$race_ethnBlack +
  regression_output$Var1 * regression_output$Black

regression_output$hisp_value <- regression_output$`(Intercept)` + 
  regression_output$`race_ethnHispanic/Latino` +
  regression_output$Var1 * regression_output$Black

regression_output$white_value <- regression_output$`(Intercept)` + 
  0 +
  regression_output$Var1 * regression_output$Black

ggtern(data = regression_output,aes(x=white,y=black,z=hisp)) +
  geom_point(size=1,aes(color=regression_output$black_value)) +
  theme_bw() +
  scale_color_gradient2(low = "blue", mid = "white", high = "red")

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

labels <- c("0-25% black","25-50% black","50-75% black","75-95% black","95-100% black")

results <- data.frame(labels, Black, White,Hispanic) 

results_l <- regression_output %>%
  select(Var1,black_value,white_value,hisp_value)

results_l <- melt(results_l,id.vars = "Var1")

levels(results_l$variable) <- c("Black","White","Hispanic/Latino")

poisson_plot <- ggplot(data=results_l,aes(x=Var1/100,y=exp(as.numeric(value)),color=variable,group=as.character(variable))) + 
  geom_line(size=2) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title = "Estimated Stop and Frisk by Neighborhood Racial Composition",
       subtitle = "Poisson regression results using constant term + race parameters for each neighborhood composition", 
       y = 'Stop and Frisks per Crime',
       x="Neighborhood Percent of Black Residents") + 
  scale_color_discrete(name="Legend") +
  scale_x_continuous(labels=scales::percent)

ggsave(plot = poisson_plot, "03_stop_frisk/images/13_poisson_plot.png", w = 10.67, h = 8,type = "cairo-png")

###################################
##
## exporting nbh data to csv for table
##
###################################

nbh_sf_demos_census$census_percent <- nbh_sf_demos_census$census_value / 100
nbh_sf_demos_census$census_percent <- round(nbh_sf_demos_census$census_percent,2)

output_sf <- dcast(data = nbh_sf_demos_census, neighborhood ~ subgroup, value.var = "freq")
  
output_sf <- output_sf %>%
  rename(Total.stop_and_frisk = Total,
         Juvenile.stop_and_frisk = Juvenile,
         White.stop_and_frisk = White,
         Black.stop_and_frisk = Black,
         "Hispanic.Latino.stop_and_frisk" = "Hispanic/Latino")

output_census <- dcast(data = nbh_sf_demos_census, neighborhood ~ subgroup, value.var = "census_percent")

output_census <- output_census %>%
  rename(Total.census = Total,
         Juvenile.census = Juvenile,
         White.census = White,
         Black.census = Black,
         "Hispanic.Latino.census" = "Hispanic/Latino")

output <- merge(output_sf,output_census,by=c("neighborhood"))

output$Black.Diff <- output$Black.census - output$Black.stop_and_frisk
output$Hispanic.Latino.Diff <- output$Hispanic.Latino.census - output$Hispanic.Latino.stop_and_frisk
output$Juvenile.Diff <- output$Juvenile.census - output$Juvenile.stop_and_frisk
output$White.Diff <- output$White.census - output$White.stop_and_frisk

order_cols <- c("neighborhood","Black.stop_and_frisk","Black.census","Black.Diff",
                "Hispanic.Latino.stop_and_frisk","Hispanic.Latino.census","Hispanic.Latino.Diff",
                "Juvenile.stop_and_frisk","Juvenile.census","Juvenile.Diff","White.stop_and_frisk",
                "White.census","White.Diff")

output <- output[, order_cols]

write.csv(output, "03_stop_frisk/scripts/shiny/sf_nbh_summary.csv")
