library(geosphere)
library(sp)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggmap)
library(ggthemes)
library(gganimate)
library(magick)
library(gridExtra)

##########################################
##
## Load data (downloaded from http://opendata.dc.gov/datasets/70392a096a8e431381f1f692aaa06afd_24/data)
##
##########################################

crash_data <- read.csv("data/transportation/Crashes_in_DC.csv")
bike_data <- subset(crash_data,TOTAL_BICYCLES==1)

bike_data$REPORTDATE <- gsub("T"," ",bike_data$REPORTDATE)
bike_data$REPORTDATE <- gsub(".000Z"," ",bike_data$REPORTDATE)

bike_data$day_of_week <- weekdays(as.Date(bike_data$REPORTDATE))

bike_data$weekend <- "Weekday"
bike_data$weekend[bike_data$day_of_week=="Saturday" | bike_data$day_of_week=="Sunday"] <- "Weekend"

bike_data$time_of_day <- format(strptime(bike_data$REPORTDATE, "%Y-%m-%d %H:%M:%S"), "%H")

bike_data$injuries <- "No Injury"
bike_data$injuries[bike_data$MINORINJURIES_BICYCLIST==1] <- "Minor Injury"
bike_data$injuries[bike_data$MAJORINJURIES_BICYCLIST==1] <- "Major Injury"
bike_data$injuries[bike_data$UNKNOWNINJURIES_BICYCLIST==1] <- "Unknown Injury"
bike_data$injuries[bike_data$FATAL_BICYCLIST==1] <- "Fatal Injury"

bike_data$year_month <- format(as.Date(bike_data$FROMDATE), "%Y-%m")

##########################################
##
## Map of crashes in DC
##
##########################################

map <- get_map(location = 'Washington DC', zoom = 12)

crash_map <- ggmap(map) + stat_density2d(data=bike_data, aes(x=bike_data$ï..X, y=bike_data$Y, fill=..level.., alpha=..level..), geom="polygon") +
  geom_point(data=bike_data, aes(x=bike_data$ï..X, y=bike_data$Y), shape=16, alpha=0.8) +
  guides(size=FALSE, alpha = FALSE) + xlab("") + ylab("") +
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank()) +
  scale_alpha_continuous(range=c(0.1,0.5))  + facet_wrap( ~ weekend)

##########################################
##
## Daily breakdown of crashes
##
##########################################

crashes_per_day_of_week <- bike_data %>%
  group_by(day_of_week) %>%
  summarise (n = n())

ggplot(crashes_per_day_of_week,aes(x=day_of_week,y=n)) + geom_bar(stat='identity') +
  scale_x_discrete(limits = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

daily_crashes_by_hour <- bike_data %>%
  group_by(weekend,time_of_day) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n))

ggplot(daily_crashes_by_hour,aes(x=time_of_day,y=freq)) + geom_line() +
  geom_bar(stat='identity') + facet_wrap( ~ weekend)

##########################################
##
## Number of crashes per month
##
##########################################

crashes_per_month <- bike_data %>%
  group_by(year_month) %>%
  summarise (n = n())

bars <- ggplot(crashes_per_month,aes(x=year_month,y=n)) + 
  geom_bar(stat='identity') 

##########################################
##
## Most dangerous streets/intersections
##
##########################################

intersections <- bike_data %>%
  group_by(ADDRESS) %>%
  summarise (n = n())

streets <- bike_data %>%
  group_by(NEARESTINTSTREETNAME) %>%
  summarise (n = n())

injuries <- bike_data %>%
  group_by(injuries) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n))
