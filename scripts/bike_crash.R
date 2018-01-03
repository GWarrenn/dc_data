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
bike_data <- subset(crash_data,TOTAL_BICYCLES>=1)

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

bike_data$year_month <- format(as.Date(bike_data$REPORTDATE), "%Y-%m")
bike_data$year_month_day <- format(as.Date(bike_data$REPORTDATE), "%Y%m%d")
bike_data$month <- format(as.Date(bike_data$REPORTDATE), "%m")

##########################################
##
## Crash Details 
##
##########################################

injuries <- bike_data %>%
  group_by(injuries) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n))

plot_01_injuries <- ggplot(injuries, aes(x=injuries,y=freq,fill=injuries)) +
  geom_bar(stat = "identity",show.legend=FALSE) +
  geom_text(aes(label=percent(freq)), 
            vjust=-.5, position=position_dodge(.5), size=5) +
  scale_y_continuous(labels = scales::percent) + ylab("") + xlab("") + ggtitle("Breakdown of Bike Crash Injuries") +
  labs(caption = "Source: DC OpenData Portal: http://opendata.dc.gov/datasets/crashes-in-dc") +
  scale_x_discrete(limits=c("No Injury","Minor Injury","Major Injury","Fatal Injury")) +
  theme_fivethirtyeight() + theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5))

ggsave(plot = plot_01_injuries, "images/injuries.png", w = 10.67, h = 8,type = "cairo-png")

details <- read.csv("data/transportation/Crash_Details_Table.csv")

crash_details <- merge(bike_data,details,by.y="CRIMEID",by.x="CRIMEID")

# worst drivers by state

worst_drivers_by_state <- crash_details %>%
  filter(crash_details$PERSONTYPE=="Driver" & crash_details$LICENSEPLATESTATE!="" & crash_details$LICENSEPLATESTATE!="Un") %>%
  group_by(PERSONTYPE,LICENSEPLATESTATE) %>%
  summarise (n = n()) %>%
  filter(n>5)

plot_02_drivers_state <- ggplot(worst_drivers_by_state, aes(x = reorder(LICENSEPLATESTATE, n), y = n,fill=n)) + 
  geom_bar(stat = "identity",show.legend=FALSE) + coord_flip() + 
  labs(caption = "Source: DC OpenData Portal: http://opendata.dc.gov/datasets/crashes-in-dc", 
       y = "Number of Crashes", x = "") + ggtitle("Total Number of Accidents Involving Bicycles by State License Plate") +
  geom_text(aes(label=n),hjust=-0.1, position=position_dodge(.5), size=5) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5))

ggsave(plot = plot_02_drivers_state, "images/drivers_state.png", w = 10.67, h = 8,type = "cairo-png")


# ages

age <- crash_details %>%
  filter(!is.na(AGE)) %>%
  group_by(PERSONTYPE) %>%
  summarise (avg_age = mean(AGE))

plot_03_age <- ggplot(data=filter(crash_details,PERSONTYPE %in% c("Bicyclist","Driver")),
       aes(PERSONTYPE,AGE)) + geom_violin() +
  labs(caption = "Source: DC OpenData Portal: http://opendata.dc.gov/datasets/crashes-in-dc", 
       y = "", x = "") + ggtitle("Average Age of Drivers/Bicyclists") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5))

ggplot(data=filter(crash_details,PERSONTYPE %in% c("Bicyclist","Driver")),
       aes(AGE,fill=PERSONTYPE)) + geom_histogram(position="identity", alpha=0.5) +
  labs(caption = "Source: DC OpenData Portal: http://opendata.dc.gov/datasets/crashes-in-dc", 
       y = "", x = "") + ggtitle("Average Age of Drivers/Bicyclists") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5))

ggsave(plot = plot_03_age, "images/age.png", w = 10.67, h = 8,type = "cairo-png")

##########################################
##
## Map of crashes in DC
##
##########################################

map <- get_map(location = 'Washington DC', zoom = 13)

plot_01_crash_map <- ggmap(map) + stat_density2d(data=bike_data, aes(x=ï..X, y=Y, fill=..level.., alpha=..level..), geom="polygon") +
  #geom_point(data=bike_data, aes(x=ï..X, y=Y), shape=16, alpha=0.2) +
  guides(size=FALSE, alpha = FALSE) + xlab("") + ylab("") + 
  scale_alpha_continuous(range=c(0.1,0.5)) +
  ggtitle("Washginton DC Bike Accidents") + theme_fivethirtyeight() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
        strip.text = element_text(size = 12)) + guides(fill=guide_legend(title="Crash Density")) +
  labs(caption="DC OpenData Portal: http://opendata.dc.gov/datasets/crashes-in-dc")

png(filename = "images/crash_map.png",width=800,height=600)  
print(plot_01_crash_map)
dev.off()

ggsave(plot = plot_01_crash_map, "images/crash_map.png", w = 10.67, h = 8,type = "cairo-png")

##########################################
##
## Daily breakdown of crashes
##
##########################################

crashes_per_day_of_week <- bike_data %>%
  group_by(day_of_week) %>%
  summarise (n = n())

plot_02_daily_crashes <- ggplot(crashes_per_day_of_week,aes(x=day_of_week,y=n)) + geom_bar(stat='identity',fill="dark blue") +
  scale_x_discrete(limits = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")) +
  xlab("") + ylab("Number of Accidents") + theme_fivethirtyeight() +
  geom_text(aes(label=crashes_per_day_of_week$n), 
            vjust=-0.5, position=position_dodge(.5), size=5) +
  labs(caption="DC OpenData Portal: http://opendata.dc.gov/datasets/crashes-in-dc")

ggsave(plot = plot_02_daily_crashes, "images/bike_crashes_per_day.png", w = 10.67, h = 8,type = "cairo-png")

daily_crashes_by_hour <- bike_data %>%
  group_by(weekend,time_of_day) %>%
  summarise (n = n()) %>%
  mutate(freq=n/sum(n))

plot_03_hourly_crashes <- ggplot(daily_crashes_by_hour,aes(x=time_of_day,y=freq,color=weekend,group=weekend)) +
  geom_line(size=2) +
  ggtitle("Bike Accidents by Time of Day") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5),strip.text = element_text(size = 12)) + 
  ylab('Percent of Daily Accidents') + xlab("Hour") + labs(caption="DC OpenData Portal: http://opendata.dc.gov/datasets/crashes-in-dc") +
  scale_y_continuous(labels = scales::percent) + scale_color_discrete(name="Legend")

ggsave(plot = plot_03_hourly_crashes, "images/bike_crashes_per_hour.png", w = 10.67, h = 8,type = "cairo-png")

##########################################
##
## Number of crashes per month
##
##########################################

crashes_per_month <- bike_data %>%
  filter(as.Date(bike_data$REPORTDATE)>'2016-01-01' & as.Date(bike_data$REPORTDATE)<'2017-01-01') %>%
  group_by(month) %>%
  summarise (n = n())

plot_04_monthly_crashes <- ggplot(crashes_per_month,aes(x=month,y=n)) + 
  geom_bar(stat='identity',fill="dark blue") + 
  geom_text(aes(label=crashes_per_month$n), 
            vjust=-0.5, position=position_dodge(.5), size=5) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Number of Accidents') + xlab("Month") + ggtitle("Total Number of Bike Accidents per Month (2016)")

ggsave(plot = plot_04_monthly_crashes, "images/bike_crashes_per_month.png", w = 10.67, h = 8,type = "cairo-png")


##########################################
##
## DC Bikeshare data
##
##########################################

files <- list.files("data/transportation/dc_bikeshare")

bikeshare_df <- data.frame()

for (f in files) {
  
  bikeshare_quarter_df <- read.csv(paste("data/transportation/dc_bikeshare/",f,sep=""))
  try(bikeshare_quarter_df$month <- month(as.POSIXlt(bikeshare_quarter_df$Start.date, format="%m/%d/%Y")))
  try(bikeshare_quarter_df$month <- month(as.POSIXlt(bikeshare_quarter_df$Start.Date, format="%Y-%m-%d")))
  
  crashes_per_month <- bikeshare_quarter_df %>%
    group_by(month) %>%
    summarise (n = n())
  
  bikeshare_df <- rbind(bikeshare_df,crashes_per_month)
}

bikeshare_summary <- bikeshare_df %>%
  group_by(month) %>%
  summarise (n = sum(n))

plot_04b_monthly_ridership <- ggplot(bikeshare_summary,aes(x=reorder(as.character(month),month),y=n)) + 
  geom_bar(stat='identity', fill="red") + 
  geom_text(aes(label=comma(bikeshare_summary$n)), 
            vjust=-0.5, position=position_dodge(.5), size=3) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5)) + 
  ylab('Monthly Ridership') + xlab("Month") + ggtitle("Total DC Bikeshare Trips per Month (2016)") +
  scale_y_continuous(labels = comma) 

grid.arrange(plot_04_monthly_crashes, plot_04b_monthly_ridership, ncol=2)  

ggsave(plot = grid.arrange(plot_04_monthly_crashes, plot_04b_monthly_ridership, ncol=2), "images/bikeshare_comparison.png", w = 13.33, h = 8,type = "cairo-png")


##########################################
##
## Most dangerous streets/intersections
##
##########################################

intersections <- bike_data %>%
  group_by(ADDRESS) %>%
  summarise (n = n())

most_dangerous_addresses <- arrange(intersections,desc(n))
most_dangerous_addresses = most_dangerous_addresses[1:10,]

streets <- bike_data %>%
  group_by(NEARESTINTSTREETNAME) %>%
  summarise (n = n())

streets <- streets[!(is.na(streets$NEARESTINTSTREETNAME) | streets$NEARESTINTSTREETNAME==""), ]

most_dangerous_streets <- arrange(streets,desc(n))
most_dangerous_streets = most_dangerous_streets[1:10,]

plot_06_most_dangerous_streets <- ggplot(most_dangerous_streets, aes(x = reorder(NEARESTINTSTREETNAME, n), y = n,fill=n)) + 
  geom_bar(stat = "identity",show.legend=FALSE) + coord_flip() + 
  labs(caption = "Source: DC OpenData Portal: http://opendata.dc.gov/datasets/crashes-in-dc", 
       y = "Number of Crashes", x = "") + ggtitle("Most Dangerous Streets in DC (For Cyclists)") +
  geom_text(aes(label=n),hjust=-0.1, position=position_dodge(.5), size=5) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5))

ggsave(plot = plot_06_most_dangerous_streets, "images/most_dangerous_streets.png", w = 10.67, h = 8,type = "cairo-png")

##########################################
##
## Effect of weather on crashes
##
##########################################

crashes_per_day <- bike_data %>%
  filter(as.Date(bike_data$REPORTDATE)>'2016-01-01' & as.Date(bike_data$REPORTDATE)<'2016-12-01') %>%
  group_by(year_month_day) %>%
  summarise (total_crashes = n())

crashes_per_day$year_month_day <- format(as.character(crashes_per_day$year_month_day))

weather <- read.csv("data/weather/weather_2006_2016.csv")
weather <- subset(weather,STATION_NAME=="DALECARLIA RESERVOIR DC US")
weather <- subset(weather,DATE>=20150823)

weather$PRCP[weather$PRCP<0] <- NA
weather$TMAX[weather$TMAX<0] <- NA

weather <- weather %>%
  filter(weather$DATE>20160101 & weather$DATE<20161201)
  
weather$DATE <- format(as.character(weather$DATE))

crashes_weather <- merge(crashes_per_day,weather,by.y="DATE",by.x="year_month_day",all=TRUE)

crashes_weather$total_crashes[is.na(crashes_weather$total_crashes)] <- 0

crashes_by_temp = crashes_weather %>%
  mutate(temperature_bucket = floor(TMAX / 5) * 5) %>%
  group_by(temperature_bucket) %>%
  summarize(avg_crashes = mean(total_crashes),
            avg_max_temp = mean(TMAX),
            count = n()) 

plot_07_accidents_weather <- ggplot(data = crashes_by_temp, aes(x = avg_max_temp,y = avg_crashes)) +
  geom_line(size=2) +
  labs(caption = "Source: DC OpenData Portal: http://opendata.dc.gov/datasets/crashes-in-dc", 
       y = "Average Number of Accidents", x = "Average Temperature") + ggtitle("Average Accidents v. Average Temperature") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),plot.title = element_text(hjust = 0.5))

ggsave(plot = plot_07_accidents_weather, "images/accidents_weather.png", w = 10.67, h = 8,type = "cairo-png")




