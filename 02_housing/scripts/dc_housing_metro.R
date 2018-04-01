library(geosphere)
library(sp)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggmap)
library(ggthemes)
library(gmapsdistance)
library(gganimate)
library(magick)

######################################################
##
## Housing trends from Zillow
##
######################################################

data <- read.csv(file="data/real_estate/Zip_Zhvi_DC.csv")

long <- melt(data, id.vars= c("RegionID","RegionName","City",
                              "State","Metro","CountyName","SizeRank"))

long$variable <- gsub("X", "", long$variable)
long$variable <- gsub("\\.", "/", long$variable)
long$variable <- gsub("$", "/01", long$variable)

long$variable <- as.Date(long$variable, "%Y/%m/%d")
long$RegionName <- as.character(long$RegionName)

long$year <- sub("-.*", "", long$variable)

Sys.setenv(PATH = paste("C:\\Program Files\\ImageMagick-7.0.6-Q16",Sys.getenv("PATH"), sep = ";"))

long$dollar_amt <- paste0("$", formatC(as.numeric(long$value), format="f", digits=0, big.mark=","))
long$dollar_amt[long$dollar_amt=="$NA"] <- ""

p2 <- ggplot(data=filter(long,as.numeric(year)>=2000), 
             aes(x=RegionName, y=value, fill = RegionName, frame = variable)) +
  geom_bar(stat="identity",position = "identity") +
  geom_text(aes(label=dollar_amt), vjust=-.5, position=position_dodge(.5), size=4) + 
  scale_color_fivethirtyeight(long$RegionName) + 
  theme_fivethirtyeight() + labs(caption = "Source: Zillow Home Value Index Data", 
                                 x = "Zip Code", y = "$") + ggtitle("Home Value Index: ") + 
  guides(fill=FALSE) + theme(plot.title = element_text(hjust = 0.5))

gganimate(p2, "output_bars.gif",interval = .2,ani.width=800, ani.height=600)

######################################################
##
## GETTING LAT/LONG FROM HOUSING ADDRESS
##
######################################################

housing <- read.csv("data/real_estate/September2017Sales.csv", stringsAsFactors = FALSE)

# removing unit #'s

housing$Address <- gsub("#.*$","",housing$Address) 

for(i in 1:nrow(housing)) {
  result <- geocode(paste(housing$Address[i],"Washington DC"), output = "latlona", source = "google")
  housing$lon[i] <- as.numeric(result[1])
  housing$lat[i] <- as.numeric(result[2])
  try(housing$geoAddress[i] <- as.character(result[3]))
  Sys.sleep(0.5)  # API only allows a certain amount of requests per second
}
#
#fix <- function(s){
  gsub(" ", "+", str_trim(s))
}

for(i in 1:nrow(housing)) {

  GEOCODE.PREFIX <- "http://geocoding.geo.census.gov/geocoder/geographies/address?street="
  GEOCODE.SUFFIX <- "&benchmark=Public_AR_Census2010&vintage=Census2010_Census2010&layers=14&format=json"
  
  housing$census_tract[i] <- NA
  
  URL <- paste(GEOCODE.PREFIX,fix(housing$Address[i]), "&city=Washington", "&state=DC", GEOCODE.SUFFIX)
  try(JSON <- fromJSON(URL))
  try(housing$census_tract[i] <- JSON$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$TRACT)

}

## Cartesian join housing data to metro data 

metro_data <- read.csv("data/transportation/Metro_Stations_in_DC.csv")

combined <- merge(metro_data,housing,by=NULL) 

combined$lat <- as.numeric(as.character(combined$lat), digits=15)
combined$lon <- as.numeric(as.character(combined$lon), digits=15)

## calculating distances from each house to each metro stop

combined$distance <- distHaversine(combined[, 19:20],  combined[, 1:2]) / 1609
combined$distance <- combined$distance * 5280 #convert to feet

combined <- arrange(combined,Address,desc(distance))
closest_metro <- combined %>% group_by(Address) %>% arrange(distance) %>% slice(1)

closest_metro <- closest_metro[complete.cases(closest_metro), ]

closest_metro$adj_price <- as.numeric(gsub("\\$|,","",closest_metro$Close.Price)) / (as.numeric(closest_metro$BRs) + 1)

closest_metro$quadrant <- gsub(" ([A-Z][A-Z])$"," || \\1",closest_metro$ADDRESS)
closest_metro$quadrant <- gsub(".* ||","",closest_metro$quadrant)

for(i in 1:nrow(closest_metro)) {
  results = gmapsdistance(origin = paste(closest_metro$lat[i],"+",closest_metro$lon[i],sep=""),
                          destination = paste(closest_metro$Y[i],"+",closest_metro$ï..X[i],sep=""),
                          mode = "walking")
  closest_metro$walking_time[i] <- as.numeric(results$Time)
  Sys.sleep(0.5)
  
}  

## Overall plot of price by distance

plot_1_scatter <- ggplot(closest_metro,aes(x=as.numeric(closest_metro$distance),y=closest_metro$adj_price)) + geom_point()

ggplot(closest_metro,aes(x=closest_metro$distance,y=closest_metro$walking_time)) + 
  geom_point() + geom_smooth()

## Plot of housing price broken up by Ward

plot_2_scatter_quadrant <- ggplot(closest_metro,aes(x=as.numeric(closest_metro$walking_time),y=closest_metro$adj_price)) + geom_point() + 
  facet_wrap( ~ quadrant) + scale_y_continuous(labels=comma)

#######################################

## Filter data to walking distance

#######################################

# Testing two methods for proximity to metro: Haverstine Distance & Walking Time (Google Maps)

walking_distance <- closest_metro[closest_metro$distance <= 1000,]
walking_time <- closest_metro[closest_metro$walking_time <= 600,]

qmplot(lon,lat,data=walking_distance,maptype = "toner-lite", color = NAME) + scale_colour_discrete(guide=FALSE)

plot_3a_walking_map <- ggmap(map) + stat_summary_2d(geom = "tile",bins = 50,data=closest_metro, aes(x = lon, y = lat, z = closest_metro$walking_time), alpha=0.5) +
  scale_fill_gradient(low = "yellow", high = "red", guide = guide_legend(title = "Seconds")) +xlab("") + ylab("") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) + ggtitle("Walking Distance to Metro")

closest_metro_trim <- closest_metro[closest_metro$walking_time <= 5000,]

closest_metro_trim$minutes <- closest_metro_trim$walking_time / 60

map <- get_map(location = 'Washington DC', zoom = 12)

plot_3b_walking_map <- ggmap(map) + stat_summary_2d(geom = "tile",bins = 50,data=closest_metro_trim, aes(x = lon, y = lat, z = as.numeric(closest_metro_trim$minutes)), alpha=0.5) +
  scale_fill_gradient(low = "yellow", high = "red", guide = guide_legend(title = "Minutes")) +xlab("") + ylab("") +
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),plot.title = element_text(hjust = 0.5),text = element_text(size=10)) + 
  ggtitle("Walking Time to Metro")

avg_price_by_distance <- walking_distance %>%
                      group_by(NAME) %>%
                      summarise(average_price=mean(adj_price))

plot_4a_price_dist <- ggplot(avg_price_by_distance, aes(x = reorder(avg_price_by_distance$NAME, avg_price_by_distance$average_price), 
                               y = as.numeric(avg_price_by_distance$average_price),fill=avg_price_by_distance$average_price)) + 
  geom_bar(stat = "identity",show.legend=FALSE) + coord_flip()  +
  ggtitle("Average Housing Price (Per Bedroom) within 1,000 Feet by Metro Stop") + 
  geom_text(aes(label=round(avg_price_by_distance$average_price,.1)), vjust=0,hjust=-0.1, position=position_dodge(.5), size=3,) + 
  labs(x = "Metro Stop", y = "Average housing price per bedroom") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size=10)) +  scale_y_continuous(labels=comma)

avg_price_by_time <- walking_time %>%
  group_by(NAME) %>%
  summarise(average_price=mean(adj_price))

plot_4b_price_time <- ggplot(avg_price_by_time, aes(x = reorder(avg_price_by_time$NAME, avg_price_by_time$average_price), 
                                  y = as.numeric(avg_price_by_time$average_price),fill=avg_price_by_time$average_price)) + 
  geom_bar(stat = "identity",show.legend=FALSE) + coord_flip()  +
  ggtitle("Average Housing Price (Per Bedroom) within 10 Minute Walk to Metro Stop") + 
  geom_text(aes(label=round(avg_price_by_time$average_price,.1)), vjust=0,hjust=-0.1, position=position_dodge(.5), size=3,) + 
  labs(x = "Metro Stop", y = "Average housing price per bedroom") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size=10)) +  scale_y_continuous(labels=comma)

## modelling

overall_model <- lm(closest_metro$adj_price ~ closest_metro$walking_time)

summary(overall_model)$r.squared

overall_model_w_ward <- lm(closest_metro$adj_price ~ closest_metro$distance + closest_metro$quadrant)

summary(overall_model_w_ward)$r.squared


cols <- c('distance')

rf <- randomForest(closest_metro$adj_price ~ ., data=closest_metro[,cols], ntree=20)

#  

##other analysis
  #break plot out by quadrant/ward DONE
  #average home price by metro stop DONE
    #--most/least expensive metro to live near? DONE
  #number of lines?