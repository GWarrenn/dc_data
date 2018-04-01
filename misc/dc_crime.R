library(ggplot2)
library(dplyr)

setwd("C:/Users/augus/Desktop/dc/data/crime")

crime_data <- read.csv("C:/Users/augus/Desktop/dc/data/crime/2016_crime_data.txt")
crime_data <- crime_data[0,]

filenames <- list.files(path=getwd()) 

for (i in filenames) {  
  name <- gsub("-",".",i)
  name <- gsub(".txt","",name)  
  i <- paste(".\\",i,sep="")
  assign(name,read.csv(i, header=TRUE))

}

crime_data <- read.csv("C:/Users/augus/Desktop/dc/data/crime/2016_crime_data.txt")
crime_data <- rbind(crime_data,`2015_crime_data.txt`)
crime_data <- rbind(crime_data,`2014_crime_data.txt`)
crime_data <- rbind(crime_data,`2013_crime_data.txt`)
crime_data <- rbind(crime_data,`2012_crime_data.txt`)
crime_data <- rbind(crime_data,`2011_crime_data.txt`)
crime_data <- rbind(crime_data,`2009_2010_crime_data.txt`)
crime_data <- rbind(crime_data,`2007_2008_crime_data.txt`)

filenames <- list.files(path=getwd()) 

for (i in filenames) {  
  name <- gsub("-",".",i)
  name <- gsub(".txt","",name)  
  i <- paste(".\\",i,sep="")
  print(`i`)  
}



#Removing time of day from date
crime_data$REPORT_DAT <- gsub("[0-9][0-9]:[0-9][0-9]:[0-9][0-9] [A-Z][A-Z]","",crime_data$REPORT_DAT)
crime_data$REPORT_DAT <- gsub("[0-9]:[0-9][0-9]:[0-9][0-9] [A-Z][A-Z]","",crime_data$REPORT_DAT)
crime_data$REPORT_DAT <- as.Date(crime_data$REPORT_DAT,format="%m/%d/%Y")

crime_data$OFFENSE <- as.character(crime_data$OFFENSE)

crime_data$OFFENSE[crime_data$OFFENSE=="ASSAULT W/DANGEROUS WEAPON"] <- "ASSAULT"
crime_data$OFFENSE[crime_data$OFFENSE=="MOTOR VEHICLE THEFT"] <- "CAR THEFT"

crime_data$OFFENSE <- as.factor(crime_data$OFFENSE)

crime_data$weeknum <- as.numeric( format(crime_data$REPORT_DAT, "%U"))

daily_crime <- crime_data %>% group_by(REPORT_DAT,OFFENSE) %>% summarise(daily_crime=n())
weekly_crime <- crime_data %>% group_by(weeknum,OFFENSE) %>% summarise(weekly_crime=n())

daily_crime$REPORT_DAT <- as.Date(daily_crime$REPORT_DAT,format="%m/%d/%Y")

ggplot(daily_crime, aes(x=REPORT_DAT,y=daily_crime,color=OFFENSE)) + 
        geom_point() + geom_smooth(method='loess')

ggplot(weekly_crime, aes(x=weeknum,y=weekly_crime,color=OFFENSE)) + 
  geom_line(size=1) + geom_smooth(method='loess')

#########################
##Crimes by ward
#########################

stats_by_ward <- crime_data %>% group_by(REPORT_DAT,WARD,OFFENSE)  %>% summarise(daily_crime=n())

##Crimes across Ward
ggplot(stats_by_ward,aes(x=stats_by_ward$WARD,y=stats_by_ward$daily_crime,color=stats_by_ward$WARD)) + geom_bar(stat="identity") + facet_wrap( ~ stats_by_ward$OFFENSE) 

##Crimes within Ward
ggplot(stats_by_ward,aes(x=stats_by_ward$OFFENSE,y=stats_by_ward$daily_crime,color=stats_by_ward$OFFENSE)) + geom_bar(stat="identity") + facet_wrap( ~ stats_by_ward$WARD) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="none")

murder_rate <-  stats_by_ward[stats_by_ward$OFFENSE %in% c('HOMICIDE') ,]

murder_rate$WARD <- as.factor(murder_rate$WARD)

ggplot(murder_rate, aes(x=murder_rate$REPORT_DAT,y=murder_rate$daily_crime,color=murder_rate$WARD)) + geom_point() 

#########################
##Crimes by time of day
#########################

stats_by_time <- crime_data %>% group_by(SHIFT,OFFENSE) %>% summarise(crime=n())

ggplot(stats_by_time,aes(x=OFFENSE,y=crime,color=OFFENSE)) + geom_bar(stat="identity") + facet_wrap( ~ SHIFT)
  