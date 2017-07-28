library("reshape2")
library("ggplot2")
library("directlabels")
library("magick")
library("readr")
library("magrittr")
library('devtools')
library("plyr")
library("gganimate")
library("magick")

data <- read.csv(file="C:\\Users\\augus\\OneDrive\\Documents\\GitHub\\dc_data\\data\\real_estate\\Zip_Zhvi_DC.csv")

long <- melt(data, id.vars= c("RegionID","RegionName","City",
                              "State","Metro","CountyName","SizeRank"))

long$variable <- gsub("X", "", long$variable)
long$variable <- gsub("\\.", "/", long$variable)
long$variable <- gsub("$", "/01", long$variable)

long$variable <- as.Date(long$variable, "%Y/%m/%d")
long$RegionName <- as.character(long$RegionName)

long$year <- sub("-.*", "", long$variable)

plot <- ggplot(long, aes(x=long$variable,y=value,colour=RegionName,label=RegionName)) + geom_line(size=1) + 
  ggtitle("DC Average Housing Prices by Zip Code 1996-2016") + labs(x = "Year",y="Average Housing Price")
theme(plot.title = element_text(size = 16))

direct.label(plot)

plot_ani <- ggplot(long, aes(x=long$variable,y=value,colour=RegionName,label=RegionName,frame=year)) + geom_line(size=1,aes(cumulative = TRUE)) + 
  ggtitle("DC Average Housing Prices by Zip Code 1996-2016") + labs(x = "Year",y="Average Housing Price")
theme(plot.title = element_text(size = 16))

gganimate(plot_ani, "output_trend.gif",interval = .8,ani.width=800, ani.height=600)


print(plot)

all_data <- data.frame(RegionName=character(), 
                       year1=integer(), 
                       year2=integer(),
                       date1=as.Date(character()),
                       date2=as.Date(character()),
                       stringsAsFactors=FALSE) 

previous_date = "X2000.01"

for(i in 2000:2016) { 
  year <- paste("X", i, sep = "")
  print(year)
  for(m in 1:12) {
    if(m < 10) {
      year_month <- paste(year,".0", m, sep = "")
    }  
    else {
      year_month <- paste(year,".", m, sep = "")
    }  
    
    myvars <- c("RegionName",previous_date,year_month)    
    trimmed_data <- data[myvars]
    
    trimmed_data$date1 <- previous_date
    trimmed_data$date1 <- gsub("X", "", trimmed_data$date1)
    trimmed_data$date1 <- gsub("\\.", "/", trimmed_data$date1)
    trimmed_data$date1 <- gsub("$", "/01", trimmed_data$date1)
    trimmed_data$date1 <- gsub("_", "/", trimmed_data$date1)
    trimmed_data$date1 <- as.Date(trimmed_data$date1, "%Y/%m/%d")
    
    trimmed_data$date2 <- year_month 
    trimmed_data$date2 <- gsub("X", "", trimmed_data$date2)
    trimmed_data$date2 <- gsub("\\.", "/", trimmed_data$date2)
    trimmed_data$date2 <- gsub("$", "/01", trimmed_data$date2)
    trimmed_data$date2 <- gsub("_", "/", trimmed_data$date2)
    trimmed_data$date2 <- as.Date(trimmed_data$date2, "%Y/%m/%d")
    
    names(trimmed_data)[names(trimmed_data) == previous_date ] <- 'year1'
    
    if(year_month=='X2000.01') { 
      var_label <- paste(year_month,".1", sep = "") 
    } else { 
      var_label <- year_month 
    }
    
    names(trimmed_data)[names(trimmed_data) == var_label] <- 'year2'
    
    trimmed_data$RegionName <- as.character(trimmed_data$RegionName)
    
    all_data <- rbind(all_data,trimmed_data)
    
    previous_date = year_month
    
  }
}  

Sys.setenv(PATH = paste("C:\\Program Files\\ImageMagick-7.0.6-Q16",Sys.getenv("PATH"), sep = ";"))

p1 <- ggplot(all_data, aes(year2, year1, color = RegionName, frame = date1)) +
  geom_point(size=5) + expand_limits(y=0,x=0) + labs(x = all_data$date2,y=all_data$date1)

gganimate(p1, "output.gif",interval = .8)

p2 <- ggplot(all_data, aes(x=RegionName, y=year1, fill = RegionName, frame = date1)) +
  geom_bar(stat="identity",position = "identity") +
  geom_text(aes(label=all_data$year1), vjust=1.5, position=position_dodge(.5), size=4)

gganimate(p2, "output_bars.gif",interval = .8,ani.width=800, ani.height=600)


