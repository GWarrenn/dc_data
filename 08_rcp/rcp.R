library("plotKML")
library("tidyverse")
library("rgdal")
library("sf")
library("FITfileR")
library("trackeR")

library("parallel")

##################################################
##
## RCP shapefile for intersection
## source: https://opendata.dc.gov/datasets/parks-and-recreation-areas
##
##################################################

all_national_parks <- readOGR("dc_data/data/National_Parks/",
                              layer="National_Parks")

rcp_shp <- all_national_parks[all_national_parks$NAME == "Rock Creek Park & Piney Branch Parkway" & !is.na(all_national_parks$NAME), ]
#rcp_shp <- rcp_shp[rcp_shp$GIS_ID == "Nps_545" & !is.na(rcp_shp$NAME), ]

rcp_shp_plot <- spTransform(rcp_shp, CRSobj = "+proj=utm +zone=50 +north +ellps=WGS84") %>%
  st_as_sf(rcp_shp_plot)

##################################################
##
## get list of rides during pandemic
## using april 11 as start of Beach Drive closure: https://coronavirus.dc.gov/release/mayor-bowser-and-national-park-service-announce-beach-drive-rock-creek-park-and-roads-fort
##
##################################################

activities <- read.csv("dc_data/08_rcp/export_4778598/activities.csv")

activities <- activities %>%
  mutate(activity_date = as.POSIXct(activities$Activity.Date,format="%b %d, %Y, %H:%M:%S %p")) %>%
  filter(Activity.Type == "Ride" & activity_date >= as.Date("2019-03-01",format="%Y-%m-%d"))
  
files <- activities$Filename

##################################################
##
## Classification function to parallelize across cores
##
##################################################

classify_rcp <- function(file){
  
  total_time_secs <- 0
  total_weekend_time <- 0
  total_weekday_time <- 0 
  
  label <- activities %>% filter(Filename == file) %>% select(Activity.Name)
  
  ## GPX file processing
  
  if(grepl("gpx", file)){ #|fit|tcx
    
    print(paste("Processing:",label$Activity.Name[1]))
    
    total_time_in_rcp <- 0
    
    print(paste0("Processing: dc_data/08_rcp/export_4778598/",file))
    
    if(grepl("gpx", file, fixed = TRUE)){
      
      route <- plotKML::readGPX(gpx.file = paste0("dc_data/08_rcp/export_4778598/",file))
      
      route_df <- as.data.frame(route$tracks[[1]][[1]])
      
    }
    
    else if(grepl("tcx$", file, fixed = TRUE)){
      
      file <- sub(pattern = "tcx.gz",replacement = "tcx",x = file)
      
      route <- readTCX(paste0("dc_data/08_rcp/export_4778598/",file))
      
      route_df <- route %>%
        arrange(time) %>%
        select(time,latitude,longitude) %>%
        rename(lat = latitude,
               lon = longitude) %>%
        filter(!is.na(lat))
      
    } 
    
    ## FIT File processing
    
    else {
      
      file <- sub(pattern = "fit.gz",replacement = "fit",x = file)
      
      route <- readFitFile(paste0("dc_data/08_rcp/export_4778598/",file))
      
      route_df <- records(route) %>% 
        bind_rows() %>% 
        arrange(timestamp) %>%
        select(timestamp,position_lat,position_long) %>%
        rename(lat = position_lat,
               lon = position_long) %>%
        filter(!is.na(lat))
    }
    
    ## route file + RCP shape file intersection
    
    coordinates(route_df) <- ~ lon + lat 
    
    proj4string(route_df) <- proj4string(rcp_shp)
    
    rcp_shp_sf <- st_as_sf(rcp_shp)
    route_df_sf <- st_as_sf(route_df)
    
    intersection <- st_intersection(x = rcp_shp_sf, y = route_df_sf)
    
    if(grepl("gpx", file, fixed = TRUE)){
      intersection$time_fmt <- as.POSIXct(intersection$time,format="%Y-%m-%dT%H:%M:%S")
    }
    else if(grepl("tcx", file, fixed = TRUE)){
      intersection$time_fmt <- as.POSIXct(intersection$time,format="%Y-%m-%d %H:%M:%S")
    }    
    else{
      intersection$time_fmt <- as.POSIXct(intersection$timestamp,format="%Y-%m-%d %H:%M:%S")
    }
    
    ## running time calculation of time spent in RCP
    
    intersection <- intersection %>%
      mutate(time_diff = as.numeric(time_fmt-lag(time_fmt), units = 'secs'))
    
    total_time_in_rcp <- sum(intersection$time_diff,na.rm = T)
    
    print(paste0("Total time in RCP: ",as.character(total_time_in_rcp)," seconds"))
    
    #total_time_secs <- total_time_in_rcp + total_time_secs
    
    if(weekdays(as.Date(intersection$time_fmt[1],format="%Y-%m-%d")) %in% c("Saturday","Sunday")){
      total_weekend_time <- total_weekend_time + total_time_in_rcp
    }
    else{
      total_weekday_time <- total_weekday_time + total_time_in_rcp
    }
    
    ride_df <- data.frame(ride_date = intersection$time_fmt[1],
                          ride_dow = weekdays(as.Date(intersection$time_fmt[1],format="%Y-%m-%d")),
                          total_time_in_rcp = total_time_in_rcp)
  }
}

classified_results <- mclapply(files, classify_rcp, mc.cores=8)

all_data <- do.call("rbind", classified_results)

all_data <- all_data %>% filter(!is.na(ride_date))

all_data$ride_dow_fixed <- weekdays(as.Date(all_data$ride_date,format="%Y-%m-%d"))

bar_plot_data <- all_data %>%
  mutate(year = if_else(ride_date < as.Date("2020-04-11","%Y-%m-%d"),"2019/2020 (Pre-Closure)","2020/2021 (Post-Closure)")) %>%
  group_by(year,ride_dow_fixed) %>%
  summarise(total_time = sum(total_time_in_rcp)) 

bar_plot_data <- bar_plot_data %>%  
  group_by(year) %>%
  complete(ride_dow_fixed) %>%
  ungroup() %>%
  mutate(total_time = if_else(is.na(total_time),0,total_time))

bar_plot_data$ride_dow_fixed <- factor(levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),x = bar_plot_data$ride_dow_fixed)

bar_plot <- ggplot(bar_plot_data,aes(x=ride_dow_fixed,y=(total_time/60/60),fill=year)) +
  geom_bar(stat = "identity",color="black",position = "dodge") +
  geom_text(aes(x=ride_dow_fixed,y=(total_time/60/60),label=round((total_time/60/60),2)),size=4,vjust=-.5,position = position_dodge(width = .9)) +
  labs(x="",y="Hours",title="Total Time Spent within RCP Boundaries (in Hours)",subtitle="During Pandemic: 4/11/20-Now",fill="Legend") +
  scale_fill_manual(values = c("#bcc7eb","#293352")) +
  theme(legend.position = "bottom")
  
ggsave(plot = bar_plot, "desktop/bar_plot.png", w = 10.67, h = 8)


