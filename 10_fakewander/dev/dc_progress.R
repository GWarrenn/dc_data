library("tcxr")
library("tidyverse")
library("sf")
library("FITfileR")
library("trackeR")
library("R.utils")
library("parallel")

##################################################
##
## plotting
##
##################################################

centroids_w_stats <- read.csv('geocoded_results_20251103.csv')
streets_df <- st_read("Street_Centerlines_1999.geojson") %>%
  select(OBJECTID,geometry)

blocks_ridden <- centroids_w_stats %>%
  filter(!is.na(file_name))

world_map <- st_as_sf(maps::map("usa", fill = TRUE, plot = FALSE))

overall_summary <- centroids_w_stats %>% summarise(blocks_ridden = sum(!is.na(file_name)),
                                                   total_blocks = n()) 

overall_summary$pct <- round((overall_summary$blocks_ridden / overall_summary$total_blocks) * 100,2)

blocks_ridden <- left_join(x=blocks_ridden,y=streets_df,by='OBJECTID')

map_plot <- ggplot(world_map) +
  #theme_minimal() +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  geom_sf(data = blocks_ridden, fill = "black",aes(geometry=geometry,color=file_name)) +
  coord_sf(crs = 4326) +
  coord_sf(
    xlim = c(-77.119759,-76.909395),
    ylim = c(38.791645, 38.99511)
  ) + 
  scale_color_viridis_c(option = "plasma",trans = "sqrt") + 
  labs(title = "DC Biking Coverage Summary",
  subtitle = paste0("Total blocks biked: ",overall_summary$blocks_ridden[1]," | ",overall_summary$pct[1],"% of all blocks"),
  caption = "Data from Strava matched to DC block centroids")

ggsave("map_plot.png", plot = map_plot, dpi = 600)

## now calculate % coverage!

## by ward

ward_summary <- centroids_w_stats %>% 
                      group_by(WARD) %>%
                      summarise(blocks_ridden = sum(!is.na(file_name)),
                                                   total_blocks = n())  %>%
  mutate(pct = blocks_ridden / total_blocks)


## by quadrant

quad_summary <- centroids_w_stats %>% 
  group_by(FROMSTQUAD) %>%
  summarise(blocks_ridden = sum(!is.na(count)),
            total_blocks = n())  %>%
  mutate(pct = blocks_ridden / total_blocks)


##################################################
##
## OLD PROCESSING: get list of rides
##
##################################################

export_filename <- "export_4778598"

activities <- read.csv(paste0(export_filename,"/activities.csv"))

activities <- activities %>%
  mutate(activity_date = as.POSIXct(activities$Activity.Date,format="%b %d, %Y, %H:%M:%S %p")) %>%
  filter(Activity.Type == "Ride")

files <- activities$Filename

centroids <- read.csv("Block_Centroids.csv") ## https://datahub-dc-dcgis.hub.arcgis.com/datasets/block-centroids/explore
df2_sf <- st_as_sf(centroids, coords = c("X", "Y"), crs = 4326)

## unzipping files

zip_files <- list.files(path = paste0(export_filename,"/activities/"), pattern = "\\.gz$", full.names = TRUE)

for (file in zip_files) {
  gunzip(file,overwrite=TRUE)
}

files <- list.files(path = paste0(export_filename,"/activities/"), pattern = "\\.", full.names = TRUE)

##################################################
##
## Classification function to parallelize across cores
##
##################################################

classify_streets <- function(file){
  
  label <- activities %>% filter(Filename == file) %>% select(Activity.Name)
  original_file <- file
  activity_date <- activities %>% filter(Filename == file) %>% select(Activity.Date)
  
  ## GPX file processing
  
  if(grepl("gpx|tcx", file)){
    
    if(grepl("gpx", file, fixed = TRUE)){
      
      #route <- plotKML::readGPX(gpx.file = paste0("export_4778598/",file))
      route <- st_read(file, layer = "tracks") 
      
      route_df <- as.data.frame(route$geometry[[1]][[1]])
      names(route_df) <- c("lon","lat")
      
    }
    
    else if(grepl("tcx", file, fixed = TRUE)){
      
      file <- sub(pattern = "tcx.gz",replacement = "tcx",x = file)
      
      my_txt <- readLines(file) 
      my_txt[1] = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
      
      fileConn <- file(gsub("activities","activities/fixed_tcx",file))
      writeLines(my_txt, fileConn)
      close(fileConn)
      
      route <- readTCX(gsub("activities","activities/fixed_tcx",file))
      
      route_df <- route %>%
        arrange(time) %>%
        select(time,latitude,longitude) %>%
        rename(lat = latitude,
               lon = longitude) %>%
        filter(!is.na(lat))
      
    } 
  } else {
    
    file <- sub(pattern = "fit.gz",replacement = "fit",x = file)
    
    route <- readFitFile(file)
    
    route_df <- records(route) %>% 
      bind_rows() %>% 
      arrange(timestamp) %>%
      select(timestamp,position_lat,position_long) %>%
      rename(lat = position_lat,
             lon = position_long) %>%
      filter(!is.na(lat))
  }
  
  ## matching ride segments to block centroids
  
  if(is.null(route_df)){
    message(paste0("Issue with ",file))
  }
  
  df1_sf <- st_as_sf(route_df, coords = c("lon", "lat"), crs = 4326)
  
  df_near <- st_join(df1_sf, df2_sf, join = st_nearest_feature)
  
  ride_summary <- as.data.frame(df_near) 
  
  if(nrow(ride_summary) <= 1){
    ride_summary <- data.frame()
  }
  
  else{
    ride_summary$file <- original_file
    ride_summary <- ride_summary %>% select(file,WARD,BLOCKNAME,ONSTQUAD,MARID,STREETSEGID) %>% unique()
  }
}

all_data <- data.frame()

for(file in files){
  if(file != "activities/2376093165.json"){
    tryCatch(
      expr = {
        classified_results <- classify_streets(file)
        all_data <- bind_rows(all_data, classified_results)
      },
      error = function(e){
        print(paste0("Error with ",file))
      })
  }
}

res <- classify_streets("export_4778598/activities/10798121445.fit")

processed_list <- all_data$file
setdiff(files,processed_list)

#classified_results <- mclapply(new_files, classify_streets, mc.cores=6)
#all_data <- do.call("rbind", classified_results)

stats <- all_data %>% group_by(MARID,BLOCKNAME) %>% summarise(count=n())

centroids_w_stats <- merge(x = centroids, y = stats, by = "MARID", all.x = TRUE)

## export results for future analysis

write.csv(all_data,'geocoded_results_20251031.csv',row.names = FALSE)
