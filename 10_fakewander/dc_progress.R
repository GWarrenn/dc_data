library("tcxr")
library("tidyverse")
library("sf")
library("FITfileR")
library("trackeR")
library("R.utils")
library("parallel")

##################################################
##
## get list of rides
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
      route <- st_read(paste0(export_filename,"/",file), layer = "tracks") 
      
      route_df <- as.data.frame(route$geometry[[1]][[1]])
      names(route_df) <- c("lon","lat")
      
    }
    
    else if(grepl("tcx", file, fixed = TRUE)){
      
      file <- sub(pattern = "tcx.gz",replacement = "tcx",x = file)
      
      my_txt <- readLines(paste0("export_4778598/",file)) 
      my_txt[1] = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
      
      fileConn <- file(paste0(export_filename,"/",gsub("activities","activities/fixed_tcx",file)))
      writeLines(my_txt, fileConn)
      close(fileConn)
      
      route <- readTCX(paste0(export_filename,"/",gsub("activities","activities/fixed_tcx",file)))
      
      route_df <- route %>%
        arrange(time) %>%
        select(time,latitude,longitude) %>%
        rename(lat = latitude,
               lon = longitude) %>%
        filter(!is.na(lat))
      
    } 
  } else {
    
    file <- sub(pattern = "fit.gz",replacement = "fit",x = file)
    
    route <- readFitFile(paste0(export_filename,"/",file))
    
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
    if(!is.null(ride_summary)){
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

res <- classify_streets("activities")

processed_list <- all_data$file
setdiff(files,processed_list)

#classified_results <- mclapply(new_files, classify_streets, mc.cores=6)
#all_data <- do.call("rbind", classified_results)

stats <- all_data %>% group_by(MARID,BLOCKNAME) %>% summarise(count=n())

centroids_w_stats <- merge(x = centroids, y = stats, by = "MARID", all.x = TRUE)

all_data %>% filter(file == "activities/142761880.gpx")

## now calculate % coverage!

overall_summary <- centroids_w_stats %>% summarise(blocks_ridden = sum(!is.na(count)),
                                                     total_blocks = n()) 

overall_summary$pct <- overall_summary$blocks_ridden / overall_summary$total_blocks

## by ward

ward_summary <- centroids_w_stats %>% 
                      group_by(WARD) %>%
                      summarise(blocks_ridden = sum(!is.na(count)),
                                                   total_blocks = n())  %>%
  mutate(pct = blocks_ridden / total_blocks)


## by quadrant

quad_summary <- centroids_w_stats %>% 
  group_by(FROMSTQUAD) %>%
  summarise(blocks_ridden = sum(!is.na(count)),
            total_blocks = n())  %>%
  mutate(pct = blocks_ridden / total_blocks)

