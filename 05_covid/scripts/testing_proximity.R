library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(hereR)

set_key("YjqgEM6_66AW5ivQHcOb0JY2evTJajiJtjDt9igeq6o")

####################################################
##
## Census Tract Geo & Population data
##
####################################################

api_data = GET("https://opendata.arcgis.com/datasets/faea4d66e7134e57bf8566197f25b3a8_0.geojson")

api_data_json <- fromJSON(rawToChar(api_data$content))
census_df <- api_data_json$features$properties

census_geo <- api_data_json$features

census_shp <- read_sf("https://opendata.arcgis.com/datasets/faea4d66e7134e57bf8566197f25b3a8_0.geojson")

centroids <- census_shp %>% 
  st_centroid() %>% 
  bind_cols(as_data_frame(st_coordinates(.)))    # unpack points to lat/lon columns

####################################################
##
## Testing location data
##
####################################################

testing_locations <- read.csv("05_covid/data/testing_locations.csv") 

## some quick geocoding

locs <- geocode(address = testing_locations$Address)

## determine walking time/distance to each testing point -- find closest location

run_geocode = FALSE

if (run_geocode == TRUE) {
  
  n <- 1
  travel_df <- data.frame()
  
  for (i in c(1:nrow(centroids))){
    for (z in c(1:nrow(testing_locations))) {
      
      origin <- st_as_sf(centroids$geometry[i])
      destination <- st_as_sf(locs$geometry[z])
      
      travel_time_df <- route(
        origin = origin,
        destination = destination,
        mode = "car"
      )
      
      travel_time_df$testing_site <- testing_locations$Site[z]
      travel_time_df$tract_name <- centroids$NAME[i]
      travel_time_df$population <- centroids$B01001_001E[i]
      
      travel_df <- rbind(travel_df,travel_time_df)
    }  
  }
}

## exporting results to save API query credits

write_csv(x = travel_df,path = "05_covid/data/testing_locations_tracts_geocoded_ped.csv")
write_csv(x = travel_df,path = "05_covid/data/testing_locations_tracts_geocoded_car.csv")

travel_df <- read.csv("05_covid/data/testing_locations_tracts_geocoded_car.csv")

travel_df_min <- travel_df %>%
  group_by(tract_name) %>%
  mutate(min_travel_time = min(travelTime),
         min_distance = min(distance),
         closest_shortest = (min_travel_time == travelTime) & (min_distance == distance),
         travel_mins = travelTime / 60) %>%
  filter(min_distance == distance)

ggplot(travel_df_min) + 
  geom_sf(data = census_shp,color=alpha("black",0.2)) +
  geom_sf(aes(color = travelTime)) + 
  geom_sf(data = locs, size = 4, shape = 23, fill = "red") +
  geom_sf(data = centroids) +
  theme(axis.text = element_blank())

travel_df_min$travel_bins <- cut(travel_df_min$travel_mins,breaks = c(0,5,10,30,60,90,Inf))

travel_stats <- travel_df_min %>%
  group_by(travel_bins) %>%
  summarise(total_people = sum(population)) %>%
  mutate(total_dc_pop = sum(total_people),
         pct_pop = total_people/total_dc_pop)

#########################################
##
## SES Indicator Variables
##
#########################################

api_data = GET("https://opendata.arcgis.com/datasets/65a03f993fdc4443af6fe94bd8ba9143_0.geojson")

api_data_json <- fromJSON(rawToChar(api_data$content))

tract_poverty_df <- api_data_json$features$properties %>%
  select(NAME,B17020_001E,B17020_002E) %>%
  mutate(poverty_rate = B17020_002E / B17020_001E)

travel_df_min_ses <- merge(travel_df_min,tract_poverty_df,
                           by.x="tract_name",by.y="NAME")

ggplot(travel_df_min_ses,aes(x=travel_mins,poverty_rate)) + 
  geom_point() +
  geom_smooth(method = "lm") 
  