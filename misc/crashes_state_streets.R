library(tidyverse)
library(sf)


states <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho",
            "Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
            "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina",
            "North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas",
            "Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")

centroids <- read.csv("../data/transportation/Block_Centroids.csv")

all_states <- data.frame()

for(state_1 in states){
  print(paste0("Processing: ",state_1))
    for(state_2 in states){
      temp_df <- centroids %>%
        filter(grepl(toupper(state_1),ONSTREETDISPLAY) & grepl(toupper(state_2),FROMSTREETDISPLAY))
      
      all_states <- rbind(all_states,temp_df)
    }
}

crashes_data <- read.csv("../data/transportation/Crashes_in_DC.csv")

test <- head(crashes_data,1000)

crashes_state_streets <- geo_left_join(test,all_states,by=c("X","Y"),max_dist=.1,unit="miles")

