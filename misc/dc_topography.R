library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(viridis)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

shp_file <- read_sf("https://opendata.arcgis.com/datasets/f3d36543cc81416791e96b95fc23e6c2_4.geojson")

water <- read_sf("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Planimetrics_2015/MapServer/14/query?where=1%3D1&outFields=*&outSR=4326&f=json")

plot <- ggplot(data = shp_file,aes(color=ELEVATION)) + 
  geom_sf()  + 
  geom_sf(data=water, fill="lightgrey", color="black") +
  theme_map() +
  theme(legend.position = c(.9, .85),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14),
        legend.key.size = unit(1.5, 'cm'),
        plot.caption = element_text(size=14)) +
  labs(caption = "DC Topography - 20 Foot Contours | August Warren | gwarrenn.github.io",
       color="Elevation (feet)") +
  scale_color_viridis(option="magma")

ggsave(plot = plot, "/users/augustwarren/Desktop/DC.png", w = 16, h = 16)

