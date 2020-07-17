library(tidyverse)
library(jsonlite)
library(zoo)
library(gridExtra)
library(grid)
library(scales)
library(mapdeck)

crime_df <- read.csv("data/dc-crimes-search-results_20200623.csv")

crime_df$month <- as.numeric(format(as.Date(crime_df$REPORT_DAT,"%Y-%m-%d"), "%m"))
crime_df$Year <- format(as.Date(crime_df$REPORT_DAT,"%Y-%m-%d"), "%Y")
crime_df$year_month <- as.Date(paste0(crime_df$month,"/","01/",as.numeric(crime_df$Year),sep=""),"%m/%d/%Y")

crime_df$day <- as.Date(crime_df$REPORT_DAT,"%Y-%m-%d")
crime_df$day_no_year <- format(as.Date(crime_df$REPORT_DAT,"%Y-%m-%d"),"%m/%d")

crime_df <- crime_df %>%
  filter(Year != "2017" & month < 7)

crime_df$violent_crime = ifelse(crime_df$OFFENSE == "homicide" |
                                  crime_df$OFFENSE == "sex abuse" | 
                                  crime_df$OFFENSE == "assault w/dangerous weapon" |
                                  crime_df$OFFENSE == "robbery",1,0)

crime_df$nonviolent_crime = ifelse(crime_df$violent_crime == 0,1,0)

#############################################################
##
## TREND DATA
##
#############################################################

crime_by_day <- crime_df %>%
  group_by(Year,day) %>%
  arrange(day) %>%
  summarise(num_violent_crimes = sum(violent_crime),
            num_nonviol_crimes = sum(nonviolent_crime)) %>%
  mutate(violent_crimes = cumsum(num_violent_crimes),
         nonviolent_crimes = cumsum(num_nonviol_crimes),
         moving_avg_violent = rollapply(num_violent_crimes,14,mean,align='right',fill=NA),
         moving_avg_nonviolent = rollapply(num_nonviol_crimes,14,mean,align='right',fill=NA))

crime_by_day <- crime_by_day %>%
  filter(Year != "2018")

p2_violent <- ggplot(crime_by_day,aes(x=as.Date(paste(2020,strftime(day,format="%m-%d"),sep="-")),
                        y=moving_avg_violent,
                        color=as.character(Year),
                        group=Year)) + 
  geom_line(size=1) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-25","%Y-%m-%d"),linetype='dashed') +
  labs(x="Date",
       y="Average Number of Violent Crimes",
       title="Violent Crimes",
       color="Year") +
  theme_bw() +
  theme(legend.position = "bottom") 

p2_nonviolent <- ggplot(crime_by_day,aes(x=as.Date(paste(2020,strftime(day,format="%m-%d"),sep="-")),
                              y=moving_avg_nonviolent,
                              color=as.character(Year),
                              group=Year)) + 
  geom_line(size=1) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-25","%Y-%m-%d"),linetype='dashed') +
  labs(x="Date",
       y="Average Number of Non-violent Crimes",
       title="Non-violent Crimes",
       color="Year") +
  theme_bw() +
  theme(legend.position = "bottom") 

change_plots <- grid.arrange(p2_violent,p2_nonviolent,ncol=2,
                             bottom = textGrob("Source: Washington MPD Crime Incident Data | Viz: August Warren (gwarrenn.github.io) | Stay At Home Order issued April 1st",
                                               x = 0,
                                               y = 0.5,
                                               just = "left",
                                               gp = gpar(fontsize = 8)),
                             top = textGrob("Average Number of Violent & Non-Violent Crimes per 14 Days in Washington DC",
                                      x = 0.03, 
                                      y = 0.5, 
                                      just = "left", 
                                      gp = gpar(fontsize = 18)
                             ))

ggsave(plot = change_plots, "images/covid_avg_crime_20200623.png", w = 12, h = 6,type = "cairo-png")

#############################################################
##
## CHANGES BY WARD
##
#############################################################

violent_crime_by_ward <- crime_df %>%
  mutate(stay_at_home = if_else(month<4,"Pre-Stay At Home Order",
                                if_else(month >= 6,"Post-George Floyd Murder","Post-Stay At Home Order"))) %>%
  group_by(WARD,stay_at_home,Year,day_no_year) %>%
  arrange(day_no_year) %>%
  summarise(num_violent_crimes = sum(violent_crime),
            num_nonviol_crimes = sum(nonviolent_crime)) %>%
  mutate(violent_crimes = cumsum(num_violent_crimes),
         nonviolent_crimes = cumsum(num_nonviol_crimes),
         moving_avg_violent = rollapply(num_violent_crimes,14,mean,align='right',fill=NA),
         moving_avg_nonviolent = rollapply(num_nonviol_crimes,14,mean,align='right',fill=NA)) %>%
  select(day_no_year,Year,moving_avg_violent) %>%
  spread(Year, moving_avg_violent, fill = NA, convert = FALSE) %>%
  mutate(avg_change = `2020` - `2019`) %>%
  group_by(WARD,stay_at_home) %>%
  summarise(avg_change = mean(avg_change,na.rm = T)) %>%
  mutate(crime_type = "Violent Crimes")

nonviolent_crime_by_ward <- crime_df %>%
  mutate(stay_at_home = if_else(month<4,"Pre-Stay At Home Order",
                                if_else(month >= 6,"Post-George Floyd Murder","Post-Stay At Home Order"))) %>%
  group_by(WARD,stay_at_home,Year,day_no_year) %>%
  arrange(day_no_year) %>%
  summarise(num_violent_crimes = sum(violent_crime),
            num_nonviol_crimes = sum(nonviolent_crime)) %>%
  mutate(violent_crimes = cumsum(num_violent_crimes),
         nonviolent_crimes = cumsum(num_nonviol_crimes),
         moving_avg_violent = rollapply(num_violent_crimes,14,mean,align='right',fill=NA),
         moving_avg_nonviolent = rollapply(num_nonviol_crimes,14,mean,align='right',fill=NA)) %>%
  select(day_no_year,Year,moving_avg_nonviolent) %>%
  spread(Year, moving_avg_nonviolent, fill = NA, convert = FALSE) %>%
  mutate(avg_change = `2020` - `2019`) %>%
  group_by(WARD,stay_at_home) %>%
  summarise(avg_change = mean(avg_change,na.rm = T)) %>%
  mutate(crime_type = "Non-Violent Crimes")

combined_crimes_by_ward <- rbind(violent_crime_by_ward,nonviolent_crime_by_ward)

violent_crime_by_ward$stay_at_home <- factor(violent_crime_by_ward$stay_at_home,levels = c("Pre-Stay At Home Order","Post-Stay At Home Order","Post-George Floyd Murder"))
violent_crime_by_ward$ward_lab <- paste("Ward",violent_crime_by_ward$WARD)

ward_violent <- ggplot(violent_crime_by_ward,aes(x=stay_at_home,y=avg_change,fill=avg_change)) +
  geom_bar(stat="identity",color="black") +
  geom_text(aes(x=stay_at_home,y=avg_change+(.1*sign(avg_change)),label=round(avg_change,2)),size=3) +
  scale_fill_gradientn(colours = c("red","white","#1a9641"), 
                       values = rescale(c(-1.5,0,1)),
                       guide = "colorbar", limits=c(-1.5,1.3)) +
  facet_wrap(~ward_lab) +
  geom_hline(yintercept = 0) +
  labs(x="Pre/Post Stay at Home Order",
       y="Average Change in Crime (per 14 Days)",
       title="Violent Crime -- 2019 vs. 2020",
       fill="Average Change") +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = function(grouping) str_wrap(grouping, width = 10))

nonviolent_crime_by_ward$stay_at_home <- factor(nonviolent_crime_by_ward$stay_at_home,levels = c("Pre-Stay At Home Order","Post-Stay At Home Order","Post-George Floyd Murder"))
nonviolent_crime_by_ward$ward_lab <- paste("Ward",nonviolent_crime_by_ward$WARD)

ward_nonviolent <- ggplot(nonviolent_crime_by_ward,aes(x=stay_at_home,y=avg_change,fill=avg_change)) +
  geom_bar(stat="identity",color="black") +
  geom_text(aes(x=stay_at_home,y=avg_change+(.9*sign(avg_change)),label=round(avg_change,2)),size=3) +
  scale_fill_gradientn(colours = c("red","white","#1a9641"), 
                       values = rescale(c(-11,0,5)),
                       guide = "colorbar", limits=c(-11,5)) +
  facet_wrap(~ward_lab) +
  geom_hline(yintercept = 0) +
  labs(x="Pre/Post Stay at Home Order",
       y="Average Change in Crime (per 14 Days)",
       title="Non-Violent Crime -- 2019 vs. 2020",
       fill="Average Change") +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = function(grouping) str_wrap(grouping, width = 10))

change_ward_plots <- grid.arrange(ward_violent,ward_nonviolent,ncol=2,
                             bottom = textGrob("Source: Washington MPD Crime Incident Data | Viz: August Warren (gwarrenn.github.io) | Stay At Home Order issued April 1st",
                                               x = 0,
                                               y = 0.5,
                                               just = "left",
                                               gp = gpar(fontsize = 8)),
                             top = textGrob("Average Change Violent & Non-Violent Crimes per 14 Days in Washington DC by Ward",
                                            x = 0.03, 
                                            y = 0.5, 
                                            just = "left", 
                                            gp = gpar(fontsize = 18)
                             ))

ggsave(plot = change_ward_plots, "images/change_ward_plots_20200623.png", w = 12, h = 6,type = "cairo-png")

## Type of crime 

crime_counts <- crime_df %>%
  mutate(stay_at_home = if_else(month<4,"Pre-Stay At Home Order",
                                if_else(month >= 6,"Post-George Floyd Murder","Post-Stay At Home Order"))) %>%
  group_by(stay_at_home,OFFENSE) %>%
  summarise(n=n()) %>%
  mutate(pct=n/sum(n))


ggplot(crime_counts,aes(x=OFFENSE,y=pct)) +
  geom_bar(stat="identity") +
  geom_text(aes(x=OFFENSE,y=pct,label=percent(pct)),vjust=-.5) +
  facet_grid(stay_at_home~.)

#############################################################
##
## MAPPING
##
#############################################################

key <- ""

pre_covid <- crime_df %>%
  filter(YEAR == "2020" & month < 4) %>%
  mutate(weight = 1)

mapdeck(token = key, style = mapdeck_style('dark'), pitch = 0) %>%
  add_hexagon(
    data = pre_covid
    , radius = 300
    , lat = "LATITUDE"
    , lon = "LONGITUDE"
    , elevation_scale = 0
    , colour_range = colourvalues::colour_values(1:6, palette = colourvalues::get_palette("viridis")[70:256,])
  )

post_covid_pre_gf <- crime_df %>%
  filter(YEAR == "2020" & month >= 4 & month <=5) %>%
  mutate(weight = 1)

mapdeck(token = key, style = mapdeck_style('dark'), pitch = 0) %>%
  add_hexagon(
    data = post_covid_pre_gf
    , radius = 300
    , lat = "LATITUDE"
    , lon = "LONGITUDE"
    , elevation_scale = 0
    , colour_range = colourvalues::colour_values(1:6, palette = colourvalues::get_palette("viridis")[70:256,])
  )

post_covid_pre_gf <- crime_df %>%
  filter(YEAR == "2020" & month >= 4) %>%
  mutate(weight = 1)

mapdeck(token = key, style = mapdeck_style('dark'), pitch = 0) %>%
  add_hexagon(
    data = post_covid_pre_gf
    , radius = 300
    , lat = "LATITUDE"
    , lon = "LONGITUDE"
    , elevation_scale = 0
    , colour_range = colourvalues::colour_values(1:6, palette = colourvalues::get_palette("viridis")[70:256,])
  )

post_gf <- crime_df %>%
  filter(YEAR == "2020" & month >= 6) %>%
  mutate(weight = 1)

mapdeck(token = key, style = mapdeck_style('dark'), pitch = 0) %>%
  add_hexagon(
    data = post_gf
    , radius = 300
    , lat = "LATITUDE"
    , lon = "LONGITUDE"
    , elevation_scale = 0
    , colour_range = colourvalues::colour_values(1:6, palette = colourvalues::get_palette("viridis")[70:256,])
  )


