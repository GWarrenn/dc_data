library(tidyverse)
library(jsonlite)
library(httr)
library(zoo)
library(gridExtra)
library(grid)
library(scales)
library(mapdeck)

api_data <- GET("https://opendata.arcgis.com/datasets/f516e0dd7b614b088ad781b0c4002331_2.geojson")

crime_2020 <- fromJSON(rawToChar(api_data$content))
crime_2020 <- crime_2020$features$properties

api_data <- GET("https://opendata.arcgis.com/datasets/f08294e5286141c293e9202fcd3e8b57_1.geojson")

crime_2019 <- fromJSON(rawToChar(api_data$content))
crime_2019 <- crime_2019$features$properties

api_data <- GET("https://opendata.arcgis.com/datasets/619c5bd17ca2411db0689bb0a211783c_3.geojson")

crime_2021 <- fromJSON(rawToChar(api_data$content))
crime_2021 <- crime_2021$features$properties

crime_df <- rbind(crime_2020,crime_2019,crime_2021)

crime_df$month <- as.numeric(format(as.POSIXct(crime_df$REPORT_DAT,format="%Y-%m-%dT%H:%M:%S"), "%m"))
crime_df$Year <- format(as.POSIXct(crime_df$REPORT_DAT,format="%Y-%m-%dT%H:%M:%S"), "%Y")
crime_df$day_numeric <- format(as.POSIXct(crime_df$REPORT_DAT,format="%Y-%m-%dT%H:%M:%S"), "%d")
crime_df$year_month <- as.Date(paste0(crime_df$month,"/","01/",as.numeric(crime_df$Year),sep=""),"%m/%d/%Y")

crime_df$day <- as.POSIXct(crime_df$REPORT_DAT,format="%Y-%m-%d")
crime_df$day_no_year <- format(as.POSIXct(crime_df$REPORT_DAT,format="%Y-%m-%dT%H:%M:%S"),"%m/%d")

crime_df <- crime_df %>%
  filter(Year != "2017")

crime_df$OFFENSE <- tolower(crime_df$OFFENSE)

crime_df$violent_crime <- ifelse(crime_df$OFFENSE == "homicide" |
                                  crime_df$OFFENSE == "sex abuse" | 
                                  crime_df$OFFENSE == "assault w/dangerous weapon" |
                                  crime_df$OFFENSE == "robbery",1,0)

crime_df$nonviolent_crime <- ifelse(crime_df$violent_crime == 0,1,0)

crime_df$homicide_assault <- ifelse(crime_df$OFFENSE == "homicide" |
                                      crime_df$OFFENSE == "assault w/dangerous weapon",1,0)

crime_df$robbery_sa <- ifelse(crime_df$OFFENSE == "robbery" |
                                      crime_df$OFFENSE == "sex abuse",1,0)

#############################################################
##
## TREND DATA
##
#############################################################

crime_by_day <- crime_df %>%
  group_by(Year,day) %>%
  arrange(day) %>%
  summarise(num_violent_crimes = sum(violent_crime),
            num_nonviol_crimes = sum(nonviolent_crime),
            num_homicide_assault = sum(homicide_assault),
            num_robbery_sa = sum(robbery_sa)) %>%
  mutate(violent_crimes = cumsum(num_violent_crimes),
         nonviolent_crimes = cumsum(num_nonviol_crimes),
         homicide_assault_cum = cumsum(num_homicide_assault),
         robbery_sa_cum = cumsum(num_robbery_sa),
         moving_avg_violent = rollapply(num_violent_crimes,14,mean,align='right',fill=NA),
         moving_avg_nonviolent = rollapply(num_nonviol_crimes,14,mean,align='right',fill=NA),
         moving_avg_homicide_assault = rollapply(num_homicide_assault,14,mean,align='right',fill=NA),
         moving_avg_robbery_sa = rollapply(num_robbery_sa,14,mean,align='right',fill=NA)) %>%
  filter(Year != "2018")

p2_violent <- ggplot(crime_by_day,aes(x=as.Date(paste(2020,strftime(day,format="%m-%d"),sep="-")),
                        y=moving_avg_violent,
                        color=as.character(Year),
                        group=Year)) + 
  geom_line(size=1) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-29","%Y-%m-%d"),linetype='dashed') +
  labs(x="Date",
       y="Average Number of Violent Crimes",
       title="Violent Crimes",
       color="Year") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "2 months",
               date_minor_breaks = "1 month",
               date_labels = "%b")

## WARD 1

wards <- crime_df %>%
  group_by(WARD,Year,day) %>%
  arrange(day) %>%
  summarise(num_violent_crimes = sum(violent_crime),
            num_nonviol_crimes = sum(nonviolent_crime)) %>%
  mutate(violent_crimes = cumsum(num_violent_crimes),
         nonviolent_crimes = cumsum(num_nonviol_crimes),
         moving_avg_violent = rollapply(num_violent_crimes,14,mean,align='right',fill=NA),
         moving_avg_nonviolent = rollapply(num_nonviol_crimes,14,mean,align='right',fill=NA)) %>%
  filter(Year != "2018")

violent_crime_by_ward <- ggplot(wards,aes(x=as.Date(paste(2020,strftime(day,format="%m-%d"),sep="-")),
                        y=moving_avg_violent,
                        color=as.character(Year),
                        group=Year)) + 
  geom_line(size=1) +
  facet_wrap(~WARD) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-29","%Y-%m-%d"),linetype='dashed') +
  labs(x="Date",
       y="Average Number of Violent Crimes",
       title="Average Number of Violent & Non-Violent Crimes per 14 Days in Washington DC",
       subtitle = "by Ward",
       color="Year",
       caption="Source: Washington MPD Crime Incident Data | Viz: August Warren (gwarrenn.github.io) | Stay At Home Order issued April 1st & Lifted May 29th") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "2 months",
               date_minor_breaks = "1 month",
               date_labels = "%b")

ggsave(plot = violent_crime_by_ward, "images/violent_crime_by_ward_20201115.png", w = 12, h = 6)

p2_nonviolent <- ggplot(crime_by_day,aes(x=as.Date(paste(2020,strftime(day,format="%m-%d"),sep="-")),
                              y=moving_avg_nonviolent,
                              color=as.character(Year),
                              group=Year)) + 
  geom_line(size=1) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-29","%Y-%m-%d"),linetype='dashed') +
  labs(x="Date",
       y="Average Number of Non-violent Crimes",
       title="Non-violent Crimes",
       color="Year") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "2 months",
               date_minor_breaks = "1 month",
               date_labels = "%b")

change_plots <- grid.arrange(p2_violent,p2_nonviolent,ncol=2,
                             bottom = textGrob("Source: Washington MPD Crime Incident Data | Viz: August Warren (gwarrenn.github.io) | Stay At Home Order issued April 1st & Lifted May 29th",
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

ggsave(plot = change_plots, "images/covid_avg_crime_20201115.png", w = 12, h = 6)

######################################################
##
## breaking up violent crimes
##
######################################################

p3_homicide <- ggplot(crime_by_day,aes(x=as.Date(paste(2020,strftime(day,format="%m-%d"),sep="-")),
                                      y=moving_avg_homicide_assault,
                                      color=as.character(Year),
                                      group=Year)) + 
  geom_line(size=1) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-29","%Y-%m-%d"),linetype='dashed') +
  labs(x="Date",
       y="Average Number of Homicides",
       title="Homicide & Assualt with a Deadly Weapon",
       color="Year") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "2 months",
               date_minor_breaks = "1 month",
               date_labels = "%b")

p4_robbery_sa <- ggplot(crime_by_day,aes(x=as.Date(paste(2020,strftime(day,format="%m-%d"),sep="-")),
                                       y=moving_avg_robbery_sa,
                                       color=as.character(Year),
                                       group=Year)) + 
  geom_line(size=1) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-29","%Y-%m-%d"),linetype='dashed') +
  labs(x="Date",
       y="Average Number of Robberies and Sexual Assualt",
       title="Robberies & Sexual Assualt",
       color="Year") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "2 months",
               date_minor_breaks = "1 month",
               date_labels = "%b")

violent_change_plots <- grid.arrange(p3_homicide,p4_robbery_sa,ncol=2,
                             bottom = textGrob("Source: Washington MPD Crime Incident Data | Viz: August Warren (gwarrenn.github.io) | Stay At Home Order issued April 1st & Lifted May 29th",
                                               x = 0,
                                               y = 0.5,
                                               just = "left",
                                               gp = gpar(fontsize = 8)),
                             top = textGrob("Average Number of Violent Crimes per 14 Days in Washington DC",
                                            x = 0.03, 
                                            y = 0.5, 
                                            just = "left", 
                                            gp = gpar(fontsize = 18)
                             ))

ggsave(plot = violent_change_plots, "images/violent_change_plots.png", w = 12, h = 6)

crime_df %>%
  group_by(Year) %>%
  summarise(total_hom = sum(homicide_assault),
            total_rob = sum(robbery_sa))

## Now actually look at covid cases?

api_data = GET("https://em.dcgis.dc.gov/dcgis/rest/services/COVID_19/OpenData_COVID19/FeatureServer/3/query?where=1%3D1&outFields=*&outSR=4326&f=json")

dc_positives <- fromJSON(rawToChar(api_data$content))
dc_positives <- dc_positives$features$attributes

dc_positives <- dc_positives %>%
  filter(!is.na(TOTAL_POSITIVES_TST)) %>%
  mutate(prev_day_total = lag(TOTAL_POSITIVES_TST),
         adj_total = if_else(condition = prev_day_total > TOTAL_POSITIVES_TST,true = prev_day_total,TOTAL_POSITIVES_TST),
         total_new_cases = adj_total - prev_day_total,
         moving_avg_new_cases = rollapply(total_new_cases,14,mean,align='right',fill=NA),
         day = as.Date(as.POSIXct(DATE_REPORTED / 1000, origin="1970-01-01")),
         type="General Population") %>%
  select(day,adj_total,total_new_cases,moving_avg_new_cases,type)

dc_positives$day <- dc_positives$day - 14
#crime_covid_by_day$day <- crime_covid_by_day$day - 14

crime_covid_by_day <- merge(crime_by_day,dc_positives,by="day") %>%
  select(day,moving_avg_new_cases,moving_avg_violent)

crime_covid_by_day$lock_down <- ifelse(crime_covid_by_day$day >= as.Date("2020-04-01") & crime_covid_by_day$day <= as.Date("2020-05-29"),
                                       "Stay at home","Stay at Home Lifted")

r2 <- crime_covid_by_day %>%
  group_by(lock_down) %>%
  summarise(r2 = cor(x = moving_avg_new_cases,y=moving_avg_violent,use = "complete.obs"))

covid_violent_crime_r2 <- ggplot(crime_covid_by_day,aes(x=moving_avg_new_cases,y=moving_avg_violent,color=lock_down)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x="Average of New Positive COVID Cases over 14-Day Period",
       y="Average of Violent Crimes over 14-Day Period",
       title="Violent Crimes & COVID Positive Cases",
       subtitle = "Lagged effect of average COVID cases on violent crime",
       caption = paste0("Stay at Home R-Squared: ",round(filter(r2,lock_down == "Stay at home")[2],2),
                         " | Post-Stay at Home R-Squared: ",round(filter(r2,lock_down == "Stay at Home Lifted")[2],2))) +
  theme(legend.position = "bottom")

ggsave(plot = covid_violent_crime_r2, "images/covid_violent_crime_r2.png", w = 12, h = 12)

crime_covid_by_day$day_number <- as.numeric(format(crime_covid_by_day$day,"%j"))

summary(lm(data = crime_covid_by_day,
   formula = moving_avg_violent ~ moving_avg_new_cases + lock_down + day_number))

## by ward

scaling <- .1

ggplot(crime_covid_by_day,aes(x=as.Date(paste(2020,strftime(day,format="%m-%d"),sep="-")))) + 
  geom_line(aes(y=moving_avg_new_cases),color="darkblue") +
  geom_line(aes(y=moving_avg_violent/scaling),color="darkred") +
  scale_y_continuous(
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*scaling, name="Moving Avergae Violent Crimes")
  ) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-29","%Y-%m-%d"),linetype='dashed') +
  labs(x="Date",
       y="Average Number of Violent Crimes",
       title="Violent Crimes",
       color="Year") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "2 months",
               date_minor_breaks = "1 month",
               date_labels = "%b")

api_data <- GET("https://opendata.arcgis.com/datasets/94b5ab3b8b334d31aa395aea9bef2c10_27.geojson")

covid_cases_ward <- fromJSON(rawToChar(api_data$content))
covid_cases_ward <- covid_cases_ward$features$properties

covid_cases_ward

#############################################################
##
## CHANGES BY WARD
##
#############################################################

violent_crime_by_ward <- crime_df %>%
  mutate(stay_at_home = if_else(month<4,"Pre-Stay At Home Order",
                                if_else(month >= 6,"Stay at Home Lifted","Stay At Home Order"))) %>%
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
                                if_else(month >= 6,"Stay at Home Lifted","Stay At Home Order"))) %>%
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

violent_crime_by_ward$stay_at_home <- factor(violent_crime_by_ward$stay_at_home,levels = c("Pre-Stay At Home Order","Stay At Home Order","Stay at Home Lifted"))
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
  theme_bw() +
  theme(legend.position = "bottom",axis.text.x = element_text(size=8)) +
  scale_x_discrete(labels = function(grouping) str_wrap(grouping, width = 8))

nonviolent_crime_by_ward$stay_at_home <- factor(nonviolent_crime_by_ward$stay_at_home,levels = c("Pre-Stay At Home Order","Stay At Home Order","Stay at Home Lifted"))
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
  theme_bw() +
  theme(legend.position = "bottom",axis.text.x = element_text(size=8)) +
  scale_x_discrete(labels = function(grouping) str_wrap(grouping, width = 8))

change_ward_plots <- grid.arrange(ward_violent,ward_nonviolent,ncol=2,
                             bottom = textGrob("Source: Washington MPD Crime Incident Data | Viz: August Warren (gwarrenn.github.io) | Stay At Home Order issued April 1st & Lifted May 29th",
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

ggsave(plot = change_ward_plots, "images/change_ward_plots_20201115.png", w = 12, h = 6,type = "quartz")

## Type of crime 

crime_counts <- crime_df %>%
  mutate(stay_at_home = if_else(month<4,"Pre-Stay At Home Order",
                                if_else(month >= 6,"Post-George Floyd Murder","Post-Stay At Home Order"))) %>%
  group_by(stay_at_home,OFFENSE) %>%
  summarise(n=n()) %>%
  mutate(pct=n/sum(n))

crime_counts$violent_crime = ifelse(crime_counts$OFFENSE == "homicide" |
                                    crime_counts$OFFENSE == "sex abuse" | 
                                    crime_counts$OFFENSE == "assault w/dangerous weapon" |
                                    crime_counts$OFFENSE == "robbery",1,0)

ggplot(crime_counts,aes(x=OFFENSE,y=pct)) +
  geom_bar(stat="identity") +
  geom_text(aes(x=OFFENSE,y=pct,label=percent(pct)),vjust=-.5) +
  facet_grid(stay_at_home~violent_crime,scales="free")

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


