library(tidyverse)
library(httr)
library(jsonlite)
library(zoo)
library(sf)
library(viridis)
library(lubridate)
library(grid)

#############################################################
##
## TOTAL DC POSITIVES
##
#############################################################

## N-day rolling average of new cases by neighborhood

req = GET("https://opendata.arcgis.com/datasets/62f6f28dbfc74ae489fa3dcda9e94744_28.geojson")

nbh_roll <- fromJSON(rawToChar(req$content))
nbh_roll <- nbh_roll$features$properties

nbh_roll <- read.csv("/Users/august.warren/Downloads/DC_COVID-19_Total_Positive_Cases_by_Neighborhood.csv") %>%
  mutate(fmt_date = as.Date(strptime(DATE_REPORTED, "%Y/%m/%d %H:%M:%S")))  %>%
  filter(fmt_date < as.Date("2021-12-23","%Y-%m-%d"))

## DC DoH started mixing up total test & total positives in their data entry on 12/23 -- thanks to Ryan Stahlin for the tip

nbh_roll_temp <- read.csv("/Users/august.warren/Downloads/DC_COVID-19_Total_Tests_by_Neighborhood.csv") %>%
  mutate(fmt_date = as.Date(strptime(DATE_REPORTED, "%Y/%m/%d %H:%M:%S")))  %>%
  filter(fmt_date >= as.Date("2021-12-23","%Y-%m-%d")) %>%
  rename(TOTAL_POSITIVES = TOTAL_TESTS)

nbh_roll <- rbind(nbh_roll_temp,nbh_roll) %>%
  filter(NEIGHBORHOOD != "Unknown"  &
           !is.na(NEIGHBORHOOD) & 
           NEIGHBORHOOD != "")

nbh_roll$CODE <- regmatches(nbh_roll$NEIGHBORHOOD,regexpr("N[0-9]+",nbh_roll$NEIGHBORHOOD))

## NBH populations -- again, thanks to Ryan Stahlin & DC COVID project for the data

nbh_demos <- read.csv("https://raw.githubusercontent.com/rstahlin/rstahlin.github.io/main/nhood_demographics.csv")

nbh_roll <- merge(nbh_demos,nbh_roll,by = "CODE")

nbh_roll_stats_all <- nbh_roll %>%
  mutate(NEIGHBORHOOD_fixed = if_else(condition = (NEIGHBORHOOD == "N24: FOGGY BOTTOM/GWU" | 
                                                     NEIGHBORHOOD == "N24: GWU" | NEIGHBORHOOD == "N24: GWU/ FOGGY BOTTOM"),
                                      "N24: FOGGY BOTTOM/GWU",NEIGHBORHOOD)) %>%
    #fmt_date = as.Date(strptime(nbh_roll$DATE_REPORTED, "%Y-%m-%dT%H:%M:%SZ"))) %>%
  group_by(NEIGHBORHOOD_fixed) %>%
  arrange(fmt_date) %>%
  filter(!is.na(TOTAL_POSITIVES) & 
           NEIGHBORHOOD_fixed != "Unknown"  &
           !is.na(NEIGHBORHOOD_fixed)) %>%
  mutate(prev_day_total = lag(TOTAL_POSITIVES),
         adj_total = if_else(condition = prev_day_total > TOTAL_POSITIVES,true = prev_day_total,TOTAL_POSITIVES),
         total_new_cases = adj_total - prev_day_total,
         total_new_cases_per100k = (total_new_cases / Population..2019.ACS.) * 100,
         moving_avg_new_cases = rollapply(total_new_cases_per100k,7,mean,align='right',fill=NA))  %>%
  mutate(CODE = regmatches(NEIGHBORHOOD_fixed,regexpr("N[0-9]+",NEIGHBORHOOD_fixed)))

lolplot <- ggplot(filter(nbh_roll_stats_all),aes(x=fmt_date,y=moving_avg_new_cases,group=NEIGHBORHOOD_fixed)) +
  geom_line() + 
  labs(y="New Positive Cases, 7-Day Moving Average",x="Date")

ggsave(plot = lolplot, "images/lolplot_nbh.png", w = 12, h = 6)

nbh_roll_stats <- nbh_roll_stats_all %>%
  filter(fmt_date == max(fmt_date))

maps_shp <- read_sf("https://opendata.arcgis.com/datasets/de63a68eb7674548ae0ac01867123f7e_13.geojson")

centroids <- maps_shp %>% 
  st_centroid() %>% 
  bind_cols(as_data_frame(st_coordinates(.))) 

maps_shp <- merge(maps_shp,nbh_roll_stats,by="CODE")

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

centroids <- maps_shp %>% 
  st_centroid() %>% 
  bind_cols(as_data_frame(st_coordinates(.)))    # unpack points to lat/lon columns

map_plot <- ggplot(maps_shp) + 
  geom_sf(data = maps_shp,color=alpha("black",0.2)) +
  geom_sf(data = maps_shp,aes(fill = moving_avg_new_cases)) + 
  theme(axis.text = element_blank()) +
  geom_sf_text(aes(label = CODE), colour = "white",size=2) +
  scale_fill_viridis(
    option = "magma", 
    direction = -1,
    name = "7-Day Rolling Avg. New Cases (per 100)",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    )) +
  theme_map() +
  theme(legend.position = "bottom") 
  
bar_plot <- ggplot(maps_shp,aes(x=reorder(NEIGHBORHOOD_fixed,moving_avg_new_cases),y=moving_avg_new_cases,fill=moving_avg_new_cases)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis(
    option = "magma", 
    direction = -1) +
  theme(legend.position = "none") +
  labs(y="7-Day Rolling Avg. New Cases(per 100)",x="Neighborhood")

plots <- grid.arrange(map_plot,bar_plot,ncol=2,
                                     bottom = textGrob("Source: Washington DC OpenData Portal | Created by August Warren (https://gwarrenn.github.io/)",
                                                       x = 0,
                                                       y = 0.5,
                                                       just = "left",
                                                       gp = gpar(fontsize = 8)))

ggsave(plot = plots, "images/7_day_trend_nbh_20220111.png", w = 12, h = 6)

api_data = GET("https://em.dcgis.dc.gov/dcgis/rest/services/COVID_19/OpenData_COVID19/FeatureServer/3/query?where=1%3D1&outFields=*&outSR=4326&f=json")

dc_positives_api <- fromJSON(rawToChar(api_data$content))
dc_positives_api <- dc_positives_api$features$attributes

dc_positives <- dc_positives_api %>%
  filter(!is.na(TOTAL_POSITIVES_TST)) %>%
  mutate(prev_day_total = lag(TOTAL_POSITIVES_TST),
         prev_day_tested_total = lag(OVERALL_TESTED_TST),
         adj_total = if_else(condition = prev_day_total > TOTAL_POSITIVES_TST,true = prev_day_total,TOTAL_POSITIVES_TST),
         adj_total_tests = if_else(condition = prev_day_tested_total > OVERALL_TESTED_TST,true = prev_day_tested_total,OVERALL_TESTED_TST),
         total_new_cases = adj_total - prev_day_total,
         total_new_tests = adj_total_tests - prev_day_tested_total) %>%
  filter(total_new_tests > 0) %>%
  mutate(total_new_cases_adj_pop = (total_new_cases / 689545) * 1000,
         moving_avg_new_cases = rollapply(total_new_cases_adj_pop,7,mean,align='right',fill=NA),
         moving_avg_total_tests = rollapply(total_new_tests,14,mean,align='right',fill=NA),
         fmt_date = as.Date(as.POSIXct(DATE_REPORTED / 1000, origin="1970-01-01")),
         type="General Population") %>%
  dplyr::select(fmt_date,adj_total,total_new_cases,total_new_cases_adj_pop,
         moving_avg_new_cases,type,total_new_tests,moving_avg_total_tests,
         adj_total_tests) 

## dc positivity rate 

req <- GET("https://opendata.arcgis.com/datasets/fdc0ac2ddd4e4524ab547ad9c601b67a_21.geojson")

pos_rate_api <- fromJSON(rawToChar(req$content))
pos_rate_api <- pos_rate_api$features$properties

pos_rate_df <- pos_rate_api %>%
  mutate(fmt_date = as.Date(DAY_OF_TEST_DATE),
         pos_rate = POSITIVITY_RATE) %>%
  arrange(fmt_date) %>%
  mutate(moving_avg_pos_rate = rollapply(pos_rate,7,mean,align='right',fill=NA)) %>%
  select(fmt_date,pos_rate,moving_avg_pos_rate)

dc_positives <- merge(dc_positives,pos_rate_df,by="fmt_date")

filted_pos_rate <- dc_positives %>%
  filter(fmt_date > as.Date("2020-07-01","%Y-%m-%d"))

max_pos_date <- filted_pos_rate %>%
  mutate(max_pos_rate = max(moving_avg_pos_rate)) %>%
  filter(max_pos_rate == moving_avg_pos_rate)

moving_pos_rate_plot <- ggplot(filted_pos_rate,aes(x=fmt_date)) +
  geom_line(aes(y=moving_avg_pos_rate*100,color="Pos Rate")) +
  geom_line(aes(y=moving_avg_total_tests/1000,color="Num Tests")) + 
  #geom_vline(data = max_pos_date,aes(xintercept = fmt_date),linetype="dashed") +
  scale_y_continuous(limits = c(0,15.5), name = "7-Day Moving Avg. Pos Rate",
                     sec.axis = sec_axis(~.*1000, name="7-Day Moving Avg Daily Tests")) +
  geom_vline(aes(xintercept = as.Date("2021-11-26","%Y-%m-%d")),linetype="solid") +
  geom_vline(aes(xintercept = as.Date("2021-12-25","%Y-%m-%d")),linetype="solid") +
  theme(legend.position = "bottom") +
  labs(x="",color="Legend")

ggsave(plot = moving_pos_rate_plot, "images/moving_pos_rate_plot.png", w = 8, h = 6)

req <- GET("https://opendata.arcgis.com/datasets/c997e67bcfe14081bf4127de07fd21f4_34.geojson")

testing_times <- fromJSON(rawToChar(req$content))
testing_times <- testing_times$features$properties

testing_times$fmt_date <- as.Date(strptime(testing_times$DATE_OF_REPORT, "%Y-%m-%dT%H:%M:%SZ"))

testing_times <- merge(testing_times,dc_positives,by="fmt_date")

ggplot(testing_times,aes(x=fmt_date)) +
  geom_line(aes(y=MEAN_TEST_TURNAROUND,color="Mean Test Time")) +
  geom_line(aes(y=moving_avg_total_tests/1000,color="Num Tests")) +
  scale_y_continuous(limits = c(0,11), name = "Mean Test Turnaround Time",
                     sec.axis = sec_axis(~.*1000, name="7-Day Moving Avg Daily Tests")) +
  geom_vline(aes(xintercept = as.Date("2021-11-26","%Y-%m-%d")),linetype="solid") +
  geom_vline(aes(xintercept = as.Date("2021-12-25","%Y-%m-%d")),linetype="solid") +
  theme(legend.position = "bottom") +
  labs(x="",color="Legend")

testing_times$week <- floor_date(testing_times$fmt_date,unit = "weeks")

week_num <- testing_times %>%
  group_by(week) %>%
  summarise(min_week = min(week)) %>%
  filter(min_week == week) %>%
  mutate(week_num = row_number())

testing_times <- merge(testing_times,week_num,by="week")

model <- lm(formula = MEAN_TEST_TURNAROUND ~ moving_avg_total_tests + week_num,
             data = testing_times)

ggplot(testing_times,aes(y=MEAN_TEST_TURNAROUND,x=moving_avg_total_tests)) +
  geom_point(aes(color=week_num)) +
  geom_smooth(method = "lm") +
  theme(legend.position = "bottom") +
  labs(y="Mean Test Time",x="Num Tests")

## clustering to find distinct "waves"

cluster_data <- filted_pos_rate %>%
  dplyr::select(moving_avg_pos_rate,moving_avg_total_tests) %>%
  filter(!is.infinite(moving_avg_pos_rate) & !is.na(moving_avg_total_tests))

k2 <- kmeans(cluster_data, centers = 4, nstart = 25)

o <- order(k2$cluster)

cluster_ts <- data.frame(filted_pos_rate$moving_avg_pos_rate[o],
                         filted_pos_rate$moving_avg_total_tests[o],
                         filted_pos_rate$fmt_date,
           k2$cluster[o])

for(i in c(1,2,3,4)){
  print(summary(lm(formula = filted_pos_rate.moving_avg_total_tests.o.~filted_pos_rate.moving_avg_pos_rate.o.,
             data=filter(cluster_ts,k2.cluster.o. == i)))$r.squared)
}

cluster_plot <- ggplot(cluster_ts,aes(filted_pos_rate.moving_avg_pos_rate.o.,filted_pos_rate.moving_avg_total_tests.o.,color=as.character(k2.cluster.o.))) +
  geom_point() +
  geom_smooth(method = "lm",aes(color=as.character(k2.cluster.o.))) +
  theme(legend.position = "bottom") +
  labs(color = "Cluster",
       x="Moving Average Positivity Rate",
       y="Moving Average Total Tests")

ggsave(plot = cluster_plot, "images/cluster_plot.png", w = 8, h = 6)

dc_positives_cluster <- merge(dc_positives,cluster_ts,by.y="filted_pos_rate.fmt_date",by.x="fmt_date")

genpop_plot <- ggplot(dc_positives_cluster,
                      aes(x=fmt_date,y=moving_avg_new_cases*100,color=as.character(k2.cluster.o.))) +
  geom_line(size=1) +
  geom_point(data=dc_positives_cluster,
             aes(x=fmt_date,y=total_new_cases_adj_pop*100),alpha=.3) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-29","%Y-%m-%d"),linetype='dashed') + ##stay at home order lifted
  geom_vline(xintercept = as.Date("2021-05-17","%Y-%m-%d"),linetype='dashed') + ##mask mandate lifted
  geom_vline(xintercept = as.Date("2021-07-31","%Y-%m-%d"),linetype='dashed') + ##mask mandate re-instated
  labs(y="Total Number of New Cases per 100,000",
       title="7-Day Moving Average of New COVID-19 Positive Cases", 
       subtitle = "among DC General Population only",
       caption="Rates are adjusted to DC Population (689,545 2020 Census)",
       color="Waves") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank()) +
  scale_x_date(date_breaks = "2 months",
               date_minor_breaks = "1 month",
               date_labels = "%b")

ggsave(plot = genpop_plot, "images/covid_genpop_20210721.png", w = 8, h = 6)

dc_positives <- dc_positives %>%
  dplyr::select(fmt_date,adj_total,total_new_cases,total_new_cases_adj_pop,moving_avg_new_cases,type)

#############################################################
##
## ADD IN POLICE COVID DATA
##
#############################################################

api_data = GET("https://em.dcgis.dc.gov/dcgis/rest/services/COVID_19/OpenData_COVID19/FeatureServer/9/query?where=1%3D1&outFields=*&outSR=4326&f=json")

police_covid <- fromJSON(rawToChar(api_data$content))
police_covid_data <- police_covid$features$attributes

## first, showing how bad the data is

plot_data <- police_covid_data %>%
  mutate(actual_total = TOTAL_POSITIVE_OUT_PSMPD + RECOVRD_RETURND_TO_WORK_PSMPD + LIVE_LOST_MPD,
         fmt_date = as.Date(as.POSIXct(DATE_REPORTED / 1000, origin="1970-01-01"))) %>%
  select(fmt_date,TOTAL_POSITIVE_PSMPD,actual_total) %>%
  gather(key="field",value="value",-fmt_date) 

plot_data <- na.omit(plot_data)

plot_data <- plot_data %>%
  mutate(fmt_date = if_else(fmt_date == as.Date("2022-12-28"),as.Date("2021-12-28"),
                            if_else(fmt_date == as.Date("2022-12-29"),as.Date("2021-12-29"),fmt_date)))

plot_data$field <- factor(x=plot_data$field,levels = c("actual_total","TOTAL_POSITIVE_PSMPD"),
                          labels = c("TOTAL_POSITIVE_OUT_PSMPD + RECOVRD_RETURND_TO_WORK_PSMPD + LIVES LOST",
                                     "TOTAL_POSITIVE_OUT_PSMPD"))

anomaly_plot <- ggplot(plot_data,aes(x=fmt_date,y=value,color=field)) +
  geom_line() + 
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x="Date Reported",y="Total MPD Positive Cases",
       title="Total Daily COVID Cases Reported by DC MPD",
       subtitle = "Data anomalies between 'Total' field and underlying fields by type. Individual align in most cases, but there are some wild differences.",
       caption = "Source: https://opendata.dc.gov/datasets/dc-covid-19-metropolitan-police-department/",
       color="Legend")

ggsave(plot = anomaly_plot, "images/anamoly_plot.png", w = 12, h = 6)

## jesus christ MPD

police_covid_data$TOTAL_POSITIVE_PSMPD <- ifelse(police_covid_data$TOTAL_POSITIVE_PSMPD == 367 & 
                                                       as.Date(as.POSIXct(police_covid_data$DATE_REPORTED / 1000, origin="1970-01-01")) == "2020-10-27",267,police_covid_data$TOTAL_POSITIVE_PSMPD)

#police_covid_data <- na.omit(police_covid_data)

police_covid_data <- police_covid_data %>%
  #filter(!is.na(TOTAL_POSITIVE_PSMPD)) %>%
  mutate(fmt_date = as.Date(as.POSIXct(DATE_REPORTED / 1000, origin="1970-01-01"))) %>%
  arrange(fmt_date) %>%
  mutate(lives_lost_adj = if_else(condition = is.na(LIVE_LOST_MPD),true = 0,as.numeric(LIVE_LOST_MPD)),
         TOTAL_POSITIVE_PSMPD = if_else(condition = is.na(TOTAL_POSITIVE_PSMPD),true = lag(TOTAL_POSITIVE_PSMPD),TOTAL_POSITIVE_PSMPD),
         TOTAL_POSITIVE_PSMPD = if_else(condition = is.na(TOTAL_POSITIVE_PSMPD),true = lag(TOTAL_POSITIVE_PSMPD),TOTAL_POSITIVE_PSMPD),
         TOTAL_POSITIVE_PSMPD = if_else(condition = is.na(TOTAL_POSITIVE_PSMPD),true = lag(TOTAL_POSITIVE_PSMPD),TOTAL_POSITIVE_PSMPD),
         TOTAL_POSITIVE_PSMPD = if_else(condition = is.na(TOTAL_POSITIVE_PSMPD),true = lag(TOTAL_POSITIVE_PSMPD),TOTAL_POSITIVE_PSMPD),
         actual_total = TOTAL_POSITIVE_OUT_PSMPD + RECOVRD_RETURND_TO_WORK_PSMPD + lives_lost_adj) %>%
  dplyr::select(fmt_date,TOTAL_POSITIVE_PSMPD,actual_total)

## wtf, new years data entry issue

police_covid_data <- police_covid_data %>%
  mutate(fmt_date = if_else(fmt_date == as.Date("2022-12-28"),as.Date("2021-12-28"),
                            if_else(fmt_date == as.Date("2022-12-29"),as.Date("2021-12-29"),fmt_date)))

police_covid_data <- police_covid_data %>%
  complete(fmt_date = seq.Date(as.Date(fmt_date[1]),as.Date(fmt_date[nrow(police_covid_data)]), by = "day")) %>%
  mutate(TOTAL_POSITIVE_PSMPD = if_else(is.na(TOTAL_POSITIVE_PSMPD),0,TOTAL_POSITIVE_PSMPD))

## fixing the many data issues involving data entry... 

police_covid_data$adj_total <- police_covid_data$TOTAL_POSITIVE_PSMPD

while('TRUE' %in% unique(police_covid_data$adj_total < lag(police_covid_data$adj_total))){
  police_covid_data <- police_covid_data %>%
    arrange(fmt_date) %>%
    mutate(adj_total = if_else(condition = adj_total < lag(adj_total),true = lag(adj_total),adj_total))
}

police_covid_data <- police_covid_data %>%
  mutate(prev_day_total = lag(adj_total),
         total_new_cases = adj_total - prev_day_total,
         total_new_cases_adj_pop = (total_new_cases / 3851) * 1000,
         moving_avg_new_cases = rollapply(total_new_cases_adj_pop,14,mean,align='right',fill=NA),
         type = "MPD")

police_covid_data <- police_covid_data %>%
  dplyr::select(fmt_date,adj_total,total_new_cases,total_new_cases_adj_pop,moving_avg_new_cases,type)

#############################################################
##
## ADD IN FEMS COVID DATA
##
#############################################################

api_data = GET("https://em.dcgis.dc.gov/dcgis/rest/services/COVID_19/OpenData_COVID19/FeatureServer/8/query?where=1%3D1&outFields=*&outSR=4326&f=json")

fems_covid <- fromJSON(rawToChar(api_data$content))
fems_covid_data <- fems_covid$features$attributes

fems_covid_data <- fems_covid_data %>%
  filter(!is.na(TOTAL_POSITIVE_FEMS)) %>%
  mutate(fmt_date = as.Date(as.POSIXct(DATE_REPORTED / 1000, origin="1970-01-01"))) %>%
  dplyr::select(fmt_date,TOTAL_POSITIVE_FEMS)

fems_covid_data <- fems_covid_data %>%
  complete(fmt_date = seq.Date(as.Date(fmt_date[1]),as.Date(fmt_date[nrow(fems_covid_data)]), by = "day")) %>%
  mutate(TOTAL_POSITIVE_FEMS = if_else(is.na(TOTAL_POSITIVE_FEMS),as.integer(0),TOTAL_POSITIVE_FEMS))

## fixing the many data issues involving data entry... 

fems_covid_data$adj_total <- fems_covid_data$TOTAL_POSITIVE_FEMS

while('TRUE' %in% unique(fems_covid_data$adj_total < lag(fems_covid_data$adj_total))){
  fems_covid_data <- fems_covid_data %>%
    arrange(fmt_date) %>%
    mutate(adj_total = if_else(condition = adj_total < lag(adj_total),true = lag(adj_total),adj_total))
}

fems_covid_data <- fems_covid_data %>%
  mutate(
      prev_day_total = lag(adj_total),
      prev_day_total = ifelse(prev_day_total == 48 & fmt_date == as.Date("2021-07-21"),438,prev_day_total),
      total_new_cases = adj_total - prev_day_total,
      total_new_cases_adj_pop = (total_new_cases / 2500) * 1000,
      moving_avg_new_cases = rollapply(total_new_cases_adj_pop,14,mean,align='right',fill=NA),
      type = "FEMS") %>%
  dplyr::select(fmt_date,adj_total,total_new_cases,total_new_cases_adj_pop,moving_avg_new_cases,type)

police_dc_covid <- rbind(police_covid_data,dc_positives,fems_covid_data)

covid_rates_plot <- ggplot(police_dc_covid,aes(x=fmt_date,y=moving_avg_new_cases,color=type)) +
  geom_line(size=1) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-29","%Y-%m-%d"),linetype='dashed') +
  labs(y="Total Number of New Cases per 1,000",
       title="14-Day Moving Average of New COVID-19 Positive Cases", 
       subtitle = "among MPD, FEMS & DC General Population",
       caption="Rates are adjusted to DC Population (705,749 2019 Census est.), Sworn Police Personnel (3,851), and FEMS Personnel (~2,500)",
       color="Legend") +
  theme_bw() +
  scale_color_manual(values = c("#e63c3c","#808080","#2f2fed")) +
  theme(legend.position = "bottom",
        axis.title.x=element_blank()) +
  scale_x_date(date_breaks = "2 months",
               date_minor_breaks = "1 month",
              date_labels = "%b") 

ggsave(plot = covid_rates_plot, "images/covid_police_genpop.png", w = 12, h = 6)

## total cases

police_dc_covid <- police_dc_covid %>%
  mutate(total_cases_adj_pop = if_else(type=="MPD",(adj_total / 3851)*1000,
                                       if_else(type=="FEMS",(adj_total / 2500)*1000,(adj_total / 705749)*1000)),NA)

total_cases <- ggplot(police_dc_covid,aes(x=fmt_date,y=total_cases_adj_pop,color=type)) + 
  geom_line(size=1) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-29","%Y-%m-%d"),linetype='dashed') +
  labs(y="Total COVID Cases per 1,000",
       title="Cumulative Total Number of COVID-19 Positive Cases", 
       subtitle = "among MPD, FEMS & DC General Population",
       #caption="Rates are adjusted to DC Population (705,749 2019 Census est.), Sworn Police Personnel (3,851), and FEMS Personnel (~2,500)",
       color="Legend") +
  theme_bw() +
  scale_color_manual(values = c("#e63c3c","#808080","#2f2fed")) +
  theme(legend.position = "bottom",
        axis.title.x=element_blank()) +
  scale_x_date(date_breaks = "2 months",
               date_minor_breaks = "1 month",
               date_labels = "%b")

ggsave(plot = total_cases, "images/total_cases.png", w = 12, h = 6)

new_total_cases_plot <- grid.arrange(covid_rates_plot,total_cases,ncol=2,
                                     bottom = textGrob("Source: Washington DC OpenData Portal | Stay At Home Order issued April 1st & Lifted May 29th | Rates are adjusted to DC Population (705,749 2019 Census est.), Sworn Police Personnel (3,851), and FEMS Personnel (~2,500)",
                                                       x = 0,
                                                       y = 0.5,
                                                       just = "left",
                                                       gp = gpar(fontsize = 8)))
                              
ggsave(plot = new_total_cases_plot, "images/new_and_total_cases.png", w = 12, h = 6)


genpop_plot <- ggplot(dc_positives,
                      aes(x=fmt_date,y=moving_avg_new_cases*100)) +
  geom_line(size=1) +
  geom_point(aes(x=fmt_date,y=total_new_cases_adj_pop*100),alpha=.3) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-29","%Y-%m-%d"),linetype='dashed') + ##stay at home order lifted
  geom_vline(xintercept = as.Date("2021-05-17","%Y-%m-%d"),linetype='dashed') + ##mask mandate lifted
  geom_vline(xintercept = as.Date("2021-07-31","%Y-%m-%d"),linetype='dashed') + ##mask mandate re-instated
  labs(y="Total Number of New Cases per 100,000",
       title="7-Day Moving Average of New COVID-19 Positive Cases", 
       subtitle = "among DC General Population only",
       caption="Rates are adjusted to DC Population (689,545 2020 Census)",
       color="Waves") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank()) +
  scale_x_date(date_breaks = "2 months",
               date_minor_breaks = "1 month",
               date_labels = "%b")
