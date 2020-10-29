library(tidyverse)
library(httr)
library(jsonlite)
library(zoo)

#############################################################
##
## TOTAL DC POSITIVES
##
#############################################################

dc_positives <- read.csv("data//total_positives_dc_20200917.csv")

dc_positives$fmt_date <- as.Date(dc_positives$date,"%m/%d/%y")
dc_positives$total_positives <- as.numeric(dc_positives$total_positives)

dc_positives <- dc_positives %>%
  filter(!is.na(total_positives)) %>%
  mutate(prev_day_total = lag(total_positives),
         adj_total = if_else(condition = prev_day_total > total_positives,true = prev_day_total,total_positives),
         total_new_cases = adj_total - prev_day_total,
         total_new_cases_adj_pop = (total_new_cases / 705749) * 1000,
         moving_avg_new_cases = rollapply(total_new_cases_adj_pop,7,mean,align='right',fill=NA),
         type="General Population") %>%
  select(fmt_date,adj_total,total_new_cases,total_new_cases_adj_pop,moving_avg_new_cases,type)

#############################################################
##
## ADD IN POLICE COVID DATA
##
#############################################################

api_data = GET("https://em.dcgis.dc.gov/dcgis/rest/services/COVID_19/OpenData_COVID19/FeatureServer/9/query?where=1%3D1&outFields=*&outSR=4326&f=json")

police_covid <- fromJSON(rawToChar(api_data$content))
police_covid_data <- police_covid$features$attributes

police_covid_data <- police_covid_data %>%
  filter(!is.na(TOTAL_POSITIVE_PSMPD)) %>%
  mutate(prev_day_total = lag(TOTAL_POSITIVE_PSMPD),
         adj_total = if_else(condition = prev_day_total > TOTAL_POSITIVE_PSMPD,true = prev_day_total,TOTAL_POSITIVE_PSMPD),
         total_new_cases = adj_total - prev_day_total,
         total_new_cases_adj_pop = (total_new_cases / 3851) * 1000,
         moving_avg_new_cases = rollapply(total_new_cases_adj_pop,7,mean,align='right',fill=NA),
         fmt_date = as.Date(as.POSIXct(DATE_REPORTED / 1000, origin="1970-01-01")),
         type = "MPD") %>%
  select(fmt_date,adj_total,total_new_cases,total_new_cases_adj_pop,moving_avg_new_cases,type)

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
  mutate(prev_day_total = lag(TOTAL_POSITIVE_FEMS),
         adj_total = if_else(condition = prev_day_total > TOTAL_POSITIVE_FEMS,true = prev_day_total,TOTAL_POSITIVE_FEMS),
         total_new_cases = adj_total - prev_day_total,
         total_new_cases_adj_pop = (total_new_cases / 2500) * 1000,
         moving_avg_new_cases = rollapply(total_new_cases_adj_pop,7,mean,align='right',fill=NA),
         fmt_date = as.Date(as.POSIXct(DATE_REPORTED / 1000, origin="1970-01-01")),
         type = "FEMS") %>%
  select(fmt_date,adj_total,total_new_cases,total_new_cases_adj_pop,moving_avg_new_cases,type)

police_dc_covid <- rbind(police_covid_data,dc_positives,fems_covid_data)

covid_rates_plot <- ggplot(police_dc_covid,aes(x=fmt_date,y=moving_avg_new_cases,color=type)) +
  geom_line(size=1) +
  geom_vline(xintercept = as.Date("2020-04-01","%Y-%m-%d")) +
  geom_vline(xintercept = as.Date("2020-05-29","%Y-%m-%d"),linetype='dashed') +
  labs(y="Total Number of New Cases per 1,000",
       title="7-Day Moving Average of New COVID-19 Positive Cases in the District", 
       subtitle = "among MPD, FEMS & DC General Population",
       caption="Rates are adjusted to DC Population (705,749), Sworn Police Personnel (3,851), and FEMS Personnel (~2,500)",
       color="Legend") +
  theme_bw() +
  scale_color_manual(values = c("#e63c3c","#808080","#2f2fed")) +
  theme(legend.position = "bottom",
        axis.title.x=element_blank()) +
  scale_x_date(date_breaks = "2 months",
               date_minor_breaks = "1 month",
              date_labels = "%b") +
  annotate("text", x = as.Date("2020-04-02","%Y-%m-%d"), y=-.07, label = "Stay-at-home ordered",size=3,hjust = 0) +
  annotate("text", x = as.Date("2020-05-30","%Y-%m-%d"), y=-.07, label = "Stay-at-home lifted",size=3,hjust = 0)

ggsave(plot = covid_rates_plot, "images/covid_police_genpop.png", w = 12, h = 6)

