library(tidyverse)
library(zoo)
library(lubridate)
library(RCurl)
library(httr)
library(jsonlite)

## Examining the relationship between increases in COVID cases and vaccinations in the context of Delta.
## I tweeted about it: https://twitter.com/gus_warren/status/1428425173158539272

############################################################################################################
##
## County-level census population estimates for 2019 
## https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/
##
############################################################################################################

census <- read.csv("/Users/augustwarren/Downloads/co-est2019-alldata.csv")

census$str_countyFIPS <- ifelse(nchar(census$COUNTY) == 1,paste0("00",census$COUNTY),
                          ifelse(nchar(census$COUNTY) == 2,paste0("0",census$COUNTY),census$COUNTY))

census$FIPS <- paste0(census$STATE,census$str_countyFIPS)

census <- census %>% select(FIPS,POPESTIMATE2019,str_countyFIPS,STATE,STNAME,CTYNAME)

# api_req = GET("https://data.cdc.gov/resource/8xkx-amqh.json")
#vacc_count_obj <- fromJSON(rawToChar(api_req$content,multiple = TRUE))

############################################################################################################
##
## County-level vaccinations w/ historical data (The Socrata API limits results and is a pain)
## https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
##
############################################################################################################

vacc_county <- read.csv("/Users/augustwarren/Downloads/COVID-19_Vaccinations_in_the_United_States_County.csv")

vacc_county$FIPS <- as.numeric(vacc_county$FIPS)

## some counties, mostly TX have 0 recorded vaccinations (they just don't report county level data because they're texas), which will throw off the findings

problem_counties <- vacc_county %>%
  filter(!is.na(Administered_Dose1_Recip)) %>%
  group_by(FIPS) %>%
  mutate(max_doses = max(Administered_Dose1_Recip)) %>%
  filter(max_doses == 0) %>%
  select(Recip_County,Recip_State) %>%
  unique()

vacc_county <- vacc_county %>%
  filter(!is.na(Administered_Dose1_Recip)) %>%
  group_by(FIPS) %>%
  mutate(max_doses = max(Administered_Dose1_Recip)) %>%
  filter(max_doses > 0)

## merge in county level population for vaxx rates per 100k

vacc_county <- merge(vacc_county,census,by="FIPS")

## aggregations

vacc_county_agg <- vacc_county %>%
  filter(as.Date(Date,"%m/%d/%Y") >= as.Date("2021-07-10","%Y-%m-%d")) %>%
  mutate(pre_post = if_else(as.Date(Date,"%m/%d/%Y") >= as.Date("2021-07-25"),"Post","Pre")) %>%
  group_by(FIPS,pre_post) %>%
  arrange(FIPS,pre_post,as.Date(Date,"%m/%d/%Y")) %>%
  dplyr::select(FIPS,pre_post,Date,Administered_Dose1_Recip,POPESTIMATE2019) %>%
  mutate(prev_day_vaxx_total = lag(Administered_Dose1_Recip),
         adj_total_vaxx = if_else(condition = prev_day_vaxx_total > Administered_Dose1_Recip,true = prev_day_vaxx_total,Administered_Dose1_Recip),
         total_new_vaxx = adj_total_vaxx - prev_day_vaxx_total,
         vaxx_per_100k = (total_new_vaxx / POPESTIMATE2019) * 100000) %>%
  group_by(FIPS,pre_post) %>%
  filter(!is.na(vaxx_per_100k)) %>%
  summarise(avg_vacc = mean(vaxx_per_100k)) %>%
  spread(pre_post,avg_vacc)
 
############################################################################################################
##
## Combining Changes in Vaccination & Positive Cases
##
############################################################################################################

vaxx_cases <- merge(vacc_county_agg,cases_counties_agg,by="FIPS")

vaxx_cases$net_change_vaxx <- vaxx_cases$Post.x - vaxx_cases$Pre.x

vaxx_cases$cases_bin <- ntile(vaxx_cases$net_change_cases, 4)

scatter_cases <- ggplot(filter(vaxx_cases,STNAME != "Colorado"),aes(x=net_change_cases,
                                                                    y=net_change_vaxx)) +
  geom_point(aes(color=as.character(cases_bin))) +
  geom_smooth(method = "lm") +
  labs(x="Net Positive Case Rate Change Per-100K Pre/Post 7/25",
       y="Net Vaccination Rate Change Per-100K Pre/Post 7/25",
       color="Positive Case Rate Quartile Bins") +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set2")

vaxx_cases_agg <- vaxx_cases %>%
  filter(!is.na(net_change_vaxx) & 
           STNAME != "Colorado") %>%
  mutate(cases_bin = ntile(net_change_cases, 4)) %>%
  group_by(cases_bin) %>%
  summarise(avg_vaxx_rate = mean(net_change_vaxx))

bar_plot_cases <- ggplot(vaxx_cases_agg,aes(x=cases_bin,y=avg_vaxx_rate)) +
  geom_bar(stat="identity",color="black",aes(fill=as.character(cases_bin))) +
  geom_text(aes(label=round(avg_vaxx_rate,2)),nudge_y = 1.5) +
  labs(x="Net Positive Case-Rate Change Quartiles",
       y="Average Vaccination Per-100K Change Pre/Post 7/25",
       fill="Positive Case Rate Quartile Bins") +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")

combined <- vaxx_cases %>%
  dplyr::select(FIPS,net_change_vaxx,net_change_cases,cases_bin) 

plots <- grid.arrange(scatter_cases,bar_plot_cases,ncol=2,
                      bottom = textGrob("Source: CDC Vaccination Data & Johns Hopkins Covid Death Data | Created by August Warren (https://gwarrenn.github.io/) | Net positive cases are measured as the change in county-level average daily positive cases in the two weeks prior to 7/25 compared to the following weeks.",
                                        x = 0,
                                        y = 0.5,
                                        just = "left",
                                        gp = gpar(fontsize = 6)))

ggsave(plot = plots, "images/vaxx_cases.png", w = 12, h = 6)
  
############################################################################################################
##
## Combining Changes in Vaccination & Hospitalizations
##
############################################################################################################

covid_act <- read.csv("https://api.covidactnow.org/v2/counties.timeseries.csv?apiKey=3a45be2889904c9ab221f55f0f7de6c2")

covid_act_filter <- covid_act %>%
  filter(date > as.Date("2021-07-10"))

covid_act_filter <- merge(covid_act_filter,census,by.x="fips",by.y="FIPS")

hospital_beds <- covid_act_filter %>%
  mutate(pre_post = if_else(as.Date(date,"%Y-%m-%d") >= as.Date("2021-07-25"),"Post","Pre")) %>%
  group_by(fips) %>%
  arrange(fips,pre_post,date) %>%
  dplyr::select(fips,pre_post,date,actuals.hospitalBeds.currentUsageCovid,CTYNAME,STNAME,POPESTIMATE2019) %>%
  group_by(fips,STNAME,pre_post) %>%
  filter(!is.na(actuals.hospitalBeds.currentUsageCovid)) %>%
  summarise(average_beds_covid = mean(actuals.hospitalBeds.currentUsageCovid),
            POPESTIMATE2019 = mean(POPESTIMATE2019)) %>%
  mutate(beds_per_100k = (average_beds_covid / POPESTIMATE2019) * 100000) %>%
  select(pre_post,beds_per_100k,fips,STNAME) %>%
  spread(pre_post,beds_per_100k) %>%
  mutate(net_change_beds = Post - Pre) %>%
  filter(!is.na(net_change_beds))

vaxx_beds <- merge(hospital_beds,vacc_county_agg,by.x="fips",by.y="FIPS")

vaxx_beds$net_change_vaxx <- vaxx_beds$Post.y - vaxx_beds$Pre.y
vaxx_beds$bed_bin <- ntile(vaxx_beds$net_change_beds, 4)

scatter <- ggplot(filter(vaxx_beds,STNAME != "Colorado"),aes(y=net_change_vaxx,
                                                              x=net_change_beds)) +
  geom_point(aes(color=as.character(bed_bin))) +
  geom_smooth(method = "lm") +
  labs(x="Net COVID ICU Bed Change Per-100K Pre/Post 7/25",
       y="Net Vaccination Rate Change Per-100K Pre/Post 7/25",
       color="Death Rate Quartile Bins") +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set2")

vaxx_beds_agg <- vaxx_beds %>%
  filter(!is.na(net_change_vaxx) & 
           STNAME != "Colorado") %>%
  mutate(bed_bin = ntile(net_change_beds, 4)) %>%
  group_by(bed_bin) %>%
  summarise(avg_vaxx_rate = mean(net_change_vaxx))

bar_plot <- ggplot(vaxx_beds_agg,aes(x=bed_bin,y=avg_vaxx_rate)) +
  geom_bar(stat="identity",color="black",aes(fill=as.character(bed_bin))) +
  geom_text(aes(label=round(avg_vaxx_rate,2)),nudge_y = 1.5) +
  labs(x="Net COVID ICU Bed Change Quartiles",
       y="Average Vaccination Per-100K Change Pre/Post 7/25",
       fill="COVID ICU Bed Quartile Bins") +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")

plots <- grid.arrange(scatter,bar_plot,ncol=2,
                      bottom = textGrob("Source: CDC Vaccination Data & COVID Act Now API | Created by August Warren (https://gwarrenn.github.io/) | Net Bed % Change is measured as the change in county-level average number of acute beds use by COVID patients in the two weeks prior to 7/25 compared to the following weeks.",
                                        x = 0,
                                        y = 0.5,
                                        just = "left",
                                        gp = gpar(fontsize = 6)))

ggsave(plot = plots, "images/vaxx_beds.png", w = 12, h = 6)

############################################################################################################
##
## Combining Changes in Vaccination & Deaths
##
############################################################################################################

cases_counties_agg <- death_counties %>%
  mutate(pre_post = if_else(as.Date(Last_Update,"%Y-%m-%d") >= as.Date("2021-07-25"),"Post","Pre")) %>%
  group_by(FIPS) %>%
  arrange(FIPS,pre_post,Last_Update) %>%
  dplyr::select(FIPS,pre_post,Last_Update,Confirmed,POPESTIMATE2019,CTYNAME,STNAME) %>%
  mutate(prev_day_cases_total = lag(Confirmed),
         adj_total_csaes = if_else(condition = prev_day_cases_total > Confirmed,true = prev_day_cases_total,Confirmed),
         total_new_cases = adj_total_csaes - prev_day_cases_total,
         cases_per_100k = (total_new_cases / POPESTIMATE2019) * 100000) %>%
  group_by(FIPS,CTYNAME,STNAME,pre_post) %>%
  filter(!is.na(cases_per_100k)) %>%
  summarise(avg_cases = mean(cases_per_100k))  %>%
  spread(pre_post,avg_cases) %>%
  mutate(net_change_cases = Post - Pre)

## county death data from jhu

death_counties <- data.frame()

dates <- as.character(seq(Sys.Date()-40,Sys.Date()-1, by="days"))

for(i in dates){
  
  print(paste0("Pulling: ",format(strptime(i, "%Y-%m-%d"), "%m-%d-%Y")))
  
  death_county <- getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",format(strptime(i, "%Y-%m-%d"), "%m-%d-%Y"),".csv"))
  death_county_data <- read.csv(text = death_county) %>%
    filter(Admin2 != "" & !is.na(FIPS)) 
  
  death_counties <- rbind(death_counties,death_county_data)
  
}

death_counties <- merge(death_counties,census,by="FIPS")

death_counties_agg <- death_counties %>%
  mutate(pre_post = if_else(as.Date(Last_Update,"%Y-%m-%d") >= as.Date("2021-07-25"),"Post","Pre")) %>%
  group_by(FIPS) %>%
  arrange(FIPS,pre_post,Last_Update) %>%
  select(FIPS,pre_post,Last_Update,Deaths,POPESTIMATE2019,CTYNAME,STNAME) %>%
  mutate(prev_day_death_total = lag(Deaths),
         adj_total_death = if_else(condition = prev_day_death_total > Deaths,true = prev_day_death_total,Deaths),
         total_new_death = adj_total_death - prev_day_death_total,
         death_per_100 = (total_new_death / POPESTIMATE2019) * 100) %>%
  group_by(FIPS,CTYNAME,STNAME,pre_post) %>%
  filter(!is.na(death_per_100)) %>%
  summarise(avg_death = mean(death_per_100)) %>%
  spread(pre_post,avg_death) %>%
  mutate(net_change_death = Post - Pre)

vaxx_death <- merge(vacc_county_agg,death_counties_agg,by="FIPS")

vaxx_death$net_change_vaxx <- vaxx_death$Post.x - vaxx_death$Pre.x

vaxx_death$death_bin <- ntile(vaxx_death$net_change_death, 4)

scatter <- ggplot(filter(vaxx_death,STNAME != "Colorado"),aes(y=net_change_vaxx,
                                                   x=net_change_death)) +
  geom_point(aes(color=as.character(death_bin))) +
  geom_smooth(method = "lm") +
  labs(x="Net Death Rate Change Per-100K Pre/Post 7/25",
       y="Net Vaccination Rate Change Per-100K Pre/Post 7/25",
       color="Death Rate Quartile Bins") +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set2")

vaxx_death_agg <- vaxx_death %>%
  filter(!is.na(net_change_vaxx) & 
           STNAME != "Colorado") %>%
  mutate(death_bin = ntile(net_change_death, 4)) %>%
  group_by(death_bin) %>%
  summarise(avg_vaxx_rate = mean(net_change_vaxx))

bar_plot <- ggplot(vaxx_death_agg,aes(x=death_bin,y=avg_vaxx_rate)) +
  geom_bar(stat="identity",color="black",aes(fill=as.character(death_bin))) +
  geom_text(aes(label=round(avg_vaxx_rate,2)),nudge_y = 1.5) +
  labs(x="Net Death-Rate Change Quartiles",
       y="Average Vaccination Per-100K Change Pre/Post 7/25",
       fill="Death Rate Quartile Bins") +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")

plots <- grid.arrange(scatter,bar_plot,ncol=2,
                      bottom = textGrob("Source: CDC Vaccination Data & Johns Hopkins Covid Death Data | Created by August Warren (https://gwarrenn.github.io/) | Net deaths are measured as the change in county-level average daily deaths in the two weeks prior to 7/25 compared to the following weeks.",
                                        x = 0,
                                        y = 0.5,
                                        just = "left",
                                        gp = gpar(fontsize = 8)))

ggsave(plot = plots, "images/vaxx_death.png", w = 12, h = 6)


combined <- merge(combined,vaxx_death,by="FIPS")

combined$cases_deaths <- paste0("Cases Bin: ",combined$cases_bin," | Deaths Bin: ",combined$death_bin)

combined_agg <- combined %>%
  filter(!is.na(net_change_vaxx.x) & 
           STNAME != "Colorado") %>%
  group_by(cases_bin,death_bin) %>%
  summarise(avg_vaxx_rate = mean(net_change_vaxx.x))

ggplot(combined_agg,aes(x=death_bin,y=avg_vaxx_rate)) +
  geom_bar(stat="identity",color="black",aes(fill=as.character(death_bin))) +
  facet_wrap(~cases_bin) +
  geom_text(aes(label=round(avg_vaxx_rate,2)),nudge_y = 2) +
  labs(x="Net Positive Case-Rate Change Quartiles",
       y="Average Vaccination Per-100K Change Pre/Post 7/25",
       fill="Positive Case Rate Quartile Bins") +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")

