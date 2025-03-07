library(tidyverse)
library(zoo)

setwd("/Users/august.warren/Documents/GitHub/dc_data/data/crime/")

years = c(2011,2012,2013,2014,2015,2016)

crime_prior = data.frame()

for (y in years) {
  
  crime_df <- read.csv(paste0(y,"_crime_data.txt"))
  crime_df <- crime_df %>% filter(WARD == 1 | WARD == 6)
  
  crime_df <- crime_df %>%
    mutate(violent = if_else(OFFENSE == "HOMICIDE" | OFFENSE == "SEX ABUSE" | OFFENSE == "ASSAULT W/DANGEROUS WEAPON" | OFFENSE =="ROBBERY",1,0)) %>%
    select(WARD,REPORT_DAT,violent)
  
  crime_prior <- rbind(crime_prior,crime_df)
  
}

crime_prior$month <- format(as.Date(crime_prior$REPORT_DAT,"%m/%d/%Y"), "%m")
crime_prior$Year <- format(as.Date(crime_prior$REPORT_DAT,"%m/%d/%Y"), "%Y")
crime_prior$year_month <- as.Date(paste0(crime_prior$month,"/","01/",as.numeric(crime_prior$Year),sep=""),"%m/%d/%Y")

crime_df <- read.csv("2017_crime_data.csv") %>%
  filter(WARD == 1 | WARD == 6) %>%
  mutate(violent = if_else(OFFENSE == "HOMICIDE" | OFFENSE == "SEX ABUSE" | OFFENSE == "ASSAULT W/DANGEROUS WEAPON" | OFFENSE =="ROBBERY",1,0)) %>%
  select(WARD,REPORT_DAT,violent)

crime_df$month <- format(as.Date(crime_df$REPORT_DAT,"%Y-%m-%d"), "%m")
crime_df$Year <- format(as.Date(crime_df$REPORT_DAT,"%Y-%m-%d"), "%Y")
crime_df$year_month <- as.Date(paste0(crime_df$month,"/","01/",as.numeric(crime_df$Year),sep=""),"%m/%d/%Y")

crime_prior <- rbind(crime_prior,crime_df)

years <- c(2018,2019,2020,2021,2022,2023,2024)

crime_all_years <- data.frame()

for (y in years) {
  
  crime_df <- read.csv(paste0("Crime_Incidents_in_",y,".csv"))
  crime_df <- crime_df %>% filter(WARD == 1 | WARD == 6)
  
  crime_df <- crime_df %>%
    mutate(violent = if_else(OFFENSE == "HOMICIDE" | OFFENSE == "SEX ABUSE" | OFFENSE == "ASSAULT W/DANGEROUS WEAPON" | OFFENSE =="ROBBERY",1,0)) %>%
    select(WARD,REPORT_DAT,violent)
  
  crime_all_years <- rbind(crime_all_years,crime_df)
  
}
  
crime_all_years$month <- format(as.Date(crime_all_years$REPORT_DAT,"%Y/%m/%d"), "%m")
crime_all_years$Year <- format(as.Date(crime_all_years$REPORT_DAT,"%Y/%m/%d"), "%Y")
crime_all_years$year_month <- as.Date(paste0(crime_all_years$month,"/","01/",as.numeric(crime_all_years$Year),sep=""),"%m/%d/%Y")

crime_all_years <- rbind(crime_all_years,crime_prior)

crime_stats <- crime_all_years %>%
  filter(year_month > "2010-12-01") %>%
  group_by(WARD,year_month) %>%
  summarise(total_violent = sum(violent)) %>%
  mutate(moving_avg = rollapply(total_violent,6,mean,align='right',fill=NA))

nadeau_stats <- crime_all_years %>%
  mutate(time_col = if_else(year_month > as.Date("2015-02-01","%Y-%m-%d"),"Post-Election","Pre-Election")) %>%
  group_by(WARD,year_month,time_col) %>%
  filter(WARD == 1) %>%
  summarise(total_violent = sum(violent)) %>%
  group_by(WARD,time_col) %>%
  summarise(avg_crime = mean(total_violent))

allen_stats <- crime_all_years %>%
  mutate(time_col = if_else(year_month > as.Date("2015-01-02","%Y-%m-%d"),"Post-Election","Pre-Election")) %>%
  group_by(WARD,year_month,time_col) %>%
  filter(WARD == 6) %>%
  summarise(total_violent = sum(violent)) %>%
  group_by(WARD,time_col) %>%
  summarise(avg_crime = mean(total_violent))

pre_stats <- rbind(allen_stats,nadeau_stats) %>%
  filter(time_col == "Pre-Election") %>%
  mutate(WARD = paste0("Ward ",WARD))

post_stats <- rbind(allen_stats,nadeau_stats)  %>%
  filter(time_col == "Post-Election") %>%
  mutate(WARD = paste0("Ward ",WARD))

crime_stats$WARD <- paste0("Ward ",crime_stats$WARD)

## staffing 

dc_staffing_df <- data.frame(year_month=c(as.Date('2010-01-01'),
                                          as.Date('2011-01-01'),
                                          as.Date('2012-01-01'),
                                          as.Date('2013-01-01'),
                                          as.Date('2014-01-01'),
                                          as.Date('2015-01-01'),
                                          as.Date('2016-01-01'),
                                          as.Date('2017-01-01'),
                                          as.Date('2018-01-01'),
                                          as.Date('2019-01-01'),
                                          as.Date('2020-01-01'),
                                          as.Date('2021-01-01'),
                                          as.Date('2022-01-01'),
                                          as.Date('2023-01-01'),
                                          as.Date('2024-01-01')),
                             staff_total = c(3961,3801,3899,4010,3972,3837,3737,3821,3855,3810,3799,3580,3460,3337,3337)
                             )
dc_staffing_df$Year <- format(as.Date(dc_staffing_df$year_month,"%Y-%m-%d"), "%Y")

recall_plot <- ggplot(crime_stats,aes(x=year_month,y=total_violent)) +
  facet_wrap(~WARD) +
  geom_line(data=dc_staffing_df,aes(x=year_month,y=staff_total/50),color = 'red') +
  ggtitle("Total Monthly Violent Crimes by Ward (1 & 6)") +
  labs(x = "Date",
       y = "Total Violent Crimes per Month",
       caption = "Crime Data Source: Yearly Data from https://opendata.dc.gov/ | MPD Staffing Source: https://mpdc.dc.gov/node/1653316") +
  annotate(geom = "text",
           label = "Councilmember elected \ninto office",
           x = as.Date("2015-04-01","%Y-%m-%d"),
           y = 5,
           angle = 0, 
           size = 2,
           hjust = 0) +
    geom_text(
      data    = pre_stats,
      mapping = aes(x = as.Date("2011-04-01","%Y-%m-%d"), y = 5, label = paste0("Pre-Election: ",round(avg_crime,2))),
      hjust   = -0.1,
      vjust   = -1,
      size = 2
    ) +
  geom_text(
    data    = post_stats,
    mapping = aes(x = as.Date("2020-04-01","%Y-%m-%d"), y = 5, label = paste0("Post-Election: ",round(avg_crime,2))),
    hjust   = -0.1,
    vjust   = -1,
    size = 2
  ) +
  geom_point() +
  geom_line(aes(x=year_month,y=moving_avg)) +
  geom_vline(xintercept = as.Date("2015-02-01","%Y-%m-%d"), linetype="dashed") +
  geom_vline(xintercept = as.Date("2020-03-01","%Y-%m-%d"), linetype="dotted") +
  scale_y_continuous(
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*50, name="Yearly MPD Staffing")
  )

ggsave(plot = recall_plot, "../../06_police/crime_ward.png", w = 10.67, h = 6)

## autocorrelation

yearly_crime <- crime_all_years %>%
  mutate(time_col = if_else(year_month > as.Date("2015-01-02","%Y-%m-%d"),"Post-Election","Pre-Election")) %>%
  group_by(WARD,Year) %>%
  filter(WARD == 6 & Year != '2024' & Year != '2010') %>%
  summarise(total_violent = sum(violent))

crime_staffing <- merge(yearly_crime,dc_staffing_df,by = 'Year')

crime_staffing <- crime_staffing %>%
  mutate(staffing_change = (staff_total - lag(staff_total)) / lag(staff_total) * 100,
         crime_change = (total_violent - lag(total_violent)) / lag(total_violent) * 100) 

ggplot(crime_staffing,aes(x=staffing_change,y=crime_change)) +
  geom_point()
