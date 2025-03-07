library(tidyverse)
library(zoo)
library(sf)

setwd('/Users/august.warren/Documents/GitHub/dc_data/06_police/')

staffing_data <- read_csv('../data/police/staffing.csv')
staffing_data$Year <- format(as.Date(staffing_data$report_date,"%Y-%m-%d"), "%Y")

stats <- staffing_data %>%
  group_by(PSA_cleaned) %>%
  arrange(report_date) %>%
  #mutate(moving_avg = rollapply(count,3,mean,align='right',fill=NA),
  #       Year_over_Year_Change = count - lag(count, 12)) %>%
  mutate(Percentage_Change = (count - lag(count)) / lag(count) * 100) %>%
  filter(!is.na(Percentage_Change)) %>%
  summarise(avg_change = mean(Percentage_Change))

stats_yearly <- staffing_data %>%
  #group_by(PSA_cleaned,Year) %>%
  #mutate(max_date = max(report_date)) %>%
  group_by(PSA_cleaned) %>%
  mutate(max_date = max(report_date),
         min_date = min(report_date)) %>%
  filter((report_date == min_date) | (report_date == max_date)) %>%
  arrange(PSA_cleaned,Year) %>%
  #group_by(PSA_cleaned) %>%
  mutate(avg_change = (count - lag(count)) / lag(count) * 100) %>%
  filter(!is.na(avg_change)) #%>%
  #summarise(avg_change = mean(Percentage_Change))

## merging census data to PSA data by using intersections

census_blocks_shp <- read_sf("data/Census_Block_Groups_in_2020/Census_Block_Groups_in_2020.shp")
psa_shp <- read_sf("data/Police_Service_Areas/Police_Service_Areas.shp")

results_df <- data.frame()

for(psa in unique(psa_shp$PSA)){
 this_psa <- psa_shp %>% filter(PSA == psa)
 this_overlap <- st_intersection(census_blocks_shp,this_psa)
 this_overlap$PSA <- psa
 
 results_df <- rbind(results_df,this_overlap)
 
}

results_df <- results_df %>% st_drop_geometry()

econ_df <- read_csv("data/ACS_Economic_Characteristics_DC_Census_Tract.csv") %>%
  select(TRACTCE,
         DP03_0001E, ## employment status, population 16 years and over: In labor force
         DP03_0008E, ## employment status, civilian workforce
         DP03_0065E, ## INCOME AND BENEFITS (IN 2019 INFLATION-ADJUSTED DOLLARS): Total households: With earnings: Mean earnings (dollars)
         DP03_0086E, ## INCOME AND BENEFITS (IN 2019 INFLATION-ADJUSTED DOLLARS): Families: Median family income (dollars)
         DP03_0119PE, ## PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL: All families
         )

agg_results_tract <- results_df %>%
  group_by(PSA,TRACT) %>%
  summarise(total_pop = sum(P0010001),
            total_blk_pop = sum(P0010004))

agg_results_tract <- merge(agg_results_tract, econ_df, by.x = "TRACT", by.y="TRACTCE", how='left')

test <- agg_results_tract  %>%
  filter(PSA == "204") %>%
  select(TRACT,total_pop,DP03_0008E,DP03_0119PE)

census_econ_df <- agg_results_tract %>%
  group_by(PSA) %>%
  summarise(total_pop = sum(total_pop),
            total_blk_pop = sum(total_blk_pop),
            avg_mean_hh_inc = mean(DP03_0065E,na.rm=T),
            avg_median_hh_inc = mean(DP03_0086E,na.rm=T),
            mean_poverty = mean(total_pop * (DP03_0119PE/100),na.rm=T)) %>%
  mutate(blk_pop_pct = total_blk_pop / total_pop)

census_econ_df$PSA_cleaned <- paste('PSA',as.character(census_econ_df$PSA))

#prev_date_window <- stats %>%
#  filter(report_date < '2018-06-01') %>%
#  group_by(PSA_cleaned) %>%
#  mutate(max_date = max(report_date)) %>%
#  filter(report_date == max_date)

#current_window <- stats %>%
#  filter((report_date <= '2023-11-01') & (report_date >= '2023-06-01')) %>%
#  group_by(PSA_cleaned) %>%
#  mutate(max_date = max(report_date)) %>%
#  filter(report_date == max_date)

#snapshot <- merge(prev_date_window,current_window,by = "PSA_cleaned")

#snapshot$diff_staffing <- snapshot$moving_avg.y - snapshot$moving_avg.x

## join in crime data 

crime_2023 <- read_csv("../data/crime/Crime_Incidents_in_2023.csv") %>%
  subset(select = c(REPORT_DAT,OFFENSE,PSA))

crime_2018 <- read_csv("../data/crime/Crime_Incidents_in_2018.csv") %>%
  subset(select = c(REPORT_DAT,OFFENSE,PSA))

crime_df <- rbind(crime_2018,crime_2023)

crime_df$day <- as.POSIXct(crime_df$REPORT_DAT,format="%Y/%m/%d")
crime_df$Year <- format(as.POSIXct(crime_df$REPORT_DAT,format="%Y/%m/%d %H:%M:%S"), "%Y")
crime_df$month <- format(as.POSIXct(crime_df$REPORT_DAT,format="%Y/%m/%d %H:%M:%S"), "%m")

crime_df$violent_crime <- ifelse(crime_df$OFFENSE == "HOMICIDE" |
                                   crime_df$OFFENSE == "SEX ABUSE" | 
                                   crime_df$OFFENSE == "ASSAULT W/DANGEROUS WEAPON" |
                                   crime_df$OFFENSE == "ROBBERY",1,0)

crime_stats <- crime_df %>%
  group_by(PSA,day) %>%
  arrange(Year,day) %>%
  summarise(num_violent_crimes = sum(violent_crime)) %>%
  mutate(violent_crimes = cumsum(num_violent_crimes),
         moving_avg_violent = rollapply(num_violent_crimes,28,mean,align='right',fill=NA))

prev_date_window_crime <- crime_stats %>%
  filter(day < '2018-06-01') %>%
  group_by(PSA) %>%
  mutate(max_date = max(day)) %>%
  filter(day == max_date)

current_window_crime <- crime_stats %>%
  filter((day <= '2023-11-01') & (day >= '2023-06-01')) %>%
  group_by(PSA) %>%
  mutate(max_date = max(day)) %>%
  filter(day == max_date)

snapshot_crime <- merge(prev_date_window_crime,current_window_crime,by = "PSA")
snapshot_crime$diff_crime <- snapshot_crime$moving_avg_violent.y - snapshot_crime$moving_avg_violent.x

snapshot_crime <- snapshot_crime %>%
  filter(!is.na(PSA))

snapshot_crime$PSA_cleaned <- paste('PSA',as.character(snapshot_crime$PSA))

## merging crime and staffing

snapshot_crime_staffing <- merge(snapshot_crime,stats_yearly,by = "PSA_cleaned")

snapshot_crime_staffing <- snapshot_crime_staffing %>%
  select(PSA_cleaned,avg_change,diff_crime)

## plotting relationship

print(summary(lm(formula = diff_crime ~ avg_change,
                 data=snapshot_crime_staffing))$r.squared)

ggplot(snapshot_crime_staffing,aes(x=avg_change, y=diff_crime)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = mean(snapshot_crime_staffing$avg_change),linetype='dashed') +
  geom_hline(yintercept = mean(snapshot_crime_staffing$diff_crime),linetype='dashed')

summary(lm(formula = diff_crime ~ avg_change,
           data=snapshot_crime_staffing))

## looking at change by crime-levels (no inference/causation)

crime_stats_2023 <- crime_df %>%
  filter(Year == "2023") %>%
  group_by(PSA,month) %>%
  summarise(num_violent_crimes = sum(violent_crime)) %>%
  group_by(PSA) %>%
  summarise(avg_monthly_crime = mean(num_violent_crimes)) %>%
  mutate(crime_deciles = ntile(avg_monthly_crime, 10))

crime_stats_2023$PSA_cleaned <- paste('PSA',as.character(crime_stats_2023$PSA))

avg_monthly_crime_staffing <- merge(crime_stats_2023,stats_yearly,by = "PSA_cleaned")

## merging in econ indicators

avg_monthly_crime_staffing <- merge(avg_monthly_crime_staffing,census_econ_df,by = "PSA_cleaned")

crime_bins_staffing <- avg_monthly_crime_staffing %>% 
  mutate(poverty_deciles = ntile(mean_poverty, 10),
         pct_black_deciles = ntile(blk_pop_pct, 10)) %>%
  group_by(crime_deciles) %>%
  summarise(avg_change = mean(avg_change))

ggplot(crime_bins_staffing,aes(x=crime_deciles,y=avg_change)) +
  geom_bar(stat="identity",color='black')

summary(lm(formula  = avg_change ~ mean_poverty + blk_pop_pct + avg_monthly_crime,data = avg_monthly_crime_staffing))

ggplot(avg_monthly_crime_staffing,aes(y=blk_pop_pct,x=avg_change)) +
  geom_vline(xintercept = mean(avg_monthly_crime_staffing$avg_change),linetype='dashed') +
  geom_hline(yintercept = mean(avg_monthly_crime_staffing$blk_pop_pct),linetype='dashed') +
  geom_point() +
  geom_smooth(method = 'lm')