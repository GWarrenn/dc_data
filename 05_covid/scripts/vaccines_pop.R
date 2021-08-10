library(tidyverse)
library(httr)
library(jsonlite)
library(zoo)
library(reshape2)

#############################################################
##
## DC WARD BY AGE POP
##
#############################################################

data <- read.csv("data/acs_age.csv")

data_l <- melt(data,id.vars = "age_group")

data_l$age_recode <- ifelse(data_l$age_group == "65 to 74 years" | 
                              data_l$age_group == "75 to 84 years" |
                              data_l$age_group == "85 years and over","65 years and over","Under 65")

agg <- data_l %>%
  mutate(value_numeric = as.numeric(gsub(",","",value))) %>%
  group_by(variable,age_recode) %>%
  summarise(total_pop_age = sum(value_numeric)) %>%
  mutate(total_pop = sum(total_pop_age),
         percent_pop = total_pop_age/total_pop)

vaccine_data <- read.csv("data/vaccines.csv")

vaccine_pop <- merge(agg,vaccine_data,by.x = "variable",by.y="ward") %>%
  filter(age_recode == "65 years and over")

age_plot <- ggplot(vaccine_pop,aes(x=variable,y=percent_pop)) +
  geom_bar(stat="identity",color="black") +
  geom_text(label=percent(vaccine_pop$percent_pop),vjust=-2) +
  geom_text(label=paste0("N=",vaccine_pop$total_pop_age),vjust=-.5,size=3) +
  labs(x="Ward",y="Percent of Population aged 65+",
       caption="Source: 2013-2017 ACS Data (https://planning.dc.gov/page/american-community-survey-acs-estimates)",
       title="Percent of Population Aged 65+ by Ward") +
  scale_y_continuous(limits = c(0,.17))

ggsave(plot = age_plot, "images/age_plot.png", w = 12, h = 6)

vaccine_pop$vaccines_per_cap <- (vaccine_pop$vaccines / vaccine_pop$total_pop_age) * 100

vaccine_plot <- ggplot(vaccine_pop,aes(x=variable,y=vaccines_per_cap)) +
  geom_bar(stat="identity",color="black") +
  geom_text(label=round(vaccine_pop$vaccines_per_cap,2),vjust=-.5) +
  labs(x="Ward",y="Total Vaccines Appointments per Capita 65+",
       caption="Source: 2013-2017 ACS Data & PoPville Vaccine Appointments Reported",
       title="Total Vaccine Appointments by Ward per Capita Aged 65+")
  
ggsave(plot = vaccine_plot, "images/vaccine_plot.png", w = 12, h = 6)

  


