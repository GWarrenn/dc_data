library(tidyverse)
##install.packages("Ternary")
library("Ternary")

###############################################
##
## Processing 2018 Results
##
###############################################

results_2018 <- read.csv("Downloads/June_19_2018_Primary_Election_Certified_Results.csv") %>%
  filter(WardNumber == 1 & (ContestName == "MEMBER OF THE COUNCIL OF THE DISTRICT OF COLUMBIA" | 
           ContestName == "REGISTERED VOTERS - TOTAL")) 

brianne_2018 <- results_2018 %>%
  filter(ContestName == "MEMBER OF THE COUNCIL OF THE DISTRICT OF COLUMBIA") %>%
  group_by(PrecinctNumber) %>%
  mutate(total_votes = sum(Votes),
         vote_pct = Votes/total_votes) %>%
  filter(Candidate == "Brianne K Nadeau") %>%
  select(PrecinctNumber,Votes,vote_pct,total_votes)

turnout_2018 <- results_2018 %>%
  filter(ContestName == "REGISTERED VOTERS - TOTAL",
         Party == "DEM") %>%
  select(PrecinctNumber,Votes) %>%
  rename(Total_Reg = Votes)

brianne_2018 <- merge(brianne_2018,turnout_2018,by = "PrecinctNumber") %>%
  mutate(turnout_pct = Votes/Total_Reg)

###############################################
##
## Processing 2022 Results
##
###############################################

results_2022 <- read.csv("Downloads/June_21_2022_Primary_Election_Election_Night_Unofficial_Results.csv") %>%
  filter(WardNumber == 1 & (ContestName == "DEM MEMBER OF THE COUNCIL OF THE DISTRICT OF COLUMBIA WARD ONE" | 
                              ContestName == "REGISTERED VOTERS - TOTAL")) 

brianne_2022 <- results_2022 %>%
  filter(ContestName == "DEM MEMBER OF THE COUNCIL OF THE DISTRICT OF COLUMBIA WARD ONE") %>%
  group_by(PrecinctNumber) %>%
  mutate(total_votes = sum(Votes),
         vote_pct = Votes/total_votes) %>%
  filter(trimws(Candidate) == "Brianne K. Nadeau") %>%
  select(PrecinctNumber,Votes,vote_pct,total_votes)

turnout_2018 <- results_2022 %>%
  filter(ContestName == "REGISTERED VOTERS - TOTAL",
         Party == "DEM") %>%
  select(PrecinctNumber,Votes) %>%
  rename(Total_Reg = Votes)

brianne_2022 <- merge(brianne_2022,turnout_2018,by = "PrecinctNumber") %>%
  mutate(turnout_pct = Votes/Total_Reg)

###############################################
##
## Combining 2022 Results
##
###############################################

ward_1 <- merge(brianne_2022,brianne_2018,by = "PrecinctNumber") %>%
  mutate(vote_diff = vote_pct.x - vote_pct.y,
         turnout_diff = turnout_pct.x - turnout_pct.y)

ggplot(ward_1,aes(x=vote_diff,y=turnout_diff)) +
  geom_point(aes(size=total_votes.y,fill=vote_diff),colour="black",pch=21) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_fill_distiller(palette = "RdYlGn",direction = 1) +
  theme(legend.position = "bottom") 


