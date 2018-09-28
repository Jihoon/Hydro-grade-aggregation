rm(list = ls())
# setwd("I:/vinca/HYDRO_COUNTRY_David")
library(readxl)
library(tidyverse)

load_factor <- read_excel("HYDRO_cost_country_Gernaat et al..xlsx", 
                          sheet = "LOAD_FACTOR") %>% 
  gather(key = "country",value = "LF",-1)

cap_cost <- read_excel("HYDRO_cost_country_Gernaat et al..xlsx", 
                          sheet = "CAP_COST") %>% 
  gather("country","cap_cost",-1)

to_check <- left_join(load_factor,cap_cost) %>% 
  select(x,country,cap_cost,LF) %>% 
  group_by(country) %>% 
  mutate(ok_check = if_else(LF > lag(LF) & cap_cost < lag(cap_cost),"FALSE", "T"))

false_cases <- to_check %>% 
  filter(ok_check == "FALSE" )

write_csv(to_check,"check_hydro_cost_curves.csv")


# percentage of cases with cost decrease and LF increase
perc <- nrow(false_cases)/nrow(to_check)*100

print(paste(round(perc,2),"of",nrow(to_check),"data entries show capital cost decrease and LF increase"))
