# aggregate Hydropower cost curves in message region
rm(list = ls())
# setwd("I:/vinca/Hydro/HYDRO_COUNTRY_David")
library(readxl)
library(tidyverse)
library(rpart)
library(zoo)
library(data.table)
library(rJava)
library(xlsx)

cap_cost0 <- read_excel("HYDRO_cost_country_Gernaat et al..xlsx", 
                          sheet = "CAP_COST")

max_pot0 <- read_excel("HYDRO_cost_country_Gernaat et al..xlsx", 
                       sheet = "MAX_POTENTIAL")

load_fact0 <- read_excel("HYDRO_cost_country_Gernaat et al..xlsx", 
                        sheet = "LOAD_FACTOR")
## CAP_COST0
names(cap_cost0)[1]="x"
names(load_fact0)[1]="x"


### JM : What is the point of having x=0? Remove it for now. (it requires many special consideration for curve fitting for the first step.)
cap_cost0 <- cap_cost0 %>% slice(-1)
load_fact0 <- load_fact0 %>% slice(-1)
###

### Reorder based on cost curves (JMin) 
ctyname.org <- names(cap_cost0) # Keep the order of countries from the original file.

cap_cost0.long <- cap_cost0 %>% gather(country, cost, -x)
load_fact0.long <- load_fact0 %>% gather(country, lfact, -x)
data.comb.cty <- cap_cost0.long %>% left_join(load_fact0.long) %>% arrange(country, cost) %>% group_by(country) %>% mutate(x=seq(0.01, 1, 0.01))

cap_cost0 <- data.comb.cty %>% select(-lfact) %>% spread(country, cost) %>% select(ctyname.org)
load_fact0 <- data.comb.cty %>% select(-cost) %>% spread(country, lfact) %>% select(ctyname.org)
###

regions <- tail(names(cap_cost0),-1)

reg_map <- read_csv("P:/ene.model/data/regions/message.csv")
names(reg_map) = c("ISO","name","msg_reg","five_reg")

map_hydro_reg <- data.frame(cname = regions,stringsAsFactors = F)

map_hydro_reg$name = NULL

for (r1 in seq(1:length(reg_map$name)) ) {
  for (r2 in seq(1:length(reg_map$name)) ) {
    if (map_hydro_reg$cname[r1] %like% reg_map$name[r2]) {
      map_hydro_reg$name[r1] = reg_map$name[r2] 
    }
  }
}

#check countries that did not match
map_hydro_reg[map_hydro_reg$name == "Afghanistan",]
# manually change some regions
map_hydro_reg$name[25] = "Brunei Darussalam"
map_hydro_reg$name[42] = "Democratic Republic of the Congo"
map_hydro_reg$name[81] = reg_map$name[84]
map_hydro_reg$name[86] = "Cote dIvoire"
map_hydro_reg$name[92] = "Korea, Democratic Peoples Republic of" 
map_hydro_reg$name[93] = reg_map$name[96]
map_hydro_reg$name[96] = "Lao Peoples Democratic Republic"
map_hydro_reg$name[101] = "Libyan Arab Jamahiriya"
map_hydro_reg$name[117] = "Republic of Moldova"
map_hydro_reg$name[132] = reg_map$name[63]
map_hydro_reg$name[161] = "Viet Nam"
map_hydro_reg$name[184] = "The former Yugoslav Republic of Macedonia"
map_hydro_reg$name[187] = "United Republic of Tanzania"

map_hydro_reg[map_hydro_reg$name == "Afghanistan",]

map_hydro_reg <- map_hydro_reg %>% 
  left_join(reg_map %>% select(name,msg_reg))

max_pot <- max_pot0 %>% 
  gather(key = "cname", value = "pot") 

max_pot_agg <- max_pot %>% 
  left_join(map_hydro_reg %>% select(cname,msg_reg)) %>% 
  group_by(msg_reg) %>% 
  summarise(pot_agg = sum(pot))

max_pot_agg2 <- max_pot %>% 
  left_join(map_hydro_reg %>% select(cname,msg_reg)) %>% 
  group_by(msg_reg) %>% 
  mutate(pot_agg = sum(pot)) %>% 
  ungroup()

cap_cost <-cap_cost0 %>% 
  gather(key = "cname",value = "cost",-x) %>% 
  left_join(max_pot_agg2) %>% 
  group_by(x,msg_reg) %>% 
  mutate(costXpot = cost * pot) %>% 
  summarise(cost_agg = sum(costXpot/pot_agg)) %>% 
  arrange(msg_reg,x)

# check is calculation is correct
# cap_cost_check <-cap_cost0 %>% 
#   gather(key = "cname",value = "cost",-x) %>% 
#   left_join(max_pot_agg2) %>% 
#   group_by(x,msg_reg) %>% 
#   mutate(costXpot = cost * pot) %>% 
#   filter(msg_reg == "AFR")
# YES

# GJ
global_pot <- sum(max_pot_agg$pot_agg)
gbl_cost <-cap_cost0 %>%
  gather(key = "cname",value = "cost",-x) %>%
  left_join(max_pot_agg2) %>%
  mutate(pot_agg = global_pot) %>% 
  group_by(x) %>%
  mutate(costXpot = cost * pot) %>%
  summarise(cost_agg = sum(costXpot/pot_agg))

ggplot()+
  geom_line(data = as.data.frame( bind_rows(cap_cost, gbl_cost %>% mutate(msg_reg = "WLD") %>% select(x,msg_reg,cost_agg)) ),aes(x,cost_agg,colour = msg_reg)) +
  geom_line(data = as.data.frame( gbl_cost  ),aes(x,cost_agg),size = 1, colour = "black")


# LOAD FACTOR
load_fact <-load_fact0 %>% 
  gather(key = "cname",value = "LF",-x) %>% 
  left_join(max_pot_agg2) %>% 
  group_by(x,msg_reg) %>% 
  mutate(LFXpot = LF * pot) %>% 
  summarise(LF_agg = sum(LFXpot/pot_agg)) %>% 
  arrange(msg_reg,x)

gbl_LF <-load_fact0 %>%
  gather(key = "cname",value = "LF",-x) %>%
  left_join(max_pot_agg2) %>%
  mutate(pot_agg = global_pot) %>% 
  group_by(x) %>%
  mutate(LFXpot = LF * pot) %>%
  summarise(LF_agg = sum(LFXpot/pot_agg))

ggplot()+
  geom_line(data = as.data.frame( bind_rows(load_fact, gbl_LF %>% mutate(msg_reg = "WLD") %>% select(x,msg_reg,LF_agg)) ),aes(x,LF_agg,colour = msg_reg)) +
  geom_line(data = as.data.frame( gbl_LF  ),aes(x,LF_agg),size = 1, colour = "black")

# Vinca's aggregation
# write.csv(max_pot_agg,"max_potential_MSG_reg.csv",row.names = F)
# write.csv(cap_cost,"cap_cost_MSG_reg.csv",row.names = F)
# write.csv(load_fact,"load_factor_MSG_reg.csv",row.names = F)


# Reorder based on cost curves (JMin) - Reordering at the beginning will be right. (No reason to order the data based on LCOE from IMAGE)
# cap_cost.ord <- cap_cost %>% arrange(msg_reg, cost_agg)
# load_fact.ord <- (cap_cost.ord %>% select(-cost_agg)) %>% left_join(load_fact)
# cap_cost.ord <- cap_cost.ord %>% group_by(msg_reg) %>% mutate(x=seq(0.01, 1, 0.01)) # reset x index
# load_fact.ord <- load_fact.ord %>% group_by(msg_reg) %>% mutate(x=seq(0.01, 1, 0.01)) # reset x index

write.xlsx(as.data.frame(max_pot_agg), "HYDRO_cost_MESSAGE_reg.ordered.xlsx", "MAX_POTENTIAL",
                 row.names = FALSE, col.names = TRUE, append = FALSE)
write.xlsx(as.data.frame(load_fact), "HYDRO_cost_MESSAGE_reg.ordered.xlsx", "LOAD_FACTOR",
                 row.names = FALSE, col.names = TRUE, append = TRUE)
write.xlsx(as.data.frame(cap_cost), "HYDRO_cost_MESSAGE_reg.ordered.xlsx", "CAP_COST", 
                 row.names = FALSE, col.names = TRUE, append = TRUE)


