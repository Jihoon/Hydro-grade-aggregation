# rm(list = ls())
setwd("H:/MyDocuments/MESSAGE/Hydro-multi")
library(readxl)
library(tidyverse)
library(rpart)
library(zoo)
library(xlsx)
library(countrycode)
source_url("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")


source("functions_Fit.R")

### Run environment set up
res <- "Reg" #  "Nat" #
focus.res <- c("EEU","FSU") # "RUS" #
# nregion = length(region)

# Decide # of Steps
n.step <- 8 # 5 # 14 # 11 # 
n.substep <- 4


### max_pot: all regions maximum potential in GWa
### final_cost_df0: yval (inv_cost), xval (relative potential, 0 to 1), msg_reg (region name)
### data.combined: Intermediate tibble having all cost & LF information
list[final_cost_df0, max_pot, data.combined]  <- GenerateInvCostCurve(res, focus.res, n.step)

sub_LF.all <- GenerateCapacityFactorCurve(res, focus.res, n.substep, data.combined, max_pot)








# final_LF_df0 <- annual_avg_lf1.ord %>% 
#   group_by(msg_reg,xval) %>% 
#   summarise(avgLF = mean(LF_agg))

# Quantify total error in LF curve estimation
# LF <- annual_avg_lf1.ord %>% left_join(final_LF_df0) %>% mutate(sqerr=(LF_agg-avgLF)^2)
# paste("LF rmse (ordered) =", sqrt(mean(LF$sqerr)))
# 
# # Quantify total error in CC curve estimation
# # temp <- final_cost_df0 %>% select(-yval) %>% mutate(n=xval*100) %>%
# #   mutate(n_diff=n-lag(n)) %>% mutate_cond(is.na(n_diff)|n_diff<0, n_diff=n+1)  # Hmm, I couldn't have done simply annual_avg_lf1.ord$xval
# # st <- as.numeric(unlist(apply(temp, 1, function(x) {rep(x[1], x[4])})))
# 
# # Quantify total error in CC curve estimation
# CC <- cost_curves %>% ungroup() %>% 
#   filter(msg_reg==focus.res) %>%
#   mutate(xval=annual_avg_lf1.ord$xval) %>% left_join(final_cost_df0) %>% mutate(sqerr=(cost_agg-yval)^2)
# paste("CC rmse (ordered) =", sqrt(mean(CC$sqerr)))



#----
### PLOT ORIGINAL AND FITTED CURVES ###
## final dfs for plotting and exporting ##
# final_LF_df_pl <- full_join(a,final_LF_df0) %>% 
#   group_by(msg_reg) %>% 
#   filter(msg_reg==focus.res) %>%
#   mutate(avgLF = na.locf(avgLF, fromLast = T))

final_cost_df_pl <- full_join(a,final_cost_df0) %>% 
  group_by(msg_reg) %>% 
  filter(msg_reg==focus.res) %>%
  mutate(yval = na.locf(yval, fromLast = T), 
         xval = xval*(max_pot %>% select(!!focus.res) %>% as.numeric())) %>%
  rename(avgIC = yval)

final_LF_df_pl <- sub_LF.all %>%
  # mutate(xval = xval*(max_pot %>% select(!!focus.res) %>% as.numeric())) %>% # Already taken care of
  mutate(commodity=cut(xval, unique(final_cost_df_pl$xval), labels=paste0("hydro_c", 1:nstep))) %>%     # To mark the technology levels to LF curve
  mutate(x.interval=xval-lag(xval))

# Making it to MESSAGE format (JM) w/ incremental intervals
# final_LF_df_pl <- final_LF_df_pl %>% mutate(xval=xval-lag(xval)) %>% slice(-1) %>% 
#   mutate(xval=xval*max_pot %>% select(RUS) %>% as.numeric()) #%>% rename(potential=xval)
# final_cost_df_pl <- final_cost_df_pl %>% mutate(xval=xval-lag(xval)) %>% slice(-1) %>% 
#   mutate(xval=xval*max_pot %>% select(RUS) %>% as.numeric()) #%>% rename(potential=xval)

# save(final_LF_df_pl, file=paste0("LoadFac_", focus.res, "_", nstep, "main_", n.substep, "subs.rda"))
# save(final_cost_df_pl, file=paste0("CapCost_", focus.res, "_", nstep, "main_", n.substep, "subs.rda"))
write.csv(final_LF_df_pl, file=paste0("LoadFac_", focus.res, "_", nstep, "main_", n.substep, "subs.csv"), row.names = FALSE)
write.csv(final_cost_df_pl, file=paste0("CapCost_", focus.res, "_", nstep, "main_", n.substep, "subs.csv"), row.names = FALSE)

region
plot_reg = focus.res
cap_cost <- data.comb %>% select(-LF_agg) %>% filter(msg_reg==plot_reg) %>% # Original cost curve
  mutate(xval=x * (max_pot %>% select(!!focus.res) %>% as.numeric()))

ggplot()+
  geom_step(data=final_cost_df_pl %>% filter(msg_reg %in% plot_reg),aes(x=xval,y=avgIC,color=msg_reg),size=1,direction = 'vh')+
  geom_line(data=cap_cost, aes(x=xval,y=cost_agg,color=msg_reg))

ggplot()+
  geom_step(data=final_LF_df_pl[final_LF_df_pl$msg_reg %in% plot_reg,],aes(x=xval,y=avgLF,color=msg_reg),size=1,direction = 'vh')+
  # geom_line(data=load_fact[load_fact$msg_reg %in% plot_reg,],aes(x=x,y=LF_agg,color=msg_reg))+
  geom_line(data=annual_avg_lf1.ord[annual_avg_lf1.ord$msg_reg %in% plot_reg,],aes(x=x,y=LF_agg,color=msg_reg))+
  geom_step(data=final_cost_df_pl[final_cost_df_pl$msg_reg %in% plot_reg,],
            aes(x=xval,y=avgIC/20000,color=msg_reg),size=1,linetype = "dotted",direction = 'vh') # Arbitrary scaling factor: 20000

ggplot()+
  geom_step(data=final_LF_df_pl[final_LF_df_pl$msg_reg %in% plot_reg,],aes(x=xval,y=avgLF,color=msg_reg),size=1,direction = 'vh')+
  # geom_line(data=load_fact[load_fact$msg_reg %in% plot_reg,],aes(x=x,y=LF_agg,color=msg_reg))+
  geom_line(data=annual_avg_lf1.ord[annual_avg_lf1.ord$msg_reg %in% plot_reg,],aes(x=x,y=LF_agg,color=msg_reg))+
  geom_step(data=final_cost_df_pl[final_cost_df_pl$msg_reg %in% plot_reg,],aes(x=xval,y=avgIC/20000,color=msg_reg),size=1,linetype = "dotted",direction = 'vh') #check india

