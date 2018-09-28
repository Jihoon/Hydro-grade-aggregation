rm(list = ls())
# setwd("I:/vinca/HYDRO_COUNTRY_David")
library(readxl)
library(tidyverse)
library(rpart)
library(zoo)

cost_curves <- read_excel("HYDRO_cost_MESSAGE_reg.xlsx", 
                          sheet = "CAP_COST")
#                          sheet = "CostCurveSmthHydro_Remain")

cost_curves.sprd <- cost_curves %>% 
  spread(msg_reg,cost_agg)
### USE REGRESSION TREE TO FIT THE CURVES WITH STEP FUNCTION ###
#----
# define the number of steps desired
nstep = 8
region <- tail(names(cost_curves.sprd),-1)
s <- cost_curves.sprd$x
# to make the first step for plotting
a= data.frame(msg_reg = as.character(head(region,-1)), xval = 0)
a$msg_reg = as.character(a$msg_reg)
eps = 1e-6
nregion = 11

final_cost_df0 = c()
rsq.val <- c()
cp_record <- data.frame(matrix(ncol = nregion+1, nrow = 0))
Dn_record <- data.frame(matrix(ncol = nregion+1, nrow = 0))
rho_record <- data.frame(matrix(ncol = nregion+1, nrow = 0))
names(cp_record) = region
names(Dn_record) = region
names(rho_record) = region
cp_record[1,] = 0.01
Dn_record[1,] = 1
rho_record[1,] = 0.001

for (j in seq(1,nregion,1)) {
  z=1
  minsplit_z = 2
  repeat{  
    cp_i=0.01
    i=1
    rho = 0.001
      repeat{
        tree_cost <- rpart(get(paste(region[j])) ~ x, data = cost_curves.sprd, control=rpart.control (minsplit = minsplit_z, cp = cp_i))
        nsplit <- max(as.data.frame(tree_cost$cptable)$nsplit)
        if (nsplit+1 == nstep) {break} else {
          i = i+1
          Dn_record[i,j] = nsplit+1-nstep
          cp_i = max(0, cp_i*(1+ rho * Dn_record[i,j]) )
          cp_record[i,j] = cp_i
          if (Dn_record[c(i-1),j]*Dn_record[i,j]>0) {rho = 1.5*rho} else{rho = 0.5* rho}
        }
        rho_record[i,j] = paste0(minsplit_z, "_", rho )
        if ((i>5000) | (abs(cp_record[i,j]-cp_record[c(i-1),j]+eps/100) < eps ) ) {
          print(paste0("In region ", region[j], " increase minsplit to ",minsplit_z+1))
          break}

      }
    if (nsplit+1 == nstep) {break} else {
      minsplit_z <- minsplit_z+1
#      Dn_record[,j] = 0
#      Dn_record[1,j] = 1
    }
    if (minsplit_z > 100/nstep) {
      print(paste0("The curve in region ", region[j], " can't be fitted with nstep= ", nstep))
      break}
  }
rsq.val <- bind_rows(rsq.val, data.frame(msg_reg = region[j], rsq =  (1-printcp(tree_cost)[,c(3)]), n_split = seq(1,nstep,1) ) )
frame_cost0 <- as.data.frame(predict(tree_cost, data.frame(x=s))) %>% mutate(x = s)
names(frame_cost0)[1] = "yval"
frame_cost_j <- frame_cost0 %>% group_by(yval) %>%
  mutate(xval = tail(x,1)) %>%
  select(-x) %>%
  distinct() %>% 
  ungroup() %>% 
  mutate(msg_reg = region[j])

final_cost_df0 <- rbind(final_cost_df0,frame_cost_j)
if (nsplit+1 == nstep) print(paste0("Region ",region[j]," completed"))

}

# plot r squared
rsq.plot <- rsq.val %>% 
  filter(n_split > 1 )

ggplot(rsq.plot)+
  geom_boxplot(aes(msg_reg,rsq))+ggtitle(paste0("R-squared with ",nstep," steps") )

#----
### CHECK THE REGRESSION TREES ###

#unique(predict(tree_cost, data.frame(x=s)))

#nsplit1 <- dim(tree_cost$splits)[1]

#check_yval <- as.data.frame(tree_cost$frame %>% filter(var == "<leaf>") %>% select(yval) %>% rename(y = yval))

# plot_tree_cost <- function(tree, x, y) {
#   s <- seq(0, 1, by=.01)
#   plot(x, y)
#   lines(s, predict(tree, data.frame(x=s)))
# }
# 
# tree_cost <- rpart(AFR ~ x, data = cost_curves.sprd, control=rpart.control (minsplit = 2, cp = 0.00992024))
# max(as.data.frame(tree_cost$cptable)$nsplit)
# 
# frame_cost0 <- as.data.frame(predict(tree_cost, data.frame(x=s))) %>% mutate(x = s)
# names(frame_cost0)[1] = "yval"
# 
# plot_tree_cost(tree_cost,cost_curves.sprd$x,cost_curves.sprd$AFR)
# plot(tree_cost, uniform=TRUE, branch=0.6, margin=0.05)
# text(tree_cost, all=TRUE, use.n=TRUE)
# title("Classification Tree")

#----
### LOAD FACTOR ###
load_fact <- read_excel("HYDRO_cost_MESSAGE_reg.xlsx", 
                        sheet = "LOAD_FACTOR")

annual_avg_lf1 <- left_join(load_fact,final_cost_df0 %>% rename(x=xval),by = c("x","msg_reg")) %>% 
  mutate(xval = yval/yval*x) %>% 
  group_by(msg_reg) %>% 
  mutate(xval = na.locf(xval,na.rm = F, fromLast = T)) %>% 
  select(-yval,-x) %>% 
  ungroup()

final_LF_df0 <- annual_avg_lf1 %>% 
  group_by(msg_reg,xval) %>% 
  summarise(avgLF = mean(LF_agg))

# Quantify total error in LF curve estimation (JM)
LF <- annual_avg_lf1 %>% left_join(final_LF_df0) %>% mutate(sqerr=(LF_agg-avgLF)^2)
paste("LF rmse =", sqrt(mean(LF$sqerr)))

# Quantify total error in CC curve estimation
CC <- cost_curves %>%  mutate(xval=annual_avg_lf1$xval) %>% left_join(final_cost_df0) %>% mutate(sqerr=(cost_agg-yval)^2)
paste("CC rmse (ordered) =", sqrt(mean(CC$sqerr)))



#----
### PLOT ORIGINAL AND FITTED CURVES ###
## final dfs for plotting and exporting ##
final_LF_df_pl <- full_join(a,final_LF_df0) %>% 
  group_by(msg_reg) %>% 
  mutate(avgLF = na.locf(avgLF, fromLast = T))

final_cost_df_pl <- full_join(a,final_cost_df0) %>% 
  group_by(msg_reg) %>% 
  mutate(yval = na.locf(yval, fromLast = T))

# plot_cost_curves <- cost_curves %>% 
#   select(-World) %>% 
#   gather("region","cost",2:27)

region
plot_reg = region[c(4)]
# ggplot()+
#   geom_step(data=final_cost_df_pl,aes(x=xval,y=yval,color=msg_reg),size=1,direction = 'vh')+
#   geom_line(data=cost_curves,aes(x=x,y=cost_agg,color=msg_reg))
ggplot()+
  geom_step(data=final_cost_df_pl %>% filter(msg_reg %in% plot_reg),aes(x=xval,y=yval,color=msg_reg),size=1,direction = 'vh')+
  geom_line(data=cost_curves %>% filter(msg_reg %in% plot_reg),aes(x=x,y=cost_agg,color=msg_reg))

plot_reg = region[c(4)]
ggplot()+
  geom_step(data=final_LF_df_pl[final_LF_df_pl$msg_reg %in% plot_reg,],aes(x=xval,y=avgLF,color=msg_reg),size=1,direction = 'vh')+
  geom_line(data=load_fact[load_fact$msg_reg %in% plot_reg,],aes(x=x,y=LF_agg,color=msg_reg))+
  geom_step(data=final_cost_df_pl[final_cost_df_pl$msg_reg %in% plot_reg,],aes(x=xval,y=yval/20000,color=msg_reg),size=1,linetype = "dotted",direction = 'vh') #check india

#----
### PREPARE FOR GAMS STRUCTURE ###
# renewable_potential, inv_cost, capacity factor
# nstep hydro tech: hydro1 ,hydro2, hydro3, hydro4, hydro5, hydro6, hydro7
# nstep grades: 7 in this attempt

# # renewable_potential, of hydro tech correspond to the 7 xval values in final_cost_df0
# hydro_tec <- c("hydro1" ,"hydro2", "hydro3", "hydro4", "hydro5", "hydro6", "hydro7")
# grade <- c("a","b","c","d","e","f","g")
# map_grade_hydro <- data.frame(tec = hydro_tec,grade = grade,stringsAsFactors=FALSE)
# 
# hydro_pot <- read_excel("IMAGE data.xlsx", 
#                         sheet = "MaxProdHydro_Remain") %>% 
#   select(-World) %>% 
#   gather("region","tot_pot",1:26)
# 
# 
# 
# #renewable_potential(node,commodity,grade,level_renewable,year)
# ren_pot <- final_cost_df0 %>% 
#   select(-yval) %>% 
#   group_by(msg_reg) %>% 
#   mutate(tec = hydro_tec) %>% 
#   left_join(map_grade_hydro) %>% 
#   left_join(hydro_pot) %>% 
#   mutate(perc_pot = xval - lag(xval, default=0)) %>% 
#   mutate(value = perc_pot*tot_pot) %>%   # write it step by step to check it
#   select(-xval,-tot_pot,-perc_pot)
# 
# #inv_cost(node,tec,year) 
# inv_cost <- final_cost_df0 %>% 
#   group_by(msg_reg) %>% 
#   mutate(tec = hydro_tec) %>% 
#   select(-xval) %>% 
#   rename(value = yval)
# 
# # will be ok to just update capacity_factor without grade,
# # for final and good version, should also define ren_cap_fact 
# # with grade for eq RENEWABLES_CAPACITY_REQUIREMENT
# 
# capacity_factor <- final_LF_df0 %>% 
#   group_by(msg_reg) %>% 
#   mutate(tec = hydro_tec) %>% 
#   select(-xval) %>% 
#   rename(value = avgLF) %>% 
#   left_join(map_grade_hydro)
# 
# #will need to take also the flex factor and probably others