#*************************************
#Preparation
#*************************************

rm(list=ls())

source("LoadPackages.R")
df2 = readRDS("3.0.rds")  
df2 = df2[which(df2$year == '2015' | df2$year == '2016'), ]

rf  = readRDS("./PreProcessing/Data-rds/yhat.RF.rds")
lg  = readRDS("./PreProcessing/Data-rds/yhat.glm.rds")

#*************************************
#Model Assessments
#*************************************


# Include estimated hit rates from both models
df2$rf_hit = rf$yhat[match(df2$rowname, rf$rowname)]  # Paste hit rate from rf ("yhat") to df2 when "rowname" matches
df2$lg_hit = lg$yhat.glm[match(df2$rowname, lg$rowname)]
df2        = subset(df2, !is.na(rf_hit) & !is.na(lg_hit))  # Remove entriew where rf.hit and lg.hit are not available

# Plot Model hit rates against empirical hit rates
ggplot(df2, aes(hitRate)) +
  geom_smooth(aes(y=rf_hit, colour = "RF"), model = "lm") +
  geom_smooth(aes(y=lg_hit, color = "Logit"), model = "lm") +
  scale_color_discrete(name = "Models") +
  labs(x = "Empirical hit rate", y = "Predicted hit rate")
  
# Plot 3 Hit rates against (1) Age group, (2) Sex, (3) Race. Use facet to split

# Define different age groups using ifelse statements
df2$age.group = ifelse(df2$age.raw < 18, "0 to 18",
                       ifelse((18 <= df2$age.raw & df2$age.raw <= 25), "18 to 25",
                              ifelse((26 <= df2$age.raw & df2$age.raw <= 32), "26 to 32",
                                     ifelse((33 <= df2$age.raw & df2$age.raw <= 40), "33 to 40",
                                            "40 or above")
                                     )
                              )
                       )
#############################################################################
age  = df2 %>% 
  dplyr::group_by(age.group) %>% 
  dplyr::select(age.group, hitRate, rf_hit, lg_hit) %>%
  summarise_all(funs(mean)) 
age$type = rep("age", nrow(age))    # Indicating the feature of the entries

sex  = df2 %>% group_by(sex) %>% 
  dplyr::select(sex, hitRate, rf_hit, lg_hit) %>%
  summarise_all(funs(mean))
sex$type = rep("sex", nrow(sex))

race = df2 %>% 
  group_by(race) %>% 
  dplyr::select(race, hitRate, rf_hit, lg_hit) %>% 
  summarise_all(funs(mean))
race$type = rep("race", nrow(race))
race$race = c("Asian", "Black", "Hispanic", "White", "Others")

#---------------------------
final      = rbindlist(list(sex, age, race))
names(final)[1] = "case"

rm(age, sex, race)

# Plot 3 Hit rates against (1) Age group, (2) Sex, (3) Race. Use facet to split
ggplot(final, aes(case)) + 
    theme_bw() + 
    coord_flip(ylim = c(0, 0.2)) +
    geom_point(aes(y=hitRate, colour = "Empirical")) + 
    geom_point(aes(y=rf_hit, colour = "RF")) + 
    geom_point(aes(y=lg_hit, colour = "Logit")) + 
    scale_color_discrete(name = "Models") +
    labs(x = "Hit Rate", y = "Features") +
    facet_grid(type~., scales = "free", space = "free")
