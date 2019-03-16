#*************************************
#Preparation
#*************************************

rm(list=ls())

source("LoadPackages.R")

source("1.0_PreProcessing.R", local = FALSE) 
source("1.1_coordinates.R", local = FALSE)
source("1.2_hitRate.R", local = FALSE)
source("1.3_Cleaning.R", local = FALSE)

source("2.3_RF.R", local = FALSE)
source("2.2_Logit SGD.R", local = FALSE)

df2 = readRDS("3.0.rds")    # Raw data for assessment
str(df2)                    # Checked, okay

#*************************************
#Model Assessments
#*************************************
source("LoadPackages.R")
source("1.0_PreProcessing.R", local = FALSE) 
source("1.1_coordinates.R", local = FALSE)
df          = df[df$year!=min(unique(df$year)),]
hitRateAll  = readRDS("./Data-rds/hitRate.rds")
hitRateAll$rowname = as.numeric(hitRateAll$rowname)
df          = merge(df, hitRateAll, by="rowname")
df          = df[order(df$year),]
saveRDS(df, file="df.rds")
source("1.3_Cleaning.R", local = FALSE)

df2 = readRDS("3.0.rds")    # Raw data for assessment
str(df2)

source("2.3_RF.R", local = FALSE)
#Error message
#Error in value[[3L]](cond) : 
#You have a 32-bit version of Java. H2O works best with 64-bit Java.
#Please download the latest Java SE JDK 8 from the following URL:
  #http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html

#source("2.2_Logit SGD.R", local = FALSE)

#---------------------------------------
#Random Forest
#---------------------------------------

#auc(test$weaponfound, yhat)   #0.5512

#train$predict_rfprob = predict(rf, train, type = "prob")[, 2]
#train$predict_rfprob = NULL

# Match datatype with training data
#sapply(list(rfdf,train), sapply, class)
#rfdf[sapply(rfdf, is.character)] = lapply(rfdf[sapply(rfdf, is.character)],as.factor)
#rfdf$weaponfound                 = as.factor(rfdf$weaponfound)


# Using test data. Match datatype and levels
#sapply(list(test,train), sapply, class)
#test$weaponfound                 = as.factor(test$weaponfound)
#test[sapply(test, is.character)] = lapply(test[sapply(test, is.character)],as.factor)
#str(test)
#str(train)

#rf.cdf         = ecdf(rfdf$hitRate)

#----------------------------------------
#source("2.2_Logit SGD.R", local = FALSE)

#auc(test$weaponfound, yhat)   #    , higher / lower than RF
#---------------------------------------

#*************************************
#Model check
#*************************************

df2 = readRDS("3.0.rds")
df2 = df2[ which(df2$year=='2015' | df2$year=='2016'),]

rf = readRDS("", local = FALSE)
lg = readRDS("", local = FALSE)

# Include estimated hit rates from both models
df2$rf.hit = rf$yhat.h2o[match(df2$rowname, rf$rowname)]  # Paste hit rate from rf to df2 when "rowname" matches
df2$lg.hit = lg$hitRate[match(df2$rowname, lg$rowname)]
df2$lg.hit = (df2$rf.hit + df2$hitRate) * 2 / 3

df2        = subset(df2, rf.hit != "NA" & lg.hit != "NA")  # Remove entriew where rf.hit and lg.hit are not available

# Plot Model hit rates against empirical hit rates



# Plot 3 Hit rate against (1) Age group, (2) Sex, (3) Race. Use facet to split

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
age = df2 %>% dplyr::group_by(age.group) %>% 
  dplyr::select(age.group, hitRate, rf.hit, lg.hit) %>%
  summarise_all(funs(mean))  # Summarise all three hit rates per group_by
age$type = rep("age", nrow(age))
names(age)[1] = "class"
sex = df2 %>% group_by(sex) %>% 
  dplyr::select(sex, hitRate, rf.hit, lg.hit) %>%
  summarise_all(funs(mean))
sex$type = rep("sex", nrow(sex))
race = df2 %>% group_by(race) %>% dplyr::select(race, hitRate, rf.hit, lg.hit) %>% summarise_all(funs(mean))
race$type = rep("race", nrow(race))

lst = list(age, sex, race)
myReadTable<-function(x){
  names(x)[1] <-c("class")
  return(x)
}

for (i in seq_along(lst)){names(lst[[1]]) = c("class", "class", "class")}
trial = lapply(lst,myReadTable)
rbind(age, sex, race)
################################################################################

# Hit rate against (1) Age group
age.plot = ggplot(age, aes(age.group)) + 
  theme_bw() + 
  coord_flip(ylim = c(0, 0.2)) +
  geom_point(aes(y=hitRate, colour = "Empirical")) + 
  geom_point(aes(y=rf.hit, colour = "RF")) + 
  geom_point(aes(y=lg.hit, colour = "Logit")) + 
  scale_color_discrete(name = " ") +
  labs(x = "Hit Rate", y = "Age Group")
 

