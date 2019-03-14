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

#source("2.3_RF.R", local = FALSE)
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

rf = readRDS("", local = FALSE)
lg = readRDS("", local = FALSE)

# Include estimated hit rates from both models
df2$rf.hit = rf$hitRate[match(df2$rowname, rf$rowname)]  # Paste hit rate from rf to df2 when "rowname" matches
df2$lg.hit = lg$hitRate[match(df2$rowname, lg$rowname)]

# Plot Model hit rates against empirical hit rates



# Plot 3 Hit rate against (1) Age group, (2) Sex, (3) Race. Use facet to split

# Define different age groups using ifelse statements
df2$age.group = ifelse(df2$age.raw < 18, "Under 18",
                       ifelse((18 <= df2$age.raw & df2$age.raw <= 25), "18 to 25",
                              ifelse((26 <= df2$age.raw & df2$age.raw <= 32), "26 to 32",
                                     ifelse((33 <= df2$age.raw & df2$age.raw <= 40), "33 to 40",
                                            "Over 40")
                                     )
                              )
                       )

df2 %>% group_by(grp) %>% summarise_all(funs(mean)) # Summarise all three hit rates per group_by

