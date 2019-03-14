#*************************************
#Preparation
#*************************************

rm(list=ls())

source("LoadPackages.R")

source("1.0_FirstSteps.R", local = FALSE) 
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
source("1.0_FirstSteps.R", local = FALSE) 
source("1.1_coordinates.R", local = FALSE)
df      = df[df$year!=min(unique(df$year)),]
hitRate = readRDS("hitRate.rds")
df      = cbind(df, hitRate)
colnames(df)[which(names(df)=="df[, \"hitRate\"]")] = "hitRate"
source("1.3_Cleaning.R", local = FALSE)

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

# Include estimated hit rates from both models

z = rownames(df2)
r = x[, "hitRate", drop = FALSE]        # x being the name of the output filee from RF
l = y[, "hitRate", drop = FALSE]        # y being the name of the output filee from logit
df2$rf.hit  = r[row.names(r) %in% z, ]  # Paste rf estimated hit rate when row name matches to df2
df2$glm.hit = y[row.names(y) %in% z, ] 

rm(r, l, z)

# Plot Model hit rates against empirical hit rates

# Plot 3 Hit rate against (1) Age group, (2) Sex, (3) Race. Use facet to split

# Define different age groups
df2$age.group = ifelse(df2$age.raw < 18, "Under 18",
                       ifelse((18 <= df2$age.raw & df2$age.raw <= 25), "18 to 25",
                              ifelse((26 <= df2$age.raw & df2$age.raw <= 32), "26 to 32",
                                     ifelse((33 <= df2$age.raw & df2$age.raw <= 40), "33 to 40",
                                            "Over 40")
                                     )
                              )
                       )

df %>% group_by(grp) %>% summarise_all(funs(mean)) # Summarise all three hit rates per group_by

