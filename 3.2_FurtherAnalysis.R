#*************************************
#Preparation
#*************************************

source("LoadPackages.R")

source("1.0_FirstSteps.R", local = FALSE) 
source("1.1_coordinates.R", local = FALSE)
source("1.2_hitRate.R", local = FALSE)
source("1.3_Cleaning.R", local = FALSE)

df2 = readRDS("To3.2.rds")
df2$sex  = factor(df2$sex)
str(df2)

#*************************************
#Model comparison
#*************************************
source("LoadPackages.R")
source("1.0_FirstSteps.R", local = FALSE) 
source("1.1_coordinates.R", local = FALSE)
df      = df[df$year!=min(unique(df$year)),]
hitRate = readRDS("hitRate.rds")
df      = cbind(df, hitRate)
colnames(df)[which(names(df)=="df[, \"hitRate\"]")] = "hitRate"
source("1.3_Cleaning.R", local = FALSE)

saveRDS(df, file = "df_cleaned.rds")

df2 = readRDS("To3.2.rds")
df2$sex  = factor(df2$sex)
str(df2)
  
#----------------------------------------
plot(weaponfound~hitRate, data=df)
#---------------------------------------
#Random Forest
#---------------------------------------

source("2.3_RF.R", local = FALSE)
auc(test$weaponfound, yhat)   #0.5512

predict_rfprob = predict(rf, train, type = "prob")[, 2]

timespan = c(2013, 2014, 2015, 2016)
name     = c(names(train))
rfdf     = subset(df, year %in% timespan, select = c(names(train)))

# Match datatype with training data
sapply(list(rfdf,train), sapply, class)
rfdf[sapply(rfdf, is.character)] = lapply(rfdf[sapply(rfdf, is.character)],as.factor)
rfdf$weaponfound                 = as.factor(rfdf$weaponfound)


#Using test data. Match datatype and levels
sapply(list(test,train), sapply, class)
test$weaponfound                 = as.factor(test$weaponfound)
test[sapply(test, is.character)] = lapply(test[sapply(test, is.character)],as.factor)
str(test)
str(train)


# Match levels with training data

rf.cdf         = ecdf(rfdf$hitRate)

#----------------------------------------
source("2.2_Logit SGD.R", local = FALSE)

#auc(test$weaponfound, yhat)   #    , higher / lower than RF



#*************************************
#Model check
#*************************************

ehr.age = 
  
ehr.sex = 
  



