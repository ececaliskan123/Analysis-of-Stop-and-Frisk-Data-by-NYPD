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

#=============================
#Access models
#=============================
source("LoadPackages.R")
source("1.0_FirstSteps.R", local = FALSE) 
source("1.1_coordinates.R", local = FALSE)
df      = df[df$year!=min(unique(df$year)),]
hitRate = readRDS("hitRate.rds")
df      = cbind(df, hitRate)
colnames(df)[which(names(df)=="df[, \"hitRate\"]")] = "hitRate"
source("1.3_Cleaning.R", local = FALSE)

#----------------------------------------
source("2.2_Logit SGD.R", local = FALSE)


#----------------------------------------
source("2.3_RF.R", local = FALSE)


#----------------------------------------



