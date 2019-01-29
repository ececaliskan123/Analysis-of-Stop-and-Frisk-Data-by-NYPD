# **********************************
#       MAIN FILE 
# **********************************

# This File combines all code-snippets.

ownwd <- "C:/SPL_local" # your personal folder
setwd(ownwd) # set working directory
source("LoadPackages.R") # load packages

#####################################
#     1) PreProcessing
#####################################

# Get the Dataset & merging
source("FirstSteps.R")

# Calculate the HitRate
source("HitRate.R")

# Create InteractionTerms
source(....)

########################################
#     2) CLEANING
########################################
# Check and correct for missing values

colSums(is.na.data.frame(df))
sort(table(df$age)) # -0.317192507 is the most frequent value for standardized age.
df$age [is.na(df$age)] <- -0.317192507

# Calculate the z-score with the function for standardization
standardize <- function(x){
  
  mu <- mean(x)
  std <- sd(x)
  result <- (x - mu)/std
  
  return(result)   }

# Outlier treatment

#  Replace the assumed outliers in standardized age, weight, height and perobs with a threshold value.

df$age [df$age > 3] <- mean(df$age) + 3*sd(df$age)
df$weight [df$weight > 3] <- mean(df$weight) + 3*sd(df$weight)
df$height [df$height > 3] <- mean(df$height) + 3*sd(df$height)
df$perobs [df$perobs > 3] <- mean(df$perobs) + 3*sd(df$perobs)


 # Month variable
install.packages("lubridate")
library(lubridate)
df$month <- month(dmy(df$datestop))

# Day variable

# Time of the day variable
# missValues & Outliers

# to be done!

###########################################
#     3) ANALYSIS
############################################

# logistic regression
# to be done....

##########################################
#     4) PostProcessing
############################################

# plots, tables, nice output etc


