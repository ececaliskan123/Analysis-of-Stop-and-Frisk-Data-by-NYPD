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

# Define a function for standardization
standardize <- function(x){
  
  mu <- mean(x)
  std <- sd(x)
  result <- (x - mu)/std
  
  return(result)   }

# Outlier treatment

boxplot(df$weight)
boxplot(df$height)
boxplot(df$perobs)

#  Replace the assumed outliers in standardized age, weight, height and perobs with a threshold value.

df$age [df$age > 3] <- mean(df$age) + 3*sd(df$age)
df$weight [df$weight > 3] <- mean(df$weight) + 3*sd(df$weight)
df$height [df$height > 3] <- mean(df$height) + 3*sd(df$height)
df$perobs [df$perobs > 3] <- mean(df$perobs) + 3*sd(df$perobs)


 # Month variable
install.packages("lubridate")
library(lubridate)
df$month <- month(dmy(df$datestop))

# Weekday variable

install.packages("anytime")
as.character(df$datestop)
date <- anytime:: anydate (df$datestop)
df$weekday <- wday(date, label=TRUE)

# Time of the day variable

time <- anytime:: anydate (df$timestop)



# Standardizing the entries for reasons for stops 

df$cs_objcs [df$cs_objcs == ""] <- "N"
df$cs_objcs [df$cs_objcs == "1"] <- "Y"
df$cs_descr [df$cs_descr == ""] <- "N"
df$cs_descr [df$cs_descr == "1"] <- "Y"
df$cs_casng [df$cs_casng == ""] <- "N"
df$cs_casng [df$cs_casng == "1"] <- "Y"
df$cs_lkout [df$cs_lkout == ""] <- "N"
df$cs_lkout [df$cs_lkout == "1"] <- "Y"
df$cs_cloth [df$cs_cloth == ""] <- "N"
df$cs_cloth [df$cs_cloth == "1"] <- "Y"
df$cs_drgtr [df$cs_drgtr == ""] <- "N"
df$cs_drgtr [df$cs_drgtr == "1"] <- "Y"
df$cs_furtv [df$cs_furtv == ""] <- "N"
df$cs_furtv [df$cs_furtv == "1"] <- "Y"
df$cs_vcrim [df$cs_vcrim == ""] <- "N"
df$cs_vcrim [df$cs_vcrim == "1"] <- "Y"
df$cs_bulge [df$cs_bulge == ""] <- "N"
df$cs_bulge [df$cs_bulge == "1"] <- "Y"
df$cs_other [df$cs_other == ""] <- "N"
df$cs_other [df$cs_other == "1"] <- "Y"


###########################################
#     3) ANALYSIS
############################################

# logistic regression
# to be done....

##########################################
#     4) PostProcessing
############################################

# plots, tables, nice output etc


