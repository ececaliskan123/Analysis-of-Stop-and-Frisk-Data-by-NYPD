rm(list = ls()) #remove all current objects to clear workspace
# **********************************
#       MAIN FILE 
# **********************************

# This File combines all code-snippets.

source("LoadPackages.R") # load all required packages

#####################################
#     1) PreProcessing
#####################################

# Get the Dataset & merging
source("1.0_FirstSteps.R")  # -> output: df.rds    

# Transform Coordinates
source("1.1_coordinates.R")  # -> output: df.rds   

# Covariates: HitRate
# source("1.2_hitRate.R")    # -> the code works! just excluded for now to save time! -> output: df.rds

    # run this to save time:
    df          = df[df$year!=min(unique(df$year)),]    # deletes year==2008 (only necessary to calculate hitRate2009)
    hitRateAll  = readRDS("./Data-rds/hitRate.rds")  # the results after time-consuming calculations
    df          = cbind(df, hitRateAll)
                colnames(df)[which(names(df)=="df[, \"hitRate\"]")] = "hitRate" #rename column
                rm(list = ls()[! ls() %in% "df"]) #remove everything except df from the global environment


# Cleaning
source("Cleaning.R")

########################################
#     2) ANALYSIS
########################################

# - logistic regression
# - logistic regression with SDG (stochastic gradient descent) to deal with 7k regressors

##########################################
#     3) PostProcessing
############################################

# this part produces relevant tables, maps & graphs

source("Graphs.R")

