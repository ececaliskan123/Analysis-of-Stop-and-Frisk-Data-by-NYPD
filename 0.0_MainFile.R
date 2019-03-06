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

# Covariates: HitRate & InteractionTerms
source("1.2_hitRate.R")    # -> output: df.rds (the current state of play doesn't calculate the hitRate for 2013, 
#                           since this is only a dirty solution so far which takes pretty long. However,
#                           it is included in the current "df.rds" in the GitHub folder.)

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

