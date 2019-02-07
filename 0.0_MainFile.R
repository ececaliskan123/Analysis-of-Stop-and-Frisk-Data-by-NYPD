rm(list = ls()) #remove all current objects to clear workspace
# **********************************
#       MAIN FILE 
# **********************************

# This File combines all code-snippets.

ownwd <- "C:/SPL_local" # your personal folder
setwd(ownwd) # set working directory
source("LoadPackages.R") # load all required packages

#####################################
#     1) PreProcessing
#####################################

# Get the Dataset & merging
source("FirstSteps.R")

# Covariates: HitRate
source("HitRate.R")

# Covariates: Pairwise Interaction Terms
source(...)

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

