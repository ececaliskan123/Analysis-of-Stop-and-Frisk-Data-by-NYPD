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


