rm(list = ls()) #remove all current objects to clear workspace
# **********************************
#       MAIN FILE 
# **********************************

# This File combines all code-snippets.

source("LoadPackages.R") # load all required packages

#####################################
#     1) Quantlet: PreProcessing
#####################################


# Get the Dataset & merging
source("./PreProcessing/PreProcessing.R")  # -> output: df.rds    

#####################################
#     2) Quantlet: Cleaning
#####################################

source("./Cleaning/Cleaning.R")  # -> output: df.rds (now cleaned)


rm(list = ls()[! ls() %in% "df"]) #remove everything except df from the global environment

########################################
#     3) Quantlet: Exploratory
########################################

source("./Exploratory/Exploratory.R")


########################################
#     4) Quantlet: Replications
########################################

source("./Replications/Replications.R")


########################################
#     5) Quantlet: Logit

source("./Logit/Logit.R")

##########################################
#     6) Quantlet: RandomForest
############################################

source("./RandomForest/RandomForest.R")


##########################################
#     7) Quantlet: ModelAssessment
############################################

source("./ModelAssessment/ModelAssessment.R")

