rm(list = ls()) #remove all current objects to clear workspace
# **********************************
#       MAIN FILE 
# **********************************

# This File combines all code-snippets.

source("LoadPackages.R") # load all required packages

#####################################
#     1) Quantlet: PreProcessing
#####################################
# Author: Malte Hessenius

# Get the Dataset & merging
source("./PreProcessing/PreProcessing.R")  # -> output: df.rds    

#####################################
#     2) Quantlet: Cleaning
#####################################
# Author: Ece Caliskan
source("./Cleaning/Cleaning.R")  # -> output: df.rds (now cleaned)


rm(list = ls()[! ls() %in% "df"]) #remove everything except df from the global environment

########################################
#     3) Quantlet: Exploratory
########################################
# Author: LingKi Wong
source("./Exploratory/Exploratory.R")


########################################
#     4) Quantlet: Replications
########################################
# Author: LingKi Wong
source("./Replications/Replications")


########################################
#     5) Quantlet: Logit
########################################
# Author: Ece Caliskan
source("./Logit/Logit.R")

##########################################
#     6) Quantlet: RandomForest
############################################
# Author: Malte Hessenius
source("./RandomForest/RandomForest.R")


##########################################
#     7) Quantlet: ModelAssessment
############################################
# Author: LingKi Wong
source("./ModelAssessment/ModelAssessment.R")

