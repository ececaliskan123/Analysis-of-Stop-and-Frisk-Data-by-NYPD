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
source("1.0_PreProcessing.R")  # -> output: df.rds    

# Transform Coordinates
source("1.1_coordinates.R")  # -> output: df.rds   

# Covariates: HitRate
# source("1.2_hitRate.R")    # -> the code works! just excluded for now to save time! -> output: df.rds

    # SHORT-CUT HIT-RATE
      df          = df[df$year!=min(unique(df$year)),]    # deletes year==2008 (only necessary to calculate hitRate2009)
      hitRateAll  = readRDS("./Data-rds/hitRate.rds")  # the results after time-consuming calculations
      hitRateAll$rowname = as.numeric(hitRateAll$rowname)
      df          = merge(df, hitRateAll, by="rowname")
      df       = df[order(df$year),]
      saveRDS(df, file="df.rds")


# Cleaning
source("1.3_Cleaning.R") 
      
#It's safe to ignore the  warning message of unknown timezone.
      
saveRDS(df,file="df.rds") 
rm(list = ls()[! ls() %in% "df"]) #remove everything except df from the global environment

########################################
#     2) ANALYSIS
########################################

source("2.2_Logit SGD.R") 

##########################################
#     3) PostProcessing
############################################

# this part produces relevant tables, maps & graphs

source("Graphs.R")

