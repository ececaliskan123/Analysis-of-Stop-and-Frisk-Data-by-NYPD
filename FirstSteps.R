rm(list = ls()) #remove all current objects to clear workspace

# NEED FOR CHANGE - SET YOUR WORKING DIRECTORY!
ownwd <- "C:/Users/Malte/Desktop/Uni/SPL/nypd-stopfrisk" # not necessary if working with GitHub


# Prework
setwd(ownwd) # set working directory


# load annual datesets (previously converted into .rds format to reduce size)

nypd2013 <- readRDS("./Data-rds/sqf2013.rds")
nypd2014 <- readRDS("./Data-rds/sqf2014.rds")
nypd2015 <- readRDS("./Data-rds/sqf2015.rds")
nypd2016 <- readRDS("./Data-rds/sqf2016.rds")

# select relevant variables as stated in the paper:
# sex, race, build, trhsloc (housing or transit), inout (inside or outside), 
# year, datestop, timestop
# all vars beginning with  "cs_" = reason for stop
# radio; offunif; ht_feet ; ht_inch ; weight; age; perobs
# columns indicating whether a weapon was found: pistol, riflshot, asltweap, knifcuti, machgun, othrweap

weaponVars <- c("pistol", "riflshot", "asltweap", "knifcuti", "machgun", "othrweap")

covariates <- c("year","datestop","timestop","pct","sex","race","ht_feet", "ht_inch", "weight", "age", 
                "trhsloc","inout","offunif","perobs",grep("cs_",names(nypd2014), value=TRUE), "radio",
                "crimsusp", #crimsusp is needed to construct dependent variable
                weaponVars) # weaponVars necessary to construct dependent variable (= weapon found or not)
                

# to be done: (2) local hit rate as described below

# rename columns if necessary
names(nypd2016)[1] = "year"   # column had a wrong name (= i...year)

# combine all rows into one file 
df <- do.call(rbind,mget(ls(pattern = "nypd*")))   # combine all rows
rm(list=ls(pattern = "nypd*"))  # remove old files

# only keep relevant covariates
df <- subset(df,select=covariates)

# some post-processing (ht_feet & ht_inch to centimeters)
df$ht_feet <- df$ht_feet*30.48 # convert feet to cm
df$ht_inch <- df$ht_inch*2.54 #convert inch to cm
df$height <- df$ht_feet+df$ht_inch
df[,c("ht_feet","ht_inch")] <- NULL
range(df$height,na.rm = TRUE) #between 91.44 and 241.3 cm -> mistakes!?

#  transformation of variables if necessary

str(df) # have a look on classes of columns
df$age <- as.numeric(df$age) # age has to be numeric or integer instead of character

# NORMALIZATION 
# Paper: "the latter four are all normalized to have mean 0 and variance 1" 
# -> height, weight, age, perobs
normalizeVars <- c("height","weight","age","perobs")
df[,normalizeVars] <- scale(df[,normalizeVars]) # scale does the job
lapply(df[,normalizeVars], mean, na.rm=TRUE) #Verification: all means are (almost) zero

# -> we are only interested in "CPW"as suspected crime

StrToMatch <- c("CPW","C.P.W","WEAPON","GUN","FIREARM") # assign patterns that indicate "CPW", add more!
df$CPW <- ifelse(grepl(paste(StrToMatch,collapse="|"),df$crimsusp)==TRUE,1,0)
length(which(df$CPW==1)) #33527, correct!

# only keep df$CPW==1
df <- df[df$CPW==1,]

# dependent variable: weapon found or not (pistol, rifle, asltweap, knife, mchngun, other weapon)
df$weaponfound <- ifelse(df$pistol=="Y" | df$riflshot=="Y" | df$asltweap=="Y" | df$knifcuti=="Y" | df$machgun=="Y" | df$othrweap=="Y", 1,0)
  df[,c(weaponVars)] <- NULL  # no use for weaponVars anymore

# ... to be continued

saveRDS(df,file="nypd2013_2016.rds")

