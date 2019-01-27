rm(list = ls()) #remove all current objects to clear workspace

# NEED FOR CHANGE - SET YOUR WORKING DIRECTORY!
ownwd <- "C:/SPL_local" # your personal folder


# Prework
setwd(ownwd) # set working directory


# load annual datesets

nypd2013 <- read.csv("./Data/sqf-2013.csv")
nypd2014 <- read.csv("./Data/sqf-2014.csv")
nypd2015 <- read.csv("./Data/sqf-2015.csv")
nypd2016 <- read.csv("./Data/sqf-2016.csv")

# select relevant variables:
# sex, race, build, trhsloc (housing or transit), inout (inside or outside), 
# year, datestop, timestop
# all vars beginning with  "cs_" = reason for stop
# radio; offunif; ht_feet ; ht_inch ; weight; age; perobs

covariates <- c("year","datestop","timestop","pct","sex","race","ht_feet", "ht_inch", "weight", "age", 
                "trhsloc","inout","offunif","perobs",grep("cs_",names(nypd2014), value=TRUE),
                "crimsusp")  #crimsusp is needed to construct dependent variable

# to be done: (2) local hit rate as described below

## rename columns if necessafry
names(nypd2016)[1] = "year"   # column had a wrong name (= i...year)

# only keep relevant covariates and apply rowbind
nypd2013 <- subset(nypd2013,select=covariates)
nypd2014 <- subset(nypd2014,select=covariates)
nypd2015 <- subset(nypd2015,select=covariates)
nypd2016 <- subset(nypd2016,select=covariates)

df <- do.call(rbind,mget(ls(pattern = "nypd*")))   # combine all rows
rm(list=ls(pattern = "nypd*"))  # remove old files

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

# dependent variable -> we are only interested in "CPW"as suspected crime

StrToMatch <- c("CPW","C.P.W","WEAPON") # assign patterns that indicate "CPW"
df$CPW <- ifelse(grepl(paste(StrToMatch,collapse="|"),df$crimsusp)==TRUE,1,0)
length(which(df$CPW==1)) #33527, correct!

# only keep df$CPW==1
df <- df[df$CPW==1,]

# validate, whether 33k are realistic: (the paper mentions 300k CPW stops for 09&2010)
# nypd2009 <- read.csv("./Data/sqf-2009.csv")
# length(which(grepl("CPW",nypd2009$crimsusp)==TRUE)) #93k (out of 581k)
# nypd2010 <- read.csv("./Data/sqf-2010.csv")
# length(which(grepl("CPW",nypd2010$crimsusp)==TRUE)) #85k (out of 601k)
# 
# # how did the authors come up with 301k CPW stops?
# # -> recording is not uniform!  
# # see examples below
# 
# length(which(grepl("C.P.W",nypd2010$crimsusp)==TRUE)) #1722 
# length(which(grepl("CRIMINAL POSSESSION WEAPON",nypd2010$crimsusp)==TRUE)) #117
# length(which(grepl("WEAPON",nypd2010$crimsusp)==TRUE)) #504

# ... to be continued

saveRDS(df,file="nypd2013_2016.rds")
write.csv(df,file="nypd2013_2016.csv")
