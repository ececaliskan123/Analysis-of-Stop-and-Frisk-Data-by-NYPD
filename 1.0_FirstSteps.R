# LOAD ANNUAL DATASETS

  # (previously converted into .rds format to reduce size)
  
  nypd2012 <- readRDS("./Data-rds/sqf2012.rds")
  nypd2013 <- readRDS("./Data-rds/sqf2013.rds")
  nypd2014 <- readRDS("./Data-rds/sqf2014.rds")
  nypd2015 <- readRDS("./Data-rds/sqf2015.rds")
  nypd2016 <- readRDS("./Data-rds/sqf2016.rds")
  
# COMBINE AND SUBSET DATA FRAME

  # select relevant variables as stated in the paper:
  # sex, race, build, trhsloc (housing or transit), inout (inside or outside), 
  # year, datestop, timestop
  # all vars beginning with  "cs_" = reason for stop
  # radio; offunif; ht_feet ; ht_inch ; weight; age; perobs
  # columns indicating whether a weapon was found: pistol, riflshot, asltweap, knifcuti, machgun, othrweap
  
  weaponVars = c("pistol", "riflshot", "asltweap", "knifcuti", "machgun", "othrweap")
  
  covariates = c("year","datestop","timestop","pct","sex","race","ht_feet", 
                  "ht_inch", "weight", "age", "trhsloc","inout","offunif",
                  "perobs",grep("cs_",names(nypd2014), value=TRUE), "radio",
                  "crimsusp", #crimsusp is needed to construct dependent variable
                  weaponVars, # weaponVars necessary to construct dependent variable (= weapon 1 or 0)
                  "xcoord", "ycoord")
  
  # manual detecting if column names differ
  
  # rename columns if necessary
  names(nypd2016)[1]                              = "year"   # column had a wrong name (= i...year)
  names(nypd2012)[c(grep("cm$",names(nypd2012)))] = gsub("cm","CM",names(nypd2012)[c(grep("cm$",names(nypd2012)))])
    
  # combine all rows into one file 
  df = do.call(rbind,mget(ls(pattern = "nypd*")))   # combine all rows
        rm(list=ls(pattern = "nypd*"))             # remove old files
  df = subset(df,select=covariates)               # only keep relevant covariates
  
  
  # Subset data (only "CPW" as suspected crime)
  
  StrToMatch  = c("CPW","C.P.W","WEAPON","GUN","FIREARM") # assign patterns that indicate "CPW", add more!
  df$CPW      = ifelse(grepl(paste(StrToMatch,collapse="|"),df$crimsusp)==TRUE,1,0)  # dummy var
  df          = df[df$CPW==1,] # only keep CPW as suspected crime
  
  # Create DEPENDENT VARIABLE (weapon found 1/0)
        # respective columns: pistol, rifle, asltweap, knife, mchngun, other weapon
  
  df$weaponfound      = ifelse(df$pistol=="Y" | df$riflshot=="Y" | df$asltweap=="Y" | df$knifcuti=="Y" | df$machgun=="Y" | df$othrweap=="Y", 1,0)
  df[,c(weaponVars)]  = NULL  #lete no use for weaponVars anymore

# SOME POST-PROCESSING

  # Convert feet & inches into centimers
  df$height                   = df$ht_feet*30.48+df$ht_inch*2.54
  df[,c("ht_feet","ht_inch")] = NULL

  df$age                      = as.numeric(df$age) # age has to be numeric or integer instead of character
  
  
  
              #lapply(df[,normalizeVars], mean, na.rm=TRUE) #Verification: all means are (almost) zero
  
  # SAVE FILE FOR FURTHER PROCESSING (cleaning etc.)

  saveRDS(df,file="df.rds")
