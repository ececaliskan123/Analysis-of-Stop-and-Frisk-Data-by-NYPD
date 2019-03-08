setwd("D:/Ling Ki/Uni/Master/HU-MEMS/Sem 3/Statistidal Programming Language/Project/Racial Disparities")

#*************************************
#Preparation
#*************************************

#Install packages
install.packages("arsenal")
install.packages("anytime")
install.packages("dplyr")
install.packages("devtools")
install.packages("foreign")
install.packages("lubridate")
install.packages("plyr")
install.packages("tidyverse")
install.packages("ggmap")
install.packages("reshape")
install.packages("RCurl")
install.packages("sf")

#Load packages
library(arsenal)
library(anytime)
library(base)
library(data.table)
library(devtools)
library(dplyr)
library(foreign)
library(plyr)
library(lattice)
library(lubridate)
library(tidyverse)
library(ggmap)
library(reshape)
library(RCurl)
library(sf)

# Read annual reports
source("1.0_FirstSteps.R", local = FALSE) 

# Clean annual report
source("Cleaning.R", local = FALSE)

# Process coordinates
source("1.1_coordinates.R", local = FALSE)

#*************************************
#Data Processing
#*************************************

#=============================
#1. Read in Homicide Reports
#=============================

# Read homicide reports
hmc16        = readLines("cy2016.csv")                                   #Read in files as text
hmc16        = read.csv(textConnection(hmc16[-c(1:4)]), header=TRUE)     #Dropping the first 4 rows, covert to dataset

hmc17        = readLines("cy2017.csv")  
hmc17        = read.csv(textConnection(hmc17[-c(1:4)] ), header=TRUE)

# Create integrated homicide report
hmc17        = hmc17[-(293:1917), -(26:99)]                              #Remove empty columns and rows
check_header = cbind(colnames(hmc16), colnames(hmc17))                   #View(check_header)  #Checked, OK
check_type   = sapply(list(hmc16,hmc17), sapply, class)                  #View(check_type)  #Checked, OK
hmc          = rbind(hmc16, hmc17)                                       #Create complete dataset

rm(hmc16, hmc17, check_header, check_type)

#=============================
#2. Process Datasets
#=============================

# Overview
variates     = c("year", "pct", "sex", "race", "long", "lat")
report       = subset(df, select = variates)
str(report)

report$pct   = as.character(report$pct)     #Variable "PRECINCT" should be a string instead of integer

unique(report$year)                         #Entries are normal
unique(report$pct)                          #Entries are normal
unique(report$race)                         #12 levels of races. Need to combine / replace
#------------
# Clean race

table(report$race)
#A     B     I     P     Q     U     W     Z       
#1152 57692   209  6944 21277   734  4061  1774     0 

report$race[report$race == "I"] = "Z"
report$race[report$race == "P"] = "Q"
report$race[report$race == "U"] = "Z"
report$race[report$race == ""]  = "Z"

report$race = factor(report$race)
table(report$race)
#A     B     Q     W     Z 
#1152 57692 28221  4061  2717 
#------------

any(is.na(report$long))                     #FALSE
any(is.na(report$lat))                      #FALSE

str(hmc)                                    
hmc$PRECINCT = as.character(hmc$PRECINCT)   #Variable "PRECINCT" should be a string instead of integer

#*************************************
#Data Visualization
#*************************************

#=============================
#1. CPW and Homicide
#=============================

#Preparation
pp_cpw              = count(report, pct)                              #Count CPW by precinct
names(pp_cpw)[2]    = "freq_cpw"
pp_cpw$pct_cpw      = pp_cpw$freq_cpw / sum(pp_cpw$freq_cpw) * 100

pp_hmc              = as.data.frame(table(hmc$PRECINCT))              #Count homicide by precinct
#pp_hmc              = plyr::rename(pp_hmc, c("Var1"=="pct", "Freq"=="freq_hmc"))
names(pp_hmc)[1]    = "pct"
names(pp_hmc)[2]    = "freq_hmc"
pp_hmc$pct_hmc      = pp_hmc$freq_hmc / sum(pp_hmc$freq_hmc) * 100

joint               = merge.data.frame(pp_cpw, pp_hmc, by=intersect(names(pp_cpw), names(pp_hmc)), 
                                       by.x="pct", by.y="pct", all.x=TRUE, sort=TRUE)
joint[is.na(joint)] = 0                                               #Replace all NAs with 0
str(joint)                                                            #All normal

#Grouped Bar Plot
dt1   = joint %>%
            top_n(n=20,wt=pct_cpw) %>%
            arrange(desc(pct_cpw))
dt1   = dt1[,-c(2, 4)]
dt1   = plyr::rename(dt1, c("pct"="Precinct", "pct_cpw"="CPW", "pct_hmc"="Homicide"))

plot  = melt(dt1, id.vars='Precinct')
plot  = rename(plot, c("value"="Percentage"))

order = plot %>%
  filter(variable=="Homicide") %>%
  arrange(desc(Percentage)) %>%
  .$Precinct %>% as.character

ggplot(plot, aes(x = Precinct, y = Percentage, fill = variable)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  scale_x_discrete(limits=order) + 
  xlab("Precinct") +
  theme(axis.text.x= element_text(angle=45, hjust=1)) + 
  labs(title='Distribution of CPW versus Homicide')

#=============================
#2. CPW and Race
#=============================

# Preparation
dt2 = filter(report, year=="2014" | year=="2015" | year=="2016")

# First, user needs to register at Google Cloud Platform for a free API key
register_google(key = "AIzaSyDke5EmHEXGoXkNvL76Ks4TL1tLtSKYqkQ")    #Set up API key to access Google Map for download

NYC = get_map(location = c(lon = median(dt2$long), lat = median(dt2$lat), source = "google", maptype = "terrain", zoom = 11)

ggmap(NYC) + 
  geom_point(data=dt2, mapping = aes(x=long, y=lat, color=race), size=1) + 
  ggtitle("Distribution of CPW Stops Between 2014 and 2016")
