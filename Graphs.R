setwd("D:/Ling Ki/Uni/Master/HU-MEMS/Sem 3/Statistidal Programming Language/Project/Racial Disparities")

#*************************************
#Preparation
#*************************************

#Install packages
install.packages("arsenal")
install.packages("dplyr")
install.packages("foreign")
install.packages("plyr")
install.packages("tidyverse")
install.packages("ggmap")
install.packages("reshape")

#Load packages
library(arsenal)
library(base)
library(data.table)
library(dplyr)
library(foreign)
library(plyr)
library(lattice)
library(tidyverse)
library(ggmap)
library(reshape)

#*************************************
#Data Processing
#*************************************

#-----------------------
#1. Read Datasets
#-----------------------

#Frisk reports
ny13 = read.csv("2013.csv", header=TRUE)
ny14 = read.csv("2014.csv", header=TRUE)
ny15 = read.csv("2015.csv", header=TRUE)
ny16 = read.csv("2016.csv", header=TRUE)

#Homicide reports
all16  = readLines("cy2016.csv")  #Read in files as text
skip16 = all16[-c(1:4)]           #Dropping the first 4 rows
hmc16  = read.csv(textConnection(skip16), header=TRUE)

all17  = readLines("cy2017.csv")  
skip17 = all17[-c(1:4)] 
hmc17  = read.csv(textConnection(skip17), header=TRUE)

#-----------------------
#2. Create Datasets
#-----------------------

#Frisk reports  (Check: Subset several data frames with one command)
info = c("pct", "race", "crimsusp", "xcoord", "ycoord")                                                      #Create list of info for data selection
ny13 = subset(ny13, select=info,                                                                             #Select only relevant columns
               ny13$crimsusp %like% "CPW" | ny13$crimsusp %like%  "C.P.W" | ny13$crimsusp %like%  "WEAPON")  #Select only CPW data
ny14 = subset(ny14, select=info,  
               ny14$crimsusp %like% "CPW" | ny14$crimsusp %like%  "C.P.W" | ny14$crimsusp %like%  "WEAPON")
ny15 = subset(ny15, select=info,  
               ny15$crimsusp %like% "CPW" | ny15$crimsusp %like%  "C.P.W" | ny15$crimsusp %like%  "WEAPON")
ny16 = subset(ny16, select=info,  
               ny16$crimsusp %like% "CPW" | ny16$crimsusp %like%  "C.P.W" | ny16$crimsusp %like%  "WEAPON")
report = rbind(ny13, ny14, ny15, ny16)                                                                       #Create complete report

#Homicide
hmc17        = hmc17[-(293:1917), -(26:99)]               #Remove empty columns and rows
check_header = cbind(colnames(hmc16), colnames(hmc17))    #View(check_header)  #Checked, OK
check_type   = sapply(list(hmc16,hmc17), sapply, class)   #View(check_type)  #Checked, OK
hmc          = rbind(hmc16, hmc17)                        #Create complete dataset

#*************************************
#Overview
#*************************************

str(report)
unique(report$pct)        #Entries are normal
unique(report$race)       #12 levels of races. Need to combine / replace (?)
any(is.na(report$xcoord)) #TRUE
any(is.na(report$ycoord)) #TRUE

str(hmc)                  #Variable "PRECINCT" should be a string instead of integer
hmc$PRECINCT = as.character(hmc$PRECINCT)

#*************************************
#Data Visualization
#*************************************

#-----------------------
#1. CPW and Homicide
#-----------------------

#Preparation
pp_cpw           = count(report, "pct")                            #Count CPW by precinct
names(pp_cpw)[2] = "freq_cpw"
pp_cpw$pct_cpw   = pp_cpw$freq_cpw / sum(pp_cpw$freq_cpw) * 100

pp_hmc           = count(hmc,"PRECINCT")                           #Count homicide by precinct
names(pp_hmc)[2] = "freq_hmc"
pp_hmc$pct_hmc   = pp_hmc$freq_hmc / sum(pp_hmc$freq_hmc) * 100

joint            = merge.data.frame(pp_cpw, pp_hmc, by=intersect(names(pp_cpw), names(pp_hmc)), 
                                       by.x="pct", by.y="PRECINCT", all.x=TRUE, sort=TRUE)
joint[is.na(joint)] = 0                                            #Replace all NAs with 0
str(joint)                                                         #All normal
  
#Grouped Bar Plot
data  = joint %>%
          top_n(n=20,wt=pct_cpw) %>%
          arrange(desc(pct_cpw))
data  = rename(data, c("pct"="Precinct", "pct_cpw"="CPW", "pct_hmc"="Homicide"))
data  = data[,-c(2, 4)]

plot  = melt(data, id.vars='Precinct')
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

#-----------------------
#2. CPW and Race
#-----------------------
#Plot Map


#Remove unwanted files
rm(ny13, ny14, ny15, ny16, all16, skip16, homi16, all17, skip17, homi17, check_header, check_type)

#-----------------------------------------------------------------------
  
    #Individual plots
ggplot(to_plot, aes(x=pct, y=pct_cpw))+
  geom_bar(stat="identity")+theme(axis.text.x= element_text(angle=45, hjust=1)) + 
  labs(title='CPW stops per precinct')

ggplot(to_plot, aes(x=pct, y=pct_hmc))+
  geom_bar(stat="identity")+theme(axis.text.x= element_text(angle=45, hjust=1)) + 
  labs(title='Homicide per precinct')

#To Do
#Replace NAs by one entry before
report <- report[order(report$xcoord),]
report1 <- report
#!!!!!!!!!Can't run it
report1 %>% 
  #group_by(report$xcoord) %>% 
  #fill(report$xcoord) %>%
  fill(report$xcoord, .direction = "up")

