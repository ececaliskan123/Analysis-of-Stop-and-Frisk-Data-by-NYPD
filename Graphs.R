setwd("D:/Ling Ki/Uni/Master/HU-MEMS/Sem 3/Statistidal Programming Language/Project/Racial Disparities")


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


#Read in datasets
  #Frisk reports
ny13 <- read.csv("2013.csv", header=TRUE)
ny14 <- read.csv("2014.csv", header=TRUE)
ny15 <- read.csv("2015.csv", header=TRUE)
ny16 <- read.csv("2016.csv", header=TRUE)
  #Homicide reports
all16 <- readLines("cy2016.csv")  #Read-in files as text
skip16 <- all16[-c(1:4)] #Dropping the first 4 rows of table
homi16 <- read.csv(textConnection(skip16), header=TRUE)

all17 <- readLines("cy2017.csv")  
skip17 <- all17[-c(1:4)] 
homi17 <- read.csv(textConnection(skip17), header=TRUE)


#Create datasets
  #Frisk reports
info <- c("pct", "race", "crimsusp", "xcoord", "ycoord")  #Create list of info for data selection
ny13 <- subset(ny13, select=info,  #Select only relevant columns
               ny13$crimsusp %like% "CPW" | ny13$crimsusp %like%  "C.P.W" | ny13$crimsusp %like%  "WEAPON") #Select only CPW data
ny14 <- subset(ny14, select=info,  
               ny14$crimsusp %like% "CPW" | ny14$crimsusp %like%  "C.P.W" | ny14$crimsusp %like%  "WEAPON")
ny15 <- subset(ny15, select=info,  
               ny15$crimsusp %like% "CPW" | ny15$crimsusp %like%  "C.P.W" | ny15$crimsusp %like%  "WEAPON")
ny16 <- subset(ny16, select=info,  
               ny16$crimsusp %like% "CPW" | ny16$crimsusp %like%  "C.P.W" | ny16$crimsusp %like%  "WEAPON")
report <- rbind(ny13, ny14, ny15, ny16) #Create complete report

  #Homicide
homi17 <- homi17[-(293:1917),-(26:99)]  #Remove empty columns and rows
check_header <- cbind(colnames(homi16), colnames(homi17))  #View(check_header)  #Checked, OK
check_type <- sapply(list(homi16,homi17), sapply, class)  #View(check_type)  #Checked, OK
homicide <- rbind(homi16, homi17)  #Create complete dataset


#Data overview
str(report)
unique(report$pct)  #Entries are normal
unique(report$race) #12 levels of races. Need to combine / replace (?)
any(is.na(report$xcoord)) #TRUE, there is NA
any(is.na(report$ycoord)) #Also TRUE, there is NA


str(homicide) #Variable "PRECINCT" should be a string instead of integer
homicide$PRECINCT <- as.character(homicide$PRECINCT)


#Plot Graphs - CPW and Homicide
  #Preparation
pp_cpw <- count(report, "pct")  #Count CPW by precinct
names(pp_cpw)[2] <- "freq_cpw"
pp_cpw$pct_cpw <- pp_cpw$freq_cpw / sum(pp_cpw$freq_cpw) * 100

pp_homicide <- count(homicide,"PRECINCT")  #Count homicide by precinct
names(pp_homicide)[2] <- "freq_hmc"
pp_homicide$pct_hmc <- pp_homicide$freq_hmc / sum(pp_homicide$freq_hmc) * 100

joint <- merge.data.frame(pp_cpw, pp_homicide, by=intersect(names(pp_cpw), names(pp_homicide)),
                        by.x = "pct", by.y = "PRECINCT", all.x = TRUE, sort = TRUE)
joint[is.na(joint)]<-0  #Replace all NAs with 0
str(joint)  #All normal
  
  #Plot grouped bar plot
to_plot <- joint%>%
  top_n(n=77,wt=pct_cpw)%>%
  arrange(desc(pct_cpw))

to_plot$freq_cpw <- NULL
to_plot$freq_hmc <- NULL

data <- melt(to_plot, id.vars='pct')

ggplot(data, aes(x = pct, y = value, fill = variable)) + 
  geom_bar(position = "dodge", stat = "identity") + theme(axis.text.x= element_text(angle=45, hjust=1)) + 
  labs(title='Distribution of CPW versus Homicide')
  
    #Individual plots
ggplot(to_plot, aes(x=pct, y=pct_cpw))+
  geom_bar(stat="identity")+theme(axis.text.x= element_text(angle=45, hjust=1)) + 
  labs(title='CPW stops per precinct')

ggplot(to_plot, aes(x=pct, y=pct_hmc))+
  geom_bar(stat="identity")+theme(axis.text.x= element_text(angle=45, hjust=1)) + 
  labs(title='Homicide per precinct')


#Plot Map


#Remove unwanted files
rm(ny13, ny14, ny15, ny16, all16, skip16, homi16, all17, skip17, homi17, check_header, check_type)

#-----------------------------------------------------------------------
#To Do
#Replace NAs by one entry before
report <- report[order(report$xcoord),]
report1 <- report
#!!!!!!!!!Can't run it
report1 %>% 
  #group_by(report$xcoord) %>% 
  #fill(report$xcoord) %>%
  fill(report$xcoord, .direction = "up")

