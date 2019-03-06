setwd("D:/Ling Ki/Uni/Master/HU-MEMS/Sem 3/Statistidal Programming Language/Project/Racial Disparities")

#*************************************
#Preparation
#*************************************

#Install packages
install.packages("dplyr")
install.packages("foreign")
install.packages("plyr")
install.packages("ggmap")
install.packages("sf")

#Load packages
library(base)
library(data.table)
library(foreign)
library(ggmap)
library(ggplot2)
library(sf)

#*************************************
#Data Processing
#*************************************

#-----------------------
#1. Read Datasets
#-----------------------

ny13 = read.csv("2013.csv", header=TRUE)
ny14 = read.csv("2014.csv", header=TRUE)
ny15 = read.csv("2015.csv", header=TRUE)
ny16 = read.csv("2016.csv", header=TRUE)

#-----------------------
#2. Create Datasets
#-----------------------

info   = c("pct", "race", "crimsusp", "xcoord", "ycoord")  # Create list of info for data selection
ny13   = subset(ny13, select=info,                         # Select only relevant columns
              ny13$crimsusp %like% "CPW" | ny13$crimsusp %like%  "C.P.W" | ny13$crimsusp %like%  "WEAPON") # Select only CPW data
ny14   = subset(ny14, select=info,  
              ny14$crimsusp %like% "CPW" | ny14$crimsusp %like%  "C.P.W" | ny14$crimsusp %like%  "WEAPON")
ny15   = subset(ny15, select=info,  
              ny15$crimsusp %like% "CPW" | ny15$crimsusp %like%  "C.P.W" | ny15$crimsusp %like%  "WEAPON")
ny16   = subset(ny16, select=info,  
              ny16$crimsusp %like% "CPW" | ny16$crimsusp %like%  "C.P.W" | ny16$crimsusp %like%  "WEAPON")
report = rbind(ny13, ny14, ny15, ny16)                   # Create complete report

#Remove unwanted files
rm(ny13, ny14, ny15, ny16)

#*************************************
#Overview & Processing
#*************************************

str(report)
any(is.na(report$race))     # False
unique(report$race)         # Need further processing
any(is.na(report$xcoord))   # True
any(is.na(report$ycoord))   # True

#-------------------
#Race
#-------------------

table(report$race)
#A     B     I     P     Q     U     W     Z                 M      T
#460 21411    89  2386  6999   187  1577   604     8     0     0    0

report$race [report$race == "I"] = "Z"
report$race [report$race == "P"] = "Q"
report$race [report$race == "U"] = "Z"
report$race [report$race == ""]  = "Z"

report$race = factor(report$race)
table(report$race)
#A     B     Q     W     Z 
#460 21411  9385  1577   888

#-------------------
#Coordinates
#-------------------

report      = report[!is.na(report$xcoord) & !is.na(report$ycoord),]    # Deletes NAs
report      = st_as_sf(report,coords=c("xcoord","ycoord"),crs=102718)   # Define spatial data, "102718" is the code for the US state planning

report      = st_transform(report, crs=32610)                           # Transform to UTM format
report$utmE = st_coordinates(report)[,1]                                # Save column UTM East
report$utmN = st_coordinates(report)[,2]                                # Save column UTM West
report      = st_transform(report, crs=4326)                            # Switch to long/lat format
report$long = st_coordinates(report)[,1]                                # Add longitude (= x value)
report$lat  = st_coordinates(report)[,2]                                # Add latitude (= y value)

report      = st_set_geometry(report, NULL)                             # Remove the spatial component

#*************************************
#Plotting map
#*************************************

# Note: user needs to register at Google Cloud Platform for a free API key

register_google(key = "AIzaSyDke5EmHEXGoXkNvL76Ks4TL1tLtSKYqkQ")    #Set up API key to access Google Map for download

NYC = get_map(location = c(lon = -73.90, lat = 40.71), source = "google", maptype = "terrain", zoom = 11)

ggmap(NYC) + 
    geom_point(data=report, mapping = aes(x=long, y=lat, color=race), size=1) + 
    ggtitle("Distribution of CPW Stops Between 2013 and 2016")
