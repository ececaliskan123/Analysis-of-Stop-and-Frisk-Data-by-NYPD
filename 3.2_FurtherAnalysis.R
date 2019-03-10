#*************************************
#Preparation
#*************************************

#Install packages
install.packages("dplyr")
install.packages("foreign")
install.packages("formatR")
install.packages("plyr")
install.packages("ggplot")

#Load packages
library(base)
library(data.table)
library(foreign)
library(formatR)
library(ggplot2)


# Source codes
source("1.0_FirstSteps.R", local = FALSE) 
source("1.1_coordinates.R", local = FALSE)
source("1.2_hitRate.R", local = FALSE)
age     = df$age
source("1.3_Cleaning.R", local = FALSE)
df$age  = age

#*************************************
#Data Processing
#*************************************
#=============================
#1. Create Dataset
#=============================
source("1.0_FirstSteps.R", local = FALSE) 
source("1.1_coordinates.R", local = FALSE)

df      = df[df$year!=min(unique(df$year)),]
hitRate = readRDS("./Data-rds/hitRate")
df      = cbind(df, hitRate)
colnames(df)[which(names(df)=="df[, \"hitRate\"]")] = "hitRate"

age     = df$age

source("1.3_Cleaning.R", local = FALSE)
df$age  = age
#-----------------------
cv      = c("year", "formated_date", "pct", "race", "age", "sex", "weaponfound", "hitRate")
yr      = c(2013, 2014, 2015, 2016)
df1     = df %>% 
    select(cv) %>% 
    filter(year %in% yr) 

#=============================
#2. Further Cleaning
#=============================

str(df1)
df1$pct  = as.character(df1$pct)
df1$race = factor(df1$race)

table(df1$sex)
    #F     M     Z       
    #1208 31379   428     0 
df1      = df1[(df1$sex == "F" | df1$sex == "M"),]    #Filter unknown sex


#Remove age outliers 
summary(df1$age)
df1$age[is.na(df1$age)] = order(table(df1$age), decreasing = TRUE)[1]  #Replace NA with mode

fun1 = function(v) {
  
  mu = mean(v)
  sd = sd(v)
  uf = min((mu + 3*sd), 100)
  lf = max((mu - 3*sd), 12)
  v[v > (mu + 3*sd)] = uf
  v[v < (mu - 3*sd)] = lf

  return(v)
}

age2    = lapply(df1[c("age")], FUN=fun1)
df1$age = unlist(age2, use.names = FALSE)

#-------------------

range(df1$hitRate)    # 0 <= range <= 1, OK


#*************************************
#Data Visualization
#*************************************

#=============================
#1. Total Number of Stops over Time
#=============================



#=============================
#2. Total Case Number of Weapon Found
#=============================


#=============================
#3. Stops - Census Racial Compositions
#=============================


#=============================
#4. 
#=============================


#=============================
#5. 
#=============================


