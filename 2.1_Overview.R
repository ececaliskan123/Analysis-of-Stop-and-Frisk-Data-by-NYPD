#*************************************
#Preparation
#*************************************

#Install packages
install.packages("dplyr")
install.packages("foreign")
install.packages("formatR")
install.packages("plyr")
install.packages("ggplot")
install.packages("gridExtra")

#Load packages
library(base)
library(data.table)
library(dplyr)
library(foreign)
library(formatR)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(plyr)
library(sf)


# Source codes
source("1.0_FirstSteps.R", local = FALSE) 
source("1.1_coordinates.R", local = FALSE)
source("1.2_hitRate.R", local = FALSE)
144age     = df$age
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
  v[v > (mu + 3*sd)] = (mu + 3*sd)
  v[v < (mu - 3*sd)] = (mu - 3*sd)

  return(v)
}

age2    = lapply(df1[c("age")], FUN=fun1)
df1$age = as.integer(unlist(age2, use.names = FALSE))

#-------------------

range(df1$hitRate)    # 0 <= range <= 1, OK

rm(age2)

#*************************************
#Data Visualization
#*************************************

#=============================
#1. Trends across Time
#=============================

df1$ym = format(df1$formated_date, format = "%y/%m")
df1$ym = as.Date(parse_date_time(df1$ym, orders="%y/%m"))
caco   = df1 %>%
    group_by(ym) %>%
    dplyr::summarize("freq" = n(), "hr" = mean(hitRate))
range(caco$freq)   

# Plot case numbers against time

c = ggplot(caco, aes(ym, freq)) + 
  geom_point() + 
  stat_smooth(color = "dark blue", fill = "dark blue", method = "loess") +  
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%y") +
  coord_cartesian(ylim = c(0, 1000)) + 
  labs(x = "Month", y = "Count", title = "Monthly Case Number over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

h = ggplot(caco, aes(ym, hr)) + 
  geom_point() + 
  stat_smooth(color = "dark red", fill = "dark red", method = "loess") +  
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%y") +
  coord_cartesian(ylim = c(0, 0.2)) + 
  labs(x = "Month", y = "Hit Rate", title = "Monthly Hit Rate over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(c, h, nrow = 2)

#=============================
#2. Stops - Census Racial Compositions
#=============================


#=============================
#4. 
#=============================


#=============================
#5. 
#=============================


