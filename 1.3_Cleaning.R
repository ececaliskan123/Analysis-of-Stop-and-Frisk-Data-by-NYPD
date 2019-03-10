# **********************************
#       CLEANING
#
# This file addresses missing values and treats outliers.
# **********************************
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("anytime")) install.packages("anytime"); library("anytime")


# Check and correct for missing values with Most Frequent Valaue(Mode)

colSums(is.na.data.frame(df)) # Missing values in Age are spotted.
MFV <- as.numeric(names(sort(table(df$age), decreasing=TRUE)[1])) 
df$age [is.na(df$age)] <- MFV 

# There are empty entries in Inside or Outside. Set them to NA and then impute
df$inout [df$inout == " "] <-NA
MFV2 <- names(sort(table(df$inout), decreasing=TRUE)[1]) 
df$inout [is.na(df$inout)] <- MFV2 



# Define a function for standardization

standardize <- function(x){
  
  mu <- mean(x)
  std <- sd(x)
  result <- (x - mu)/std
  
  return(result)   }

df[,c("weight", "height", "age")] <- apply(df[,c("weight", "height","age")], MARGIN=2, FUN=standardize)


# Outlier check

boxplot(df [, c("weight", "height", "perobs" , "age") ] , main = "Multiple boxplots for outlier check",
 ylim= c(-3,8),      
 horizontal=TRUE,
 boxwex=0.8, 
 boxfill= c("red", "green" , "blue", "orange")
)


#  Replace the assumed outliers in standardized age, weight, height and perobs with a threshold value.

outlier.fun <- function(vector) {
  
vector [vector > 3] <- 3
vector [vector < -3] <- -3

return (vector) }     

df[,c("weight", "height", "perobs")] <- apply(df[,c("weight", "height", "perobs")], MARGIN=2, FUN=outlier.fun)

summary(df)

# Extract Month from Date 

df$formated_date <- mdy(df$datestop)
df$month <- month(df$formated_date)
df$datestop <-NULL

# Extract Day of Week from Date

df$weekday <- wday(df$formated_date, label=TRUE)


# Standardizing the entries for reasons for stops. 
#It's assumed that police officers leave reason for stop empty or enter 0 when it's a NO.Likewise they enter 1 when it'a YES. 

formatting <- function (a) {
  
  a [a == " " | a== 0 ] <- "N"
  a [a == 1] <- "Y"
  
  return(a)
  
}

df [, c("cs_objcs", "cs_descr" , "cs_casng" , "cs_cloth","cs_drgtr", "cs_furtv", "cs_vcrim", "cs_bulge", "cs_other","cs_lkout" , "radio")] <- apply(df [, c("cs_objcs", "cs_descr" , "cs_casng" , "cs_cloth","cs_drgtr", "cs_furtv", "cs_vcrim", "cs_bulge", "cs_other","cs_lkout","radio")],2, FUN= formatting)



#Standardazing the entries for races.
df$race [df$race == "I" | df$race == " " | df$race == "U"] = "Z"
df$race [df$race == "P"] = "Q"

# Standardizing entries for location of stop
df$trhsloc [df$trhsloc == "H"]  <- "P"
df$trhsloc [df$trhsloc == " "] <- "Other"


