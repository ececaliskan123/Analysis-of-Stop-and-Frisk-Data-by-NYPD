# **********************************
#       CLEANING
#
# This file addresses missing values and treats outliers.
# **********************************



#Remove the variables which are out of scope.
df[, c("xcoord", "ycoord", "perobs", "timestop", "crimsusp", "CPW")] <-NULL

# Check and correct for missing values with Most Frequent Valaue(Mode)

colSums(is.na.data.frame(df)) 

# Missing values in Age are spotted.

#MFV <- as.numeric(names(sort(table(df$age), decreasing=TRUE)[1])) 
df$age [is.na(df$age)] <- median(df$age, na.rm= TRUE)


# This is just in case if NA's were introduced during coercion in "1.0_Preprocessing.R"
df <- df[!apply(is.na(df), 1, any),]


# Correlation between variables
i_num <- sapply(df, is.numeric)
cor(df[,i_num])
corrplot: corrplot(cor(df[,i_num]))


# There are empty entries in Inside or Outside. Set them to NA and then impute.
df$inout <- as.character(df$inout)
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

boxplot(df [, c("weight", "height", "age") ] , main = "Multiple boxplots for outlier check",
 ylim= c(-3,8),      
 horizontal=TRUE,
 boxwex=0.8, 
 boxfill= c("red", "green" , "blue") 

)


#  Replace the assumed outliers in standardized age, weight, height and perobs with a threshold value.

outlier.fun <- function(vector) {
  
vector [vector > 3] <- 3
vector [vector < -3] <- -3

return (vector) }     

df[,c("weight", "height", "age")] <- apply(df[,c("weight", "height",  "age")], MARGIN=2, FUN=outlier.fun)

summary(df)


# Extract Month from Date 

df$formated_date <- lubridate::mdy(df$datestop)
df$month <- month(df$formated_date)
df$datestop <-NULL

# Extract Day of Week from Date

df$weekday <- wday(df$formated_date)

# Standardizing  entries for reasons for stops. 
# It's assumed that police officers leave reason for stop empty or enter 0 when it's a NO. Likewise they enter 1 when it'a YES. 

formatting_cs <- function (a) {
  
  a [a == " " | a== 0 ] <- "N"
  a [a == 1] <- "Y"
  
  return(a)
  
}

df [, c("cs_objcs", "cs_descr" , "cs_casng" , "cs_cloth","cs_drgtr", "cs_furtv", "cs_vcrim", "cs_bulge", "cs_other","cs_lkout" , "radio")] <- apply(df [, c("cs_objcs", "cs_descr" , "cs_casng" , "cs_cloth","cs_drgtr", "cs_furtv", "cs_vcrim", "cs_bulge", "cs_other","cs_lkout","radio")],2, FUN= formatting_cs)



#Standardazing the entries for races. I stands for Indiana and set to  Z (others). U is unknown and also set to Z (others).
#Both Q and P mean Hispanic.
df$race <- as.character(df$race)
df$race [df$race == "I" | df$race == " " | df$race == "U"] = "Z"
df$race [df$race == "P"] = "Q"

# Standardizing entries for location of stop
# It's assumed that both H and P stand for public housing. The cells were left emty when the location neither housing nor transit. Z is assigned for other locations.
df$trhsloc <- as.character(df$trhsloc)
df$trhsloc [df$trhsloc == "H"]  <- "P"
df$trhsloc [df$trhsloc == " "]  <- "Z"





  saveRDS(df, file= "df.rds")
        