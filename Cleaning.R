# **********************************
#       CLEANING
#
# This file addresses missing values and treats outliers.
# **********************************

# Check and correct for missing values

colSums(is.na.data.frame(df))
indeces <- complete.cases(df)
MFV <- as.numeric(names(sort(table(df$age), decreasing=TRUE)[1])) 
df$age [is.na(df$age)] <- MFV # completes the NA's in age with most frequent value

# Define a function for standardization
standardize <- function(x){
  
  mu <- mean(x)
  std <- sd(x)
  result <- (x - mu)/std
  
  return(result)   }

df[,c("weight", "height", "perobs" , "age")] <- apply(df[,c("weight", "height", "perobs", "age")], MARGIN=2, FUN=standardize)


# Outlier check
boxplot(df [, c("weight", "height", "perobs" , "age") ] , main = "Multiple boxplots for outlier check",
       
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)

#  Replace the assumed outliers in standardized age, weight, height and perobs with a threshold value.
outlier.fun <- function(vector) {
  
vector [vector > 3] <- mean(vector) + 3*sd(vector)
 
return (vector) }     

df.new <- apply(data.frame(df$weight, df$height, df$perobs), 2, outlier.fun)

df[,c("weight", "height", "perobs")] <- apply(df[,c("weight", "height", "perobs")], MARGIN=2, FUN=outlier.fun)

summary(df)

# Extract the Month from Date
install.packages("lubridate")
library(lubridate)
df$month <- month(dmy(df$datestop))

# Extract Weekday from Date
install.packages("anytime")
as.character(df$datestop)
date <- anytime:: anydate (df$datestop)
df$weekday <- wday(date, label=TRUE)

# Time of the day variable

time <- anytime:: anydate (df$timestop)



# Standardizing the entries for reasons for stops 

formatting <- function (vector) {
  
    loc <- which (vector == "")  
    vector[loc] <- "N" 
    loc2 <-which (vector == "1")
  vector[loc] <- "Y"

}

df [, c("cs_objcs", "cs_descr" , "cs_casng" , "cs_cloth","cs_drgtr", "cs_furtv", "cs_vcrim", "cs_bulge", "cs_other")] <- apply(df [, c("cs_objcs", "cs_descr" , "cs_casng" , "cs_cloth","cs_drgtr", "cs_furtv", "cs_vcrim", "cs_bulge", "cs_other")],2, FUN= formatting)


# df$cs_objcs [df$cs_objcs == ""] <- "N"
# df$cs_objcs [df$cs_objcs == "1"] <- "Y"
# df$cs_descr [df$cs_descr == ""] <- "N"
# df$cs_descr [df$cs_descr == "1"] <- "Y"
# df$cs_casng [df$cs_casng == ""] <- "N"
# df$cs_casng [df$cs_casng == "1"] <- "Y"
# df$cs_lkout [df$cs_lkout == ""] <- "N"
# df$cs_lkout [df$cs_lkout == "1"] <- "Y"
# df$cs_cloth [df$cs_cloth == ""] <- "N"
# df$cs_cloth [df$cs_cloth == "1"] <- "Y"
# df$cs_drgtr [df$cs_drgtr == ""] <- "N"
# df$cs_drgtr [df$cs_drgtr == "1"] <- "Y"
# df$cs_furtv [df$cs_furtv == ""] <- "N"
# df$cs_furtv [df$cs_furtv == "1"] <- "Y"
# df$cs_vcrim [df$cs_vcrim == ""] <- "N"
# df$cs_vcrim [df$cs_vcrim == "1"] <- "Y"
# df$cs_bulge [df$cs_bulge == ""] <- "N"
# df$cs_bulge [df$cs_bulge == "1"] <- "Y"
# df$cs_other [df$cs_other == ""] <- "N"
# df$cs_other [df$cs_other == "1"] <- "Y"
