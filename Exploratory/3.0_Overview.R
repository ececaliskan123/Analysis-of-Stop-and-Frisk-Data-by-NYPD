#*************************************
#Preparation
#*************************************

rm(list=ls())

source("LoadPackages.R")
source("1.0_PreProcessing.R", local = FALSE) 
source("1.1_coordinates.R", local = FALSE)

# Use output from 1.2_hitRate.R directly
df          = df[df$year!=min(unique(df$year)),]    
hitRateAll  = readRDS("./Data-rds/hitRate.rds")  
hitRateAll$rowname = as.numeric(hitRateAll$rowname)
df          = merge(df, hitRateAll, by="rowname")
df          = df[order(df$year),]
saveRDS(df, file="df.rds")              

age = df$age          #Store the original age separately

source("1.3_Cleaning.R", local = FALSE)

df$age.raw  = age     #Add cleaned dataset with original age

#*************************************
#Data Processing
#*************************************
#=============================
#1. Create Dataset
#=============================

var = c("rowname", "year", "formated_date", "pct", "age", "age.raw", 
        "race", "sex", "weaponfound", "hitRate", "long", "lat")
yr  = c("2013", "2014","2015","2016")

df1 = df %>%
  dplyr::select(var) %>%               #Subset df with relevant variables only
  dplyr::filter(year %in% yr)          #Keep only records from 2013 to 2016

rm(age)

#=============================
#2. Further Cleaning
#=============================

str(df1)  #Quick glance at what needs to be processed    
df1$pct         = as.character(df1$pct)

# Inspect race 
df1$race = as.factor(df1$race)
table(df1$race)  #OK

df1$weaponfound = as.factor(df1$weaponfound)

# Filter unknown sex
table(df1$sex)
#F     M     Z       
#2486 64904   678     0 
df1      = df1[(df1$sex == "F" | df1$sex == "M"),]
df1$sex  = factor(df1$sex) 

# Replace NA in age with mode
summary(df1$age.raw)
df1$age.raw[is.na(df1$age.raw)] = order(table(df1$age.raw), decreasing = TRUE)[1]

# Substitue age outliers with values 3 s.d. from mean via a function
fun.outlier = function(v) {
  mu = mean(v)
  sd = sd(v)
  v[v > (mu + 3*sd)] = (mu + 3*sd)
  v[v < (mu - 3*sd)] = (mu - 3*sd)
  
  return(v)
}

age2    = lapply(df1[c("age.raw")], FUN = fun.outlier)
df1$age.raw = unlist(age2, use.names = FALSE) #Unlist age2, change data to numbers.

# Access if hitRate values are normal
range(df1$hitRate)    # 0 <= range <= 1, OK

saveRDS(df1, file = "3.0.rds")
rm(age2)

#*************************************
#Data Visualization
#*************************************

#=============================
#Trends across Time
#=============================

df1$ym    = format(df1$formated_date, format = "%y/%m")         #New column to store dates in form of "YY-MM". Data is character.
df1$ym    = as.Date(parse_date_time(df1$ym, orders = "%y/%m"))
case.numb = df1 %>%
  group_by(ym) %>%
  summarize("freq" = n(), "hr" = mean(hitRate))           
case.numb

range(case.numb$freq)
  #[1]  100 9196
format(range(case.numb$hr), scientific = FALSE)
  #[1] "0.03467758" "0.13191693"

# Plot case numbers against time
trend.case = ggplot(case.numb, aes(ym, freq)) + 
  theme_bw() +
  geom_point() + 
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%y") +
  coord_cartesian(ylim = c(0, 3000)) +      # Limit y-coordinates to exclude months with high counts but low weighted value
  stat_smooth(color = "dark blue", fill = "dark blue", method = "loess") + 
  labs(x = "Month", y = "Count", title = "Monthly Case Number over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot hit rates against time
trend.rate = ggplot(case.numb, aes(ym, hr)) + 
  theme_bw() +
  geom_point() + 
  stat_smooth(color = "dark red", fill = "dark red", method = "loess") +  
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%y") +
  coord_cartesian(ylim = c(0, 0.2)) + 
  labs(x = "Month", y = "Hit Rate", title = "Monthly Hit Rate over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(trend.case, trend.rate, ncol = 1, align = "v")   # Combine two plots with aligned x-axis in one graph

rm(case.numb, trend.case, trend.rate)

#=============================
#Racial Compositions in Different Contexts
#=============================

df2 = df1 %>% 
  group_by(race) %>% 
  dplyr::summarize(freq = n()) %>%
  mutate(stops = 100 * freq / sum(freq))
df2 = df2[, -2]

# Include census data (https://statisticalatlas.com/place/New-York/New-York/Race-and-Ethnicity)
df2$census = c(14.0, 24.3, 29.1, 32.1, 0.1+0.4)

# (tidyverse::gather gives error message, it's performance depends on version of R (R3.4 works, not R3.5))
plot = data.table::melt(df2, id.vars = "race")  # df2 is a df, and reshape::melt cannot melt multiple measures

ggplot(plot, aes(y = value, x = race, fill = variable)) +
  geom_bar(position = "dodge", stat="identity") +    
  theme_bw() +                               
  scale_x_discrete(labels = c("Asian", "Black", "Hispanic", "White", "Others")) +  #Re-label legend text
  labs(x = "Race Group", y = "Composition (%)", 
       title = "Race Composition in Different Contexts", fill = "Context")

rm(df2, plot)
