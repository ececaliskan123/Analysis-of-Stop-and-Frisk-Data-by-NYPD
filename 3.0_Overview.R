#*************************************
#Preparation
#*************************************

source("LoadPackages.R")

# Source codes. Fast track refers to 1.Create Dataset
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
hitRate = readRDS("hitRate.rds")
df      = cbind(df, hitRate)
colnames(df)[which(names(df)=="df[, \"hitRate\"]")] = "hitRate"                 

age     = df$age

source("1.3_Cleaning.R", local = FALSE)
df$age  = age     #Replace cleaned dataset with original age
rm(age)
#-----------------------
var     = c("year", "formated_date", "pct", "race", "age", "sex", 
            "weaponfound", "hitRate")
yr      = c(2013, 2014, 2015, 2016)
df1     = df %>% 
  dplyr::select(var) %>% 
  filter(year %in% yr) 

#=============================
#2. Further Cleaning
#=============================

str(df1)
df1$pct  = as.character(df1$pct)

# Regroup races
df1$race = as.factor(df1$race)
df1$race [df1$race == "I" | df1$race == " " | df1$race == "U"] = "Z"
df1$race [df1$race == "P"] = "Q"
df1$race = factor(df1$race)

# Filter unknown sex
table(df1$sex)
#F     M     Z       
#1208 31379   428     0 
df1      = df1[(df1$sex == "F" | df1$sex == "M"),]
df1$sex  = factor(df1$sex) 

# Replace NA in age with mode
summary(df1$age)
df1$age[is.na(df1$age)] = order(table(df1$age), decreasing = TRUE)[1]

# Substitue age outliers with values 3 s.d. from mean via a function
fun.outlier = function(v) {
  mu = mean(v)
  sd = sd(v)
  v[v > (mu + 3*sd)] = (mu + 3*sd)
  v[v < (mu - 3*sd)] = (mu - 3*sd)
  
  return(v)
}
age2    = lapply(df1[c("age")], FUN = fun.outlier)
df1$age = as.integer(unlist(age2, use.names = FALSE))

# Access if hitRate values are normal
range(df1$hitRate)    # 0 <= range <= 1, OK

saveRDS(df1, file = "To3.2.rds")
rm(age2)

#*************************************
#Data Visualization
#*************************************

#=============================
#Trends across Time
#=============================

df1$ym    = format(df1$formated_date, format = "%y/%m")
df1$ym    = as.Date(parse_date_time(df1$ym, orders = "%y/%m"))
case.numb = df1 %>%
  group_by(ym) %>%
  summarize("freq" = n(), "hr" = mean(hitRate))
range(case.numb$freq)   

# Plot case numbers against time
trend.case = ggplot(case.numb, aes(ym, freq)) + 
  theme_bw() +
  geom_point() + 
  stat_smooth(color = "dark blue", fill = "dark blue", method = "loess") +  
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%y") +
  coord_cartesian(ylim = c(0, 1000)) + 
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

plot_grid(trend.case, trend.rate, ncol = 1)   # Combine two plots with aligned x-axis in one graph

rm(case.numb, trend.case, trend.rate)

#=============================
#Racial Compositions in Different Contexts
#=============================

df2 = df1 %>% 
  group_by(race) %>% 
  summarize(hit = 100 * mean(hitRate), freq = n()) %>%
  mutate(stops = 100 * freq / sum(freq))
df2 = df2[, -3]

# Include census data (https://statisticalatlas.com/place/New-York/New-York/Race-and-Ethnicity)
df2$census = c(13.7, 24.4, 26.7, 32.3, 1.8+1.1)

# (tidyverse::gather gives error message, it's performance depends on version of R (R3.4 works, not R3.5))
plot = data.table::melt(df2, id.vars = "race")  # df2 is a df, and reshape::melt cannot melt multiple measures

ggplot(plot, aes(y = value, x = race, fill = variable)) +
  geom_bar(position = "dodge", stat="identity") +    
  theme_bw() +                               
  scale_x_discrete(labels = c("Asian", "Black", "Hispanic", "White", "Others")) + 
  labs(x = "Race Group", y = "Composition (%)", 
       title = "Race Composition in Different Contexts", fill = "Context")

