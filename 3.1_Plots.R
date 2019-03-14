#*************************************
#Preparation
#*************************************

rm(list=ls())

source("LoadPackages.R")

source("1.0_PreProcessing.R", local = FALSE) 
source("1.1_coordinates.R", local = FALSE)
source("1.2_hitRate.R", local = FALSE)
source("1.3_Cleaning.R", local = FALSE)

#*************************************
#Data Processing
#*************************************
source("LoadPackages.R")
source("1.0_PreProcessing.R", local = FALSE) 
source("1.1_coordinates.R", local = FALSE)
# Paste 1.2_hitRate.R output directly to skip time-consuming calculation
df          = df[df$year!=min(unique(df$year)),]  
hitRateAll  = readRDS("./Data-rds/hitRate.rds")     
hitRateAll$rowname = as.numeric(hitRateAll$rowname)
df          = merge(df, hitRateAll, by="rowname")
df       = df[order(df$year),]
saveRDS(df, file="df.rds")
source("1.3_Cleaning.R", local = FALSE)

#=============================
#1. Read in Homicide Reports
#=============================

# Read homicide reports
hmc16 = readLines("cy2016.csv")                                   # Read in files as text
hmc16 = read.csv(textConnection(hmc16[-c(1:4)]), header=TRUE)     # Dropping the first 4 rows, covert to dataset

hmc17 = readLines("cy2017.csv")  
hmc17 = read.csv(textConnection(hmc17[-c(1:4)] ), header=TRUE)

# Create integrated homicide report
hmc17 = hmc17[-(293:1917), -(26:99)]     # Remove empty columns and rows
sapply(list(hmc16,hmc17), sapply, class) # Compare column names and class of the dataset
hmc   = rbind(hmc16, hmc17)              # Create complete dataset

rm(hmc16, hmc17)

#=============================
#2. Process Datasets
#=============================

# Overview
yr     = c(2013, 2014, 2015, 2016)
report = df %>% 
  dplyr::select("year", "pct", "sex", "race", "long", "lat", "hitRate") %>% 
  dplyr::filter(year %in% yr)
str(report)

report$pct = as.character(report$pct)       #Variable "PRECINCT" should be a string instead of integer

unique(report$year)                         #Entries are normal
unique(report$pct)                          #Entries are normal
unique(report$race)                         #5 levels of races 
any(is.na(report$long))                     #FALSE. No NAs
any(is.na(report$lat))                      #FALSE.

str(hmc)                                    
hmc$PRECINCT = as.character(hmc$PRECINCT)   #Change variable "PRECINCT" to character

#*************************************
#Data Visualization
#*************************************

#=============================
#1. CPW and Homicide
#=============================

#Preparation
pct.stop            = count(report, pct)                                  # Count stops by precinct
names(pct.stop)[2]  = "freq_cpw"
pct.stop$stops      = pct.stop$freq_cpw / sum(pct.stop$freq_cpw) * 100    # Calculate weights of stops per precinct

pct.hmc             = as.data.frame(table(hmc$PRECINCT))              
pct.hmc             = rename(pct.hmc , c("Var1"="pct", "Freq"="freq_hmc"))
pct.hmc$homicide    = pct.hmc $freq_hmc / sum(pct.hmc $freq_hmc) * 100

joint               = merge.data.frame(pct.stop, pct.hmc, 
                                       by = intersect(names(pct.stop), names(pct.hmc)), 
                                       by.x = "pct", by.y = "pct", 
                                       all.x = TRUE, sort = TRUE)
joint[is.na(joint)] = 0       #Replace all NAs with 0                                         
str(joint)                    #All normal                                       

#Grouped Bar Plot
dt1   = joint %>%                     #Select top 20 precincts per number of stops 
    top_n(n = 20, wt = stops) %>% 
    arrange(desc(stops))
dt1   = dt1[,-c(2, 4)]

plot  = melt(dt1, id.vars = 'pct')    #Combine data by category pct_cpw, pct_hmc 

order = plot %>%                      #Homicide counts arranged in descending order
    filter(variable == "homicide") %>% 
    arrange(desc(value)) %>% 
    .$pct %>% as.character
    
ggplot(plot, aes(x = pct, y = value, fill = variable)) + 
    theme_bw() + 
    geom_bar(position = "dodge", stat = "identity") + 
    scale_x_discrete(limits = order) + 
    labs(x = "Precinct", y = "Percentage", fill = "Type", 
         title = "Distribution of CPW versus Homicide") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank())

rm(hmc, pct.hmc, pct.stop, joint, dt1, plot)

#=============================
#2. CPW and Race
#=============================

# First, user needs to register at Google Cloud Platform for a free API key
# Set up API key to access Google Map for download
register_google(key = "AIzaSyDke5EmHEXGoXkNvL76Ks4TL1tLtSKYqkQ")   

#------------
# For the period 2014-2016
#------------
set.seed(23456)

dt2 = report %>% 
  sample_n(size = 10000)

NYC = get_map(location = c(lon = median(dt2$long), lat = median(dt2$lat)), 
              source = "google", maptype = "terrain", zoom = 11)

ggmap(NYC) + 
    geom_point(data = dt2, mapping = aes(x = long, y = lat, color = race), size = 0.5) + 
    ggtitle("Distribution of CPW Stops Between 2014 and 2016") + 
    scale_color_manual(name  = "Race", labels = c("Asian", 
                                                  "Black", 
                                                  "Hispanic", 
                                                  "White", 
                                                  "Others"), 
                                       values = c("A" = "#CC66FF", 
                                                  "B" = "#FF6666",
                                                  "Q" = "#00CC33",
                                                  "W" = "#0099FF",
                                                  "Z" = "#CCCC00"))
                       
#------------
# Compare with the period 2011-2012
#------------
# To randomly select 10,000 samples for plot (as stated in paper)
set.seed(12345)

df.map = df %>%
  dplyr::select("year", "race", "long", "lat") %>%
  filter(year == "2011" | year == "2012") %>%
  sample_n(size = 10000)

df.map$race = factor(df.map$race)

ggmap(NYC) + 
  geom_point(data = df.map, mapping = aes(x = long, y = lat, color = race), size = 0.5) + 
  ggtitle("Distribution of CPW Stops Between 2011 and 2012") + 
  scale_color_manual(name  = "Race", labels = c("Asian", 
                                                "Black", 
                                                "Hispanic", 
                                                "White", 
                                                "Others"), 
                                     values = c("A" = "#CC66FF", 
                                                "B" = "#FF6666",
                                                "Q" = "#00CC33",
                                                "W" = "#0099FF",
                                                "Z" = "#CCCC00"))
 
rm(df.map, dt2)

#=============================
#3. Empirical hitRate per Race
#=============================

# Filter out Asians and Others due to small Stop-percentage (as in Overview)
report = report %>% 
  group_by(race) %>% 
  filter(race == "B" | race == "W"| race == "Q")

ggplot(report, aes(x=hitRate)) + 
  stat_ecdf(aes(color = race)) +
  theme_bw() + 
  
  # Coordinates in sqaure-root scale to zoom early stage of weapon recovery probability
  coord_trans(x = "sqrt", y = "sqrt") +      
  scale_color_discrete(name = "Race", labels = c("Black",
                                                "Hispanic", 
                                                "White")) + 
  labs(x = "Probability of Weapon Recovery", y = "CDF", title = "CDF of Empirical Hit Rate")

# Comparing overall and race-specific hit rates

# Mean hit rates do not tell much difference
avg.rate = rbind(aggregate(hitRate ~ race, data = report, mean), mean(report$hitRate))
avg.rate[,1] = c("Black", "Hispanic", "White", "Overall")
avg.rate
    #      race    hitRate
    #1    Black 0.04476465
    #2 Hispanic 0.05064923
    #3    White 0.08589065
    #4  Overall 0.04849073

# Use quantiles to find differences
race.p = report %>%
  dplyr::select(race, hitRate) %>%
  arrange(race, desc(hitRate))        # Arrange data first per race, then hitRate in descending order

quantile(race.p$hitRate[race.p$race == "B"], c(0.5, 0.75, 0.9)) 
    # Probability of weapon recovery for stopped black individual at 50th-, 75th- and 90th- quantiles
    #50%        75%        90% 
    #0.03212716 0.05102079 0.07579140 
quantile(race.p$hitRate[race.p$race == "Q"], c(0.5, 0.75, 0.9)) 
    #50%        75%        90% 
    #0.03162746 0.05747656 0.09638025  
quantile(race.p$hitRate[race.p$race == "W"], c(0.5, 0.75, 0.9)) 
    #50%        75%        90% 
    #0.05425556 0.09359589 0.22235586 

rm(avg.rate, race.p)
