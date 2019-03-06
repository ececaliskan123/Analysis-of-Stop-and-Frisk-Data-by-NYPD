# **********************
# This snippet calculates the hit-rate as described on p. 372 in the paper
# ***********************


# load dataset (for testing purpose only one year so far)
df <- readRDS("df.rds")

# load package to calculate (pairwise) geodesic distance
if(!require("geodist")) install.packages("geodist"); library("geodist") # check


hitRateCalculation = function(yr){
  
  # create pairwise geodesic distances in kilometres
  df_yr_unweighted = geodist(df[df$year==yr,c("long","lat")], df[df$year==yr-1,c("long","lat")])/1000
  
  # apply formula according to p.371
  df_yr_unweighted = exp(-(df_yr_unweighted^2)/2)
  
  # weight the distances according to binary variable, i.e. creates the numerator
  df_yr_wpfound    = as.matrix(df$weaponfound[df$year==yr-1])
  sums_weighted    = df_yr_unweighted %*% df_yr_wpfound
  
  # sum up rows, i.e. creates the denumerator
  sums_unweighted  = apply(df_yr_unweighted, 1, sum)
  
  # hitRate = numerator/denumerator
  hitRate_yr       = sums_weighted/sums_unweighted
  
  return(hitRate_yr)
}

# apply the function to each year

years           = list(2014,2015,2016) #so far only these years, since 2013 & 2012 have too many rows
list_hr         = lapply(years, function(x){X = hitRateCalculation(x)})
names(list_hr)  = years


df$hitRate      = NA # create dummy column

# assign hitRate to the dataset

for (yr in years){
  
  # assign hitRate to respective years
  df$hitRate[which(df$year==yr)] = list_hr[[paste(yr,collapse = "")]]
  
}

# save Output
saveRDS(df,file="df.rds")