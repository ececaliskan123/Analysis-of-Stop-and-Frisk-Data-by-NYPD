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

### DIRTY SOLUTION FOR 2013 -> all excluded to save calculation time!

# df_coord = df[c("year","long","lat","weaponfound")]
# df2012 = df_coord[df_coord$year==2012,]
# df2012_1 = df2012[1:10000,]
# df2012_2 = df2012[10001:20000,]
# df2012_3 = df2012[20001:30000,]
# df2012_4 = df2012[30001:40000,]
# df2012_5 = df2012[40001:50000,]
# df2012_6 = df2012[50001:60000,]
# df2012_7 = df2012[60001:nrow(df2012),]
# 
# # store all in a list
# dflist = list(df2012_1,df2012_2,df2012_3,df2012_4,df2012_5,df2012_6,df2012_7)
# 
# # apply HitRateCalcution on every element of the list
# output = lapply(dflist, function(x){
#   df_yr_unweighted = geodist(df[df$year==2013,c("long","lat")], x[,c("long","lat")])/1000
#   df_yr_unweighted = exp(-(df_yr_unweighted^2)/2)
#   
#   df_yr_wpfound = as.matrix(x$weaponfound)
#   sums_weighted = df_yr_unweighted %*% df_yr_wpfound
#   
#   sums_unweighted = apply(df_yr_unweighted, 1, sum)
#   
#   return(sums_weighted/sums_unweighted)
#   
# })
# 
# # put list-elements together
# test = as.data.frame(unlist(output[[1]]))
# test$two = as.data.frame(unlist(output[[2]]))
# test$three = as.data.frame(unlist(output[[3]]))
# test$four = as.data.frame(unlist(output[[4]]))
# test$five = as.data.frame(unlist(output[[5]]))
# test$six = as.data.frame(unlist(output[[6]]))
# test$seven = as.data.frame(unlist(output[[7]]))
# test$hitRate2013 = apply(test,1,sum)
# 
# # add to dataset
# df$hitRate[which(df$year==2013)] = test$hitRate2013

# save Output
saveRDS(df,file="df.rds")