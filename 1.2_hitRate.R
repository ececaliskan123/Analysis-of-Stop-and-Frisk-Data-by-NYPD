# **********************
# This snippet calculates the hit-rate as described on p. 372 in the paper
# ***********************


# load dataset (for testing purpose only one year so far)
df <- readRDS("df.rds")

# load package to calculate (pairwise) geodesic distance
if(!require("geodist")) install.packages("geodist"); library("geodist") # to calculate pairwise (pw) distances
if(!require("pbapply")) install.packages("pbapply"); library("pbapply") # to track progress of lapply


hitRateCalculation = function(yr,blocksize){
  
  # create df(t) & df(t-1)
  df_t  = df[df$year==yr,]
  df_t1 = df[df$year==yr-1,]
  
  # split up df(t)
  block   = blocksize
  totRow  = nrow(df_t)
  splits  = rep(1:ceiling(totRow/block),each=block)[1:totRow]
  df_splitted = split(df_t,splits) #contains df_t splitted by block of rows
  
  
  # use inside lapply:
  df_t1_wpfound     = as.matrix(df_t1$weaponfound)  # weight the distances according to binary variable, i.e. creates the numerator
  
  # calculate the hit-Rate
  
  hitRate = pblapply(df_splitted, function(X){
    gc() # garbage collection -> empty memory
    step1 = geodist(X[,c("long","lat")], df_t1[,c("long","lat")])/1000
    step1 = exp(-(step1^2)/2)
    sums_unweighted = rowSums(step1)  #rowsum for block of rows
    sums_weighted = step1 %*% df_t1_wpfound
    hitRate = sums_weighted/sums_unweighted #for each chunk of rows
  })
  
  return(unlist(hitRate))
}

years = unique(df$year)[unique(df$year)>min(unique(df$year))] # saves all years except the lowest (since this is only needed for the h(t-1) annotation)
AllHRs = lapply(years, function(x){hitRateCalculation(x,500)}) # calculates hitRates for all yearas
names(AllHRs) = years

# AllHRs is a list with sub-lists -> unlist each sublist
AllHRs = lapply(years, function(x){unlist(AllHRs[[paste(x)]])})
names(AllHRs) = years

# assign lsit elements to dataset

df$hitRate2 = NA
for(i in years){
  test =as.data.frame(AllHRs[[paste(i)]])
  df$hitRate2[which(df$year==i)] = test[,1]
  
}


