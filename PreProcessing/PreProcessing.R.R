# LOAD YEARLY DATASETS

# define years that are of relevance for us (2008-2012 to compare results with
# the paper's results, 2012-2016 to extend the analysis)
years = 2008:2016

# assign all yearly datasets to a list (of dataframes)
dfs = lapply(years, function(x) {
  readRDS(file = paste0("./Data-rds/sqf", x, ".rds"))
})

# ONLY KEEP CPW STOPS (stops in which the suspected crime is 'criminal possession
# of weapon')

# apply names for better readability
dfs_names = paste0("df", years)
names(dfs) = dfs_names

# rename 'detailCM' to 'detailcm'
dfs = lapply(years, function(x) {
  namedf = paste0("df", x)
  names(dfs[[namedf]])[names(dfs[[namedf]]) == "detailCM"] <- "detailcm"
  dfs[[namedf]]
})

# the CPW stops are addressed with code '20' in column 'detailcm'
dfs = lapply(seq_along(years), function(i) {
  dfs[[i]] = dfs[[i]][dfs[[i]]$detailcm == "20", ]
  dfs[[i]]
})

# SUBSET THE DATAFRAME - only keep the relevant columns (as defined in the paper)
# The paper mentions: sex, race, build, trhsloc (housing or transit), inout
# (inside or outside), year, datestop, timestop all vars beginning with 'cs_' =
# reason for stop radio; offunif; ht_feet ; ht_inch ; weight; age; perobs columns
# indicating whether a weapon was found: pistol, riflshot, asltweap, knifcuti,
# machgun, othrweap

# define relevant columns

weaponVars = c("pistol", "riflshot", "asltweap", "knifcuti", "machgun", "othrweap")
rs_stop = grep("cs_", names(dfs[[1]]), value = TRUE)
add_circumstances = grep("ac_", names(dfs[[1]]), value = TRUE)

covariates = c("year", "datestop", "timestop", "pct", "sex", "race", "build", "ht_feet", 
               "ht_inch", "weight", "age", "trhsloc", "inout", "offunif", "radio", rs_stop, 
               add_circumstances, weaponVars, "perobs", "xcoord", "ycoord")

# check if 'covariates' exist in all datasets

# apply names for better readability
dfs_names = paste0("df", years)
names(dfs) = dfs_names

lapply(seq_along(years), function(x) {
  diff_names = sum(covariates %in% names(dfs[[x]]) == FALSE)
  if (diff_names == 0) {
    print(paste("All covariates exist in the yearly dataset:", names(dfs[x])))
  } else {
    print(paste("There are differences in the yearly dataset:", names(dfs[x])))
  }
})

# the only difference is a typo in df2016
names(dfs[["df2016"]])[1] = "year"  # column had a wrong name (= i...year)

# keep only relevant covariates
dfs = lapply(seq_along(dfs), function(x) {
  subset(dfs[[x]], select = covariates)
})

# COMBINE EVERYTHING INTO ONE DATAFRAME
df = do.call(rbind, dfs)  # combine all rows into one df
rm(dfs)  # remove list (which is now unnecessary)

# Create dependent variable: the binary var whether a weapon was found

df$weaponfound = ifelse(df$pistol == "Y" | df$riflshot == "Y" | df$asltweap == "Y" | 
                          df$knifcuti == "Y" | df$machgun == "Y" | df$othrweap == "Y", 1, 0)
df[, c(weaponVars)] = NULL  #delete, since no use for weaponVars anymore

# Convert feet & inches into centimers and create variable 'height'
df$height = df$ht_feet * 30.48 + df$ht_inch * 2.54
df[, c("ht_feet", "ht_inch")] = NULL

df$age = as.numeric(df$age)  # convert age to numeric

# save row-name in a column as a unique identifier (easier to work with)
if (!require("tibble")) install.packages("tibble")
df = tibble::rownames_to_column(df)
df$rowname = as.numeric(df$rowname)

##### TRANSFORM COORDINATES ######

# Define Spatial Data
df = df[!is.na(df$xcoord) & !is.na(df$ycoord),]             # deletes NAs
df = st_as_sf(df,coords=c("xcoord","ycoord"),crs=102718)   #define spatial data, "102718" is the code for the US state planning

# store coordinates in long/lat format
df      = st_transform(df, crs=4326) # switch to long/lat format
df$long = st_coordinates(df)[,1] # add longitude (= x value)
df$lat  = st_coordinates(df)[,2]  # add latitude (= y value)

df      = st_set_geometry(df, NULL) # remove the spatial component

#### CALCULATE LOCAL HIT RATE #######

# load package to calculate (pairwise) geodesic distance
if(!require("geodist")) install.packages("geodist"); library("geodist") # to calculate pairwise (pw) distances
if(!require("pbapply")) install.packages("pbapply"); library("pbapply") # to track progress of lapply

# define hitRate function

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

# apply the hitrate function to all years
years = unique(df$year)[unique(df$year)>min(unique(df$year))] # saves all years except the lowest (since this is only needed for the h(t-1) annotation)
AllHRs = lapply(years, function(x){hitRateCalculation(x,500)}) # calculates hitRates for all yearas
names(AllHRs) = years

# AllHRs is a list with sub-lists -> unlist each sublist
AllHRs = lapply(years, function(x){unlist(AllHRs[[paste(x)]])})
names(AllHRs) = years

# assign list elements to dataset

df$hitRate = NA
for(i in years){
  test =as.data.frame(AllHRs[[paste(i)]])
  df$hitRate[which(df$year==i)] = test[,1]
}

# delete the first year (since it was only necessary for hitRate calculation)
firstYear = min(unique(df$year))
df = df[df$year!=firstYear,]

# transform hitrate to numeric
df$hitRate = as.numeric(df$hitRate)

# SAVE FILE FOR FURTHER PROCESSING (cleaning etc.)
saveRDS(df,file="df.rds")
