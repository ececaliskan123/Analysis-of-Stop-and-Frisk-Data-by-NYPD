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

df$age = as.numeric(df$age)  # YOU CAN COPY THIS LINE INTO CLEANING!

# save row-name in a column as a unique identifier (easier to work with)
if (!require("tibble")) install.packages("tibble")
library("tibble")
df = tibble::rownames_to_column(df)
df$rowname = as.numeric(df$rowname)


# SAVE FILE FOR FURTHER PROCESSING (cleaning etc.)
saveRDS(df, file = "df.rds")