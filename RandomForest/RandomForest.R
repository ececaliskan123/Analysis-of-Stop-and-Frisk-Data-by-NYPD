# ******************************************************* This code
# snippet applies the random forest classifier to predict the hit rate.
# It has been used by Goel et al (2016) as a robustness check and
# yielded largely similar results.  Outline of this snippet: - Data
# Preparation (Conversions and interaction terms) - Setup of the H2o
# environment - Random Forest Model - Post Validation
# *******************************************************

### set parameters
perc = 1
model.choice = "extend"

# settings for the server -- exclude at the very end!
# .libPaths('H:/RPackages') setwd('H:/nypd-stopfrisk-master')


######## DATA PREPARATION ########

# This subsection applies necessary conversions, creates interaction
# terms and splits the data into test and train.

# load the dataset
df = readRDS("df.rds")

# delete irrelevant columns, which are not part of the modeling process
df[, c("long", "lat", "formated_date")] = NULL
# long / lat, because spatial information is captured by local hitRate
# datestop & timestop, because timely information is captured by
# weekday & month

# treat inconsistencies in all columns 'additional circumstances'
# (starting with 'ac_')

add_circ = grep("ac_", names(df), value = TRUE)  # extract all respective columns 

reduceFactors = function(x) {
  # function adapted from the cleaning-quantlet
  x[x == " " | x == 0] = "N"
  x[x == 1] = "Y"
  
  return(x)
}

df[, c(add_circ)] = apply(df[, c(add_circ)], 2, FUN = reduceFactors)

# convert all characters into factors
df[sapply(df, is.character)] = lapply(df[sapply(df, is.character)], as.factor)

# convert other relevant columns into factors
relevant_columns = c("weaponfound", "pct", "weekday", "month", "year")
df[, relevant_columns] = lapply(df[, relevant_columns], as.factor)
# weaponfound needs to be a factor, since it is a classification
# problem pct, weekday and months are factors as well



# do a last check if missing values are present
if (sum(sapply(df, function(x) {
  sum(is.na(x))
}) > 0) == 0) {
  print("No missing values - Ready to go!")
} else {
  print("Check missing values!")
}

# reduce dimensionality - remove 'unknowns'
df = df[df$sex != "Z" & df$build != "Z", ]
# both columns only had a few 'Z' (i.e., 'unknown') dropping irrelevant
# factor levels reduces the dimensionality after creating the
# interaction-terms

# remove all unused factor levels

df[] = lapply(df, function(x) { 
  if (is.factor(x)) {       # in this setting, lapply iterates through every column
    droplevels(x)           # droplevels removes empty factors
  } else {
    x                       # return x, if column != factor
  }
})

# split the dataset into test & train the following lines are a
# work-around if one wants to apply the random forest on a fraction of
# the observations (for testing purpose only)

set.seed(123)  # guarantees comparability in the row-sampling
percentage = perc  # set one for the whole train/test dataframes 

# replicate or extend analysis
choice = model.choice  # 'replicate' or 'extend' defined above

if (choice == "replicate") {
  # replicate considers the same years as Goel et al (2016)
  years_train = c(2009, 2010)
  years_test = c(2011, 2012)
} else if (choice == "extend") {
  # extent (=default), uses more recent data
  years_train = c(2013, 2014)
  years_test = c(2015, 2016)
} else {
  stop("Indicate clearly whether the model should replicate or extend the analysis!")
}

train = df[df$year == c(years_train), ]
train$year = droplevels(train$year)
test = df[df$year == c(years_test), ]
test$year = droplevels(test$year)

# reduce number of observations (if percentage != 1)

reduceData = function(train_or_test) {
  smp_size = floor(percentage * nrow(train_or_test))
  row_indices = sample(seq_len(nrow(train_or_test)), size = smp_size)  # creates random indeces
  train_or_test = train_or_test[row_indices, ]
  return(train_or_test)
}

train = reduceData(train_or_test = train)
test = reduceData(train_or_test = test)

# extract wpfound before
train.wpfound = as.data.frame(train[, c("rowname", "weaponfound")])
test.wpfound = as.data.frame(test[, c("rowname", "weaponfound")])

# create interaction terms

vars_intact = paste(names(train[, !names(train) %in% c("rowname", "weaponfound")]), 
                    collapse = " + ")

formula = as.formula(paste("weaponfound", "~ (", vars_intact, ")^2 + ", 
                           "rowname"))

# create test $ train data with interaction-terms
train = as.data.frame(model.matrix(formula, data = train))
test = as.data.frame(model.matrix(formula, data = test))

train = merge(train, train.wpfound, by = "rowname")
test = merge(test, test.wpfound, by = "rowname")

names(train) = make.names(names(train), unique = TRUE)  # create valid names
names(test) = make.names(names(train), unique = TRUE)  # create valid names

### try h2o modeling (a machine-learning package making use of multiple
### CPU cores)
if (!require("h2o")) install.packages("h2o")
library("h2o")

localH2O <- h2o.init(nthreads = -1, max_mem_size = NULL)

h2o.init()  # check status

# convert to h2o
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)

# create interactions - already done factorVars =
# names(train[sapply(train,is.factor)]) train.h2o.intact =
# h2o.interaction(train.h2o,pairwise = TRUE, factor= factorVars,
# max_factors = 77, min_occurrence = 1)

system.time(rforest.model <- h2o.randomForest(y = "weaponfound", training_frame = train.h2o, 
                                              ntrees = 1000, min_rows = 10, seed = 1122))

# h2o.performance(rforest.model)

# predict on test-data h2o.predict(rforest.model, test.h2o)

test.h2o$yhat = h2o.predict(rforest.model, test.h2o)[, 3]
yhat.rf = as.data.frame(test.h2o[, c("rowname", "yhat")])
saveRDS(yhat.rf, file = "yhat.RF.rds")

# auc for evaluation
if (!require("ModelMetrics")) install.packages("ModelMetrics")
auc(test$weaponfound, yhat.rf$yhat)
