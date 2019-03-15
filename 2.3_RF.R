# ******************************
#   This code snippet duplicates the analyses of section 3.1-3.4 with the 
#   random forest classifier. Goel et al (2016) use the same method as a robustness
#   check to validate their results from logitic regression.
# ******************************

# settings for the server
# .libPaths("H:/RPackages")
# setwd("H:/nypd-stopfrisk-master")

if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("ModelMetrics")) install.packages("ModelMetrics"); library("ModelMetrics") #for fct AUC

df = readRDS("df.rds")

### CLEANING

# delete columns that ar enot necessary for the modeling
df[,c("long","lat","CPW","timestop")] = NULL

# conversions
df[sapply(df, is.character)] = lapply(df[sapply(df, is.character)],as.factor) #convert all characters into factors

df$weaponfound = as.factor(df$weaponfound) # to make it a classification-problem! (no regression)
df$pct = as.factor(df$pct) # pct is also not an integer
df$weekday = as.factor(df$weekday) # weekday also isn't
df$month = as.factor(df$month) # month also isn't

#last check for missing values
if(sum(sapply(df,function(x){sum(is.na(x))}) > 0) == 0){
  print("No missing values - Ready to go!")
}  else{
  print("Check missing values!")
}

# column sex: remove unknown
df = df[df$sex!="Z",]
# column build: remove unknown (=Z), only 6k rows
df = df[df$build!="Z",]
# column race: remove unknown (=Z), only 6k rows

# remove all unused factor levels - line from stackoverflow -- change it!
df[] = lapply(df, function(x) if(is.factor(x)) droplevels(x) else x)

# reduce df for testing purpose (only a fraction of each year)
set.seed(123)
percentage     = 1

reducedTrainYrs  = lapply(c(2013,2014), function(x){
  df_yr = df[df$year==x,]
  smp_size = floor(percentage * nrow(df_yr))
  train_ind = sample(seq_len(nrow(df_yr)), size = smp_size) # creates various indeces
  train_yr = df_yr[train_ind,]
}) 

reducedTestYrs  = lapply(c(2015,2016), function(x){
  df_yr = df[df$year==x,]
  smp_size = floor(percentage * nrow(df_yr))
  train_ind = sample(seq_len(nrow(df_yr)), size = smp_size) # creates various indeces
  train_yr = df_yr[train_ind,]
}) 

train = do.call(rbind, reducedTrainYrs)
rm(reducedTrainYrs)
test = do.call(rbind, reducedTestYrs)
rm(reducedTestYrs)

# extract wpfound before
wpfound = as.data.frame(train$weaponfound)
rowname = as.data.frame(train$rowname)
train$rowname = NULL # so no interaction terms...

# create interaction terms  
train = as.data.frame(model.matrix(weaponfound ~ .^2, data=train))

names(train) = make.names(names(train),unique = TRUE) # create valid names

# add wpfound again
train = cbind(train,wpfound)
train = cbind(train, rowname)
names(train)[names(train) == "train$weaponfound"] = "weaponfound" 
names(train)[names(train) == "train$rowname"] = "rowname" 
train$weaponfound = as.factor(train$weaponfound)

# make test comparable with train
wpfound = as.data.frame(test$weaponfound)
rowname = as.data.frame(test$rowname)
test$rowname = NULL
test = as.data.frame(model.matrix(weaponfound ~ .^2, data=test))
names(test) = make.names(names(test),unique = TRUE) # create valid names
test = cbind(test,wpfound)
test = cbind(test,rowname)
names(test)[names(test) == "test$weaponfound"] = "weaponfound" 
names(test)[names(test) == "test$rowname"] = "rowname" 
test$weaponfound = as.factor(test$weaponfound)

### try h2o modeling (a machine-learning package making use of multiple CPU cores)
if(!require("h2o")) install.packages("h2o"); library("h2o") 
localH2O <- h2o.init(nthreads = -1)
h2o.init() # check status

#convert to h2o
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)

# create interactions - already done
# factorVars = names(train[sapply(train,is.factor)])
# train.h2o.intact = h2o.interaction(train.h2o,pairwise = TRUE, factor= factorVars, max_factors = 77, min_occurrence = 1)

system.time(
  rforest.model <- h2o.randomForest(y="weaponfound", training_frame = train.h2o, ntrees = 1000,  min_rows = 10, seed = 1122)
)

h2o.performance(rforest.model)

# predict on test-data
h2o.predict(rforest.model, test.h2o)

yhat.h2o = as.data.frame(h2o.predict(rforest.model, test.h2o))[,3]

auc(test$weaponfound,yhat.h2o)


