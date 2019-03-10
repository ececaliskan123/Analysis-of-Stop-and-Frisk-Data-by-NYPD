# ******************************
#   This code snippet duplicates the analyses of section 3.1-3.4 with the 
#   random forest classifier. Goel et al (2016) use the same method as a robustness
#   check to validate their results from logitic regression.
# ******************************


if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("ModelMetrics")) install.packages("ModelMetrics"); library("ModelMetrics") #for fct AUC

df = readRDS("df.rds")
# reduce df for testing purpose (only a fraction of each year)
set.seed(123)
percentage     = 0.1

reducedTrainYrs  = lapply(c(2009,2010), function(x){
  df_yr = df[df$year==x,]
  smp_size = floor(percentage * nrow(df_yr))
  train_ind = sample(seq_len(nrow(df_yr)), size = smp_size) # creates various indeces
  train_yr = df_yr[train_ind,]
  }) 

reducedTestYrs  = lapply(c(2011,2012), function(x){
  df_yr = df[df$year==x,]
  smp_size = floor(percentage * nrow(df_yr))
  train_ind = sample(seq_len(nrow(df_yr)), size = smp_size) # creates various indeces
  train_yr = df_yr[train_ind,]
}) 

train = do.call(rbind, reducedTrainYrs)
  rm(reducedTrainYrs)
test = do.call(rbind, reducedTestYrs)
  rm(reducedTestYrs)

# split to train & test
# train  = subset(df, year==2009 | year==2010) # excluded for now! (too big)
# test  = subset(df, year==2011 | year==2012)

# train the model....
test[,c("long","lat","CPW","timestop","crimsusp")] = NULL
train[,c("long","lat","CPW","timestop","crimsusp")] = NULL
train[sapply(train, is.character)] = lapply(train[sapply(train, is.character)],as.factor) #convert all characters into factors


rf <- randomForest(weaponfound~ . , # formula
                   data = train, 
                   mtry = 10, 
                   ntree = 800)

yhat = predict(rf)
auc(test$weaponfound,yhat) #0.66


