# ******************************
#   This code snippet duplicates the analyses of section 3.1-3.4 with the 
#   random forest classifier. Goel et al (2016) use the same method as a robustness
#   check to validate their results from logitic regression.
# ******************************


if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("ModelMetrics")) install.packages("ModelMetrics"); library("ModelMetrics") #for fct AUC

df = readRDS("df.rds")

### CLEANING

# delete columns that ar enot necessary for the modeling
df[,c("long","lat","CPW","timestop","rowname")] = NULL

# conversions
df[sapply(df, is.character)] = lapply(df[sapply(df, is.character)],as.factor) #convert all characters into factors

df$weaponfound = as.factor(train$weaponfound) # to make it a classification-problem! (no regression)
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
  # overview = as.data.frame(table(df$sex))
df = df[df$sex!="Z",]

# column build: remove unknown (=Z), only 6k rows
df = df[df$build!="Z",]



# reduce df for testing purpose (only a fraction of each year)
set.seed(123)
percentage     = 0.05

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

  # create interaction-terms!
  
bla = as.data.frame(model.matrix(weaponfound ~ .^2, data=train))

rf <- randomForest(weaponfound~ . , # formula
                   data = train, 
                   #mtry = 10, 
                   ntree = 1000,
                   nodesize = 10,
                   do.trace = 10) #sets a progress bar)

yhat = predict(rf, type="class")
auc(test$weaponfound,yhat) #0.66


# parallel computing!
if(!require("parallel")) install.packages("parallel"); library("parallel")
detectCores()
