# **********************************
#       FEATURE SELECTION AND LOGISTIC REGRESSION
#
# This file addresses filter methods and fits logistic regression.
# **********************************

df <- readRDS("df.rds")

##### Feature Selection with Filter Methods

# Include Month, Precinct and weekday as factors instead of integers

df[, c("pct", "month", "weekday")] <- apply(df[, c("pct", "month", "weekday")], 2, FUN = as.character)


#Fisher Score 

# Define a function to calculate the Fisher score 
fisherScore <- function(feature, targetVariable){

  classMeans <- tapply(feature, targetVariable, mean)
  classStds <- tapply(feature, targetVariable, sd)
  classDiff <- abs(diff(classMeans))
  score <- as.numeric(classDiff / sqrt(sum(classStds^2)))
  return(score)}

# Calculate the Fisher score for each numeric variable in the dataset

fisher_scores <- apply(df[,sapply(df, is.numeric)],   2, fisherScore, df$weaponfound)
fisher_scores

# Convert characters into factor variables before regression.

chrIdx <- which(sapply(df, is.character))
df[, chrIdx] <- lapply(df[, chrIdx], factor)

# Information Value (based on WoE) 
#Check the relation between target and categorical variables in the dataset.

i_factor <- sapply(df, is.factor)
X <- df[,i_factor]

y<- as.factor(df$weaponfound)
woe.object <- woe(X, y,zeroadj = 0.5)
# It is safe to ignore empty cell messages as the above parameter zeroadj is set.

# As a rule of thumb: <0.02: not predictive, 0.02-0.1: weak, 0.1-0.3: medium, >0.3: strong

##Preparing the data before  regression 

# Split the data into training and test sets.
df$year <- year(df$formated_date) 
train <- subset(df, year== 2013 | year== 2014)
test <- subset(df, year== 2015 | year== 2016)
train[, c("year", "formated_date")] <- NULL
test[, c("year", "formated_date")] <- NULL


#Multicollinierarty Test

i_num <- sapply(df, is.numeric)
mctest::omcdiag(as.matrix(df[, i_num]), df$weaponfound)


#########Logistic Regression 

##### First option: Stochastic Gradient Descent to solve for dimensioanlity of 7K covariates.

# sgd Package for Logit regression wih Stochastic Gradient Descent
#logitSGD <- sgd(weaponfound ~ . + .*., data = train, model= "glm", model.control= binomial(link="logit")) 
# Problem: sgd package is removed from CRAN repository. Older versions cannot be manually installed from Archive.  ERROR: NON-ZERO EXIT STATUS

##### Second Option: Reducing dimensionality with Lasso Regularization 
#x <- model.matrix(weaponfound~.*. -1, train)
#y <- train$weaponfound

#lasso <- glmnet(x = x, y = y, family = "binomial", standardize = TRUE, alpha = 1, nlambda = 100) 
# Memory problem. Error: Cannot allocate vector of size

##### Third Option: Speeding up GLM with speedglm()

#glm <- speedglm(weaponfound ~.*., data= train, family = binomial(link="logit")) 
# Memory problem.  Cannot alocate vector of size


##### Fourth Option: Speeding up GLM with parallel glms with parglm() using method= FAST 

#parglm(weaponfound ~ ., binomial(), train, control = parglm.control(method = "FAST",nthreads = 2)) 
# Problem: unstable results

##### Fifth Option: Speeding up GLM with parallel glms with parglm() using method= LINPACK

#glm.model <- parglm(weaponfound ~ .*.-height, binomial(), train, control = parglm.control(method = "LINPACK", nthreads = 2))
# Doesn't work, session gets aborted after long hours. Therefore, interaction terms were dropped.

glm.model <- parglm(weaponfound ~. -height, binomial(), train, control = parglm.control(method = "LINPACK", nthreads = 4))

#Or just the regular glm()

glm.modell <- glm(formula = weaponfound ~ . - height-perobs, family = binomial(link = "logit"), data = train)

coef <- glm.modell$coefficients

sort(glm.modell$coefficients,decreasing = TRUE) [1:10]
sort(glm.modell$coefficients,decreasing = FALSE) [1:10]

predict.glm <- predict(glm.modell, newdata= test, type = 'response') 

auc(test$weaponfound,predict.glm)

# Code works with advantages --> Relatively fast and also more stable results than method="FAST"

##### Sixth Option: glinternet

# Adds automaticaly interaction terms to LASSO

#Categorical variables must be turned into intergers starting from 0
# get the numLevels vector containing the number of categories
X <- train
i_num <- sapply(train, is.numeric)

X[, !i_num] <- apply(X[, !i_num], 2, factor) %>% as.data.frame()
numLevels <- X %>% sapply(nlevels)
numLevels[numLevels==0] <- 1

# make the categorical variables take integer values starting from 0
X[, !i_num] <- apply(X[, !i_num], 2, function(col) as.integer(as.factor(col)) - 1)
                     
y <- train$weaponfound

fit <- glinternet(X, y, numLevels, numCores=2, family= "binomial")

plot(fit)
   
i_1Std <- which(cv_fit$lambdaHat1Std == cv_fit$lambda)
coefs <- coef(cv_fit$glinternetFit)[[i_1Std]]
                    
coefs$interactions
coefs$interactionsCoef$contcont
coefs$interactionsCoef$catcont
coefs$interactionsCoef$catcat


   
