
df <- readRDS("df.rds")
if(!require("speedglm")) install.packages("speedglm"); library("speedglm")


# Split the data into training and test sets.
df$year <- year(df$formated_date) #The error message can be ignored; the code works fine.
train <- subset(df, year== 2013 | year== 2014)
test <- subset(df, year== 2015 | year== 2016)
df$year <- NULL


## Feature Selection 
#Fisher Score 
# Define a function to calculate the Fisher score 
fisherScore <- function(feature, targetVariable){

  classMeans <- tapply(feature, targetVariable, mean)
  classStds <- tpply(feature, targetVariable, sd)
  classDiff <- abs(diff(classMeans))
  score <- as.numeric(classDiff / sqrt(sum(classStds^2)))
  return(score)
}

# Calculate the Fisher score for each numeric variable in the dataset

fisher_scores <- apply(train[,sapply(train, is.numeric)], 
                       2, fisherScore, train$weaponfound)

# Information Value (based on WoE) 
#Check the relation between target and categorical variables in the dataset.
woe.object <- woe(weaponfound ~ ., data = train, zeroadj = 0.5)
# As a rule of thumb: <0.02: not predictive, 0.02-0.1: weak, 0.1-0.3: medium, >0.3: strong
woe.object$IV

#Preparing the data before  regression 
#Remove the variables which are out of scope.
df[, c("xcoord", "ycoord", "perobs", "formated_date", "timestop", "offunif" , "crimsusp", "CPW")] <-NULL

# Include Month and Precinct as factors instead of integers
df[, c("pct", "month")] <- apply(df[, c("pct", "month")], 2, FUN = as.character)

# Convert characters into factor variables before regression.

chrIdx <- which(sapply(df, is.character))
df[, chrIdx] <- lapply(df[, chrIdx], factor)

glm <- speedglm(weaponfound ~.*., data= train, family = binomial(link="logit"))

# sgd Package for Logit regression wih Stochastic Gradient Descent
#logitSGD <- sgd(weaponfound ~ . + .*., data = train, model= "glm", model.control= binomial(link="logit")) 
# sgd package is removed from CRAN repository. Older versions cannot be manually installed from Archive.  ERROR: NON-ZERO EXIT STATUS

# Regularized Logit Model 

# Among other information, it includes the formula of the elastic net between lasso and ridge
#vignette("glmnet_beta")

# The main function seems to be glmnet(), so let's look at it
?glmnet

char_vars <- names(sapply(df, is.character)) 
df[, num_vars] <- sapply(df[, num_vars], FUN = as.factor)

# Glmnet doesn't have formula interface, create model.matrix which expand factors into dummies

x <- model.matrix(weaponfound~.*. -1, df)
y <- df$weaponfound

lasso <- glmnet(x = x, y = y, family = "binomial", standardize = TRUE,
                alpha = 1, nlambda = 100) # standardized numbers+ ONLY factors
# Check out the model
lasso
#  Plot the coefficients at different values of lambda
plot(lasso, xvar = "lambda")
plot(lasso, xvar = "dev") 
# Deviance is a goodness-of-fit criterion

# How to choose the best lambda value? We will discuss this later in the course.
# For now, let's look at the deviance or pseudo-R^2
str(lasso)
plot(y = lasso$dev.ratio, x = lasso$lambda)

# By eyeballing the curve, let's (arbitrarily!) decide to pick lambda=0.02
coef(lasso, 0.01)
pred.lasso <- predict(lasso, newx = x, s = 0.01, type = "response")

accuracy.lasso <- Accuracy(pred.lasso, class = loans$BAD)

elasticnet <- glmnet(x = x, y = y, family = "binomial", standardize = TRUE,
                alpha = 0.5, nlambda = 100)
plot(y = elasticnet$dev.ratio, x = elasticnet$lambda)
coef(elasticnet, 0.01)
pred.elasticnet <- predict(elasticnet, newx = x, s = 0.01, type = "response")
accuracy.elasticnet <- Accuracy(pred.elasticnet, class = test$weaponfound)
auc(test$weaponfound,pred.elasticnet)

# Compare coefficients of standard and regularized logit models
#round(coef(lr), 2)
round(coef(elasticnet, 0.01), 2)[,1]

#Second Alternative: glinternet

# Adds automaticaly interaction terms to LASSO

#Categorical variables must be turned into intergers starting from 0
# get the numLevels vector containing the number of categories
X <- df
i_num <- sapply(df, is.numeric)

X[, !i_num] <- apply(X[, !i_num], 2, factor) %>% as.data.frame()
numLevels <- X %>% sapply(nlevels)
numLevels[numLevels==0] <- 1

# make the categorical variables take integer values starting from 0
X[, !i_num] <- apply(X[, !i_num], 2, function(col) as.integer(as.factor(col)) - 1)
                     

set.seed(1001)
cv_fit <- glinternet.cv(X, y, numLevels)
plot(cv_fit)
   
i_1Std <- which(cv_fit$lambdaHat1Std == cv_fit$lambda)
coefs <- coef(cv_fit$glinternetFit)[[i_1Std]]
                    
   coefs$interactions
                     coefs$interactionsCoef$contcont
                     coefs$interactionsCoef$catcont
                     coefs$interactionsCoef$catcat


   
