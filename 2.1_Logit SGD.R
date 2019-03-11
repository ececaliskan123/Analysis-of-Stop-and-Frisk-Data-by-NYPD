if(!require("glmnet")) install.packages("glmnet"); library("glmnet") 
if(!require("glinternet")) install.packages("glinternet"); library("glinternet") 
if(!require("dplyr")) install.packages("dplyr"); library("dplyr") 
if(!require("ModelMetrics")) install.packages("ModelMetrics"); library("ModelMetrics") 



df <- readRDS("df.rds")


df[, c("xcoord", "ycoord", "perobs", "formated_date", "timestop", "offunif" , "crimsusp", "CPW")] <-NULL
#df$pct <- as.character(df$pct) numericten factore mi gecmeliyim?

# Split the data into training and test sets.
df$year <- year(df$formated_date)
train <- subset(df, year== 2013 | year== 2014)
test <- subset(df, year== 2015 | year== 2016)
df$year <- NULL

# Convert characters into factor variables before regression.

chrIdx <- which(sapply(df, is.character))
df[, chrIdx] <- lapply(df[, chrIdx], factor)

#logitSGD <- sgd(weaponfound ~ . + .*., data = train, model= "glm", model.control= binomial(link="logit")) 
#sgd cannot be installed from Archive older versions ERROR: NON-ZERO EXIT STATUS

# Regularized Logit Model 

# Among other information, it includes the formula of the elastic net between lasso and ridge
#vignette("glmnet_beta")

# The main function seems to be glmnet(), so let's look at it
?glmnet

char_vars <- names(sapply(df, is.character)) 
df[, num_vars] <- sapply(df[, num_vars], FUN = as.factor)

# Glmnet function doesn't have formula interface, create model.matrix which expand factors into dummies

x <- model.matrix(weaponfound~. -1, df)
y <- df$weaponfound

lasso <- glmnet(x = x, y = y, family = "binomial", standardize = TRUE,
                alpha = 1, nlambda = 100) # standardized numbers+ ONLY factors
# Check out the model
lasso
# The vignette also tells us that we can plot the coefficients
# at different values of lambda
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


   
