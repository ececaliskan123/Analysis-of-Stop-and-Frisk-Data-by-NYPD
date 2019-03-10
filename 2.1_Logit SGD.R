if(!require("glmnet")) install.packages("glmnet"); library("glmnet") 



df <- readRDS("df.rds")


df[, c("xcoord", "ycoord", "perobs", "formated_date", "timestop", "offunif" , "crimsusp", "CPW")] <-NULL


df$year <- year(df$formated_date)
train <- subset(df, year== 2013 | year== 2014)
test <- subset(df, year== 2015 | year== 2016)
df$year <- NULL



#logitSGD <- sgd(weaponfound ~ . + .*., data = train, model= "glm", model.control= binomial(link="logit")) 
#sgd cannot be installed from Archive older versions ERROR: NON-ZERO EXIT STATUS

# Regularized Logit Model 



# Check out the package vignette
# Among other information, it includes the formula of the elastic net between lasso and ridge
#vignette("glmnet_beta")

# The main function seems to be glmnet(), so let's look at it
?glmnet
# Most importantly, we see that the glmnet function doesn't have formula interface
# So we'll need to build a numeric matrix ourselves


x <- model.matrix(weaponfound~.*. -1, df)
y <- df$weaponfound

lasso <- glmnet(x = x, y = y, family = "binomial", standardize = TRUE,
                alpha = 1, nlambda = 100)
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
accuracy.elasticnet <- Accuracy(pred.elasticnet, class = loans$BAD)

# Compare coefficients of standard and regularized logit models
#round(coef(lr), 2)
round(coef(elasticnet, 0.01), 2)[,1]

# Adds automaticaly interaction terms to LASSO
library(glinternet)
set.seed(1001)
cv_fit <- glinternet.cv(X, y, numLevels)
plot(cv_fit)
