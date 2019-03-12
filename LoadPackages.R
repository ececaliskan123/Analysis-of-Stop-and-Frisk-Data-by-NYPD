# ******************************
#     LOAD REQUIRED PACKAGES
# ******************************

if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("klaR")) install.packages("klaR"); library("klaR")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("anytime")) install.packages("anytime"); library("anytime")
if(!require("glmnet")) install.packages("glmnet"); library("glmnet") 
if(!require("parglm")) install.packages("parglm"); library("parglm") 
if(!require("glinternet")) install.packages("glinternet"); library("glinternet") 
if(!require("ModelMetrics")) install.packages("ModelMetrics"); library("ModelMetrics") #for fct AUC
if(!require("data.table")) install.packages("data.table"); library("data.table") 
if(!require("foreign")) install.packages("foreign"); library("foreign") 
if(!require("ggmap")) install.packages("ggmap"); library("ggmap")  # get map
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("reshape")) install.packages("reshape"); library("reshape") # to melt
if(!require("devtools")) install.packages("devtools"); library("devtools") # source, install
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")
if(!require("cowplot")) install.packages("cowplot"); library("cowplot")
if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("sf")) install.packages("sf"); library("sf") # to deal with spatial data



library(base)
# to be continued
