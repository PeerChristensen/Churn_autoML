# Variable selection with elastic net regularization and variable importance
# "2019-11-05"
# Peer Christensen

############################################
# RESULTS

# bad predictors:

# DaysSinceLatestOrder
# TotalNetRevenue
# AverageOrderSize
# ProductPlanType

#############################################

library(tidyverse)
library(h2o)
library(caret)
library(recipes)

df <- read_csv("churn_data_training.csv") %>%
  mutate(#Churned30 = factor(Churned30),
    IsFree = factor(IsFree)) %>%
  mutate_if(is.character,factor) %>%
  select(-Customer_Key,-`FirstOrderDate within 3 months`) 

# New features

df$AverageOrderSize <- df$TotalNetRevenue / df$TotalOrderCount

today <- max(as.Date(df$DateStatus)) 
# today <- as.Date(now()) 

df$DateLatestSignup <- today - lubridate::days(df$DaysSinceLatestSignup)
df$DateLatestSignup_month <- lubridate::month(df$DateLatestSignup)

# behaviour?

df <- df %>%
  select(-DateStatus,-DateLatestSignup)

# feature selection


library(glmnet)
library(ISLR)

x = model.matrix(Churned30 ~ . - 1, data = df)
y = df$Churned30




cv.lasso = cv.glmnet(x, y)
plot(cv.lasso)
coef(cv.lasso)


cvfit = cv.glmnet(x, y)
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")



####################################
# Partition

set.seed(42)
index <- createDataPartition(df$Churned30, p = 0.7, list = FALSE)

train_data <- df[index, ]
test_data  <- df[-index, ]

index2 <- createDataPartition(test_data$Churned30, p = 0.5, list = FALSE)

valid_data <- test_data[-index2, ]
test_data <- test_data[index2, ]

####################################
# Preprocess

recipe_churn <- recipe(Churned30 ~ ., train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  step_nzv(all_predictors(), -all_outcomes()) %>%
  step_YeoJohnson(all_predictors(), -all_outcomes()) %>%
  prep(data = train_data)

train_data <- bake(recipe_churn, new_data = train_data) %>%
  select(Churned30, everything())

valid_data <- bake(recipe_churn, new_data = valid_data) %>%
  select(Churned30, everything())

test_data <- bake(recipe_churn, new_data = test_data) %>%
  select(Churned30, everything())

# H2O frames

h2o.init(nthreads = -1)

h2o.no_progress()

train_hf <- as.h2o(train_data)
test_hf <- as.h2o(test_data)
valid_hf <- as.h2o(valid_data)

response <- "Churned30"
features <- setdiff(colnames(train_hf), response)

train_hf[, response] <- as.factor(train_hf[, response])
valid_hf[, response] <- as.factor(valid_hf[, response])
test_hf[, response] <- as.factor(test_hf[, response])

summary(train_hf$Churned30, exact_quantiles = TRUE)
summary(test_hf$Churned30, exact_quantiles = TRUE)
summary(valid_hf$Churned30, exact_quantiles = TRUE)

train_hf <- na.omit(train_hf)
test_hf  <- na.omit(test_hf)
valid_hf <- na.omit(valid_hf)

########################################## 
# Train

aml <- h2o.automl(x = features, 
                  y = response,
                  training_frame = train_hf,
                  #validation_frame = valid_hf,
                  balance_classes = TRUE,
                  max_runtime_secs = 60*5)


aml@leaderboard

# save all models in leaderboard
# for (i in 1:nrow(aml@leaderboard)) {
#   
#   aml_model = h2o.getModel(aml@leaderboard[i, 1])
#   h2o.saveModel(object = aml_model, "models2")
# }

# rename file

h2o.varimp_plot(aml@leader)

