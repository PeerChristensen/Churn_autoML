#  Train script
# "2019-11-05"
# Peer Christensen

Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(h2o)
library(caret)
library(recipes)

##################################################
# Load train, valid and test data

train_data <- read_csv("preprocessed_data/train_data_new.csv") %>%
  select(-Customer_Key) %>%
  mutate_if(is.character,factor) %>%
  mutate(Churned30 = factor(Churned30))
# valid_data <- read_csv("preprocessed_data/valid_data_new1.csv") %>%
#   select(-Customer_Key) %>%
#   mutate_if(is.character,factor)  %>%
#   mutate(IsFree = factor(IsFree),
#          Perm_anyperm = factor(Perm_anyperm),
#          Churned30 = factor(Churned30)) %>%
#   select(Churned30,vars_keep)
test_data  <- read_csv("preprocessed_data/test_data_new1.csv") %>%
  select(-Customer_Key) %>%
  mutate_if(is.character,factor)  %>%
  mutate(Churned30 = factor(Churned30))
  

####################################################
# start H2O

h2o.init(nthreads = -1)

h2o.no_progress()

train_hf <- as.h2o(train_data)
test_hf <- as.h2o(test_data)
#valid_hf <- as.h2o(valid_data)

response <- "Churned30"
features <- setdiff(colnames(train_hf), response)

train_hf[, response] <- as.factor(train_hf[, response])
#valid_hf[, response] <- as.factor(valid_hf[, response])
test_hf[, response] <- as.factor(test_hf[, response])

summary(train_hf$Churned30, exact_quantiles = TRUE)
summary(test_hf$Churned30, exact_quantiles = TRUE)
#summary(valid_hf$Churned30, exact_quantiles = TRUE)

train_hf <- na.omit(train_hf)
test_hf  <- na.omit(test_hf)
#valid_hf <- na.omit(valid_hf)

########################################## 
# Train

aml <- h2o.automl(x = features, 
                  y = response,
                  training_frame = train_hf,
                  balance_classes = TRUE,
                  max_runtime_secs = 3600*5,
                  nfolds = 10)

aml@leaderboard

# save all models in leaderboard
for (i in 1:nrow(aml@leaderboard)) {
  
  aml_model = h2o.getModel(aml@leaderboard[i, 1])
  h2o.saveModel(object = aml_model, "models10")
}

# rename file to identify the best model
aml@leader@model_id



