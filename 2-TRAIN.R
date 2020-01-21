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

train_data <- read_csv("preprocessed_data/train_data.csv") %>%
  select(-Customer_Key) %>%
  mutate_if(is.character,factor) %>%
  mutate(
    Perm_anyperm    = factor(Perm_anyperm),
    Churned30       = factor(Churned30),
    Perm_recommendations = factor(Perm_recommendations),
    Perm_newsletter = factor(Perm_newsletter),
    MatasUser       = factor(MatasUser),
    CoopUser        = factor(CoopUser)) %>%
    mutate_if(is.character,factor) %>%
    mutate_if(is.integer,as.numeric) %>%
    drop_na()


test_data  <- read_csv("preprocessed_data/test_data.csv") %>%
  select(-Customer_Key) %>%
  mutate_if(is.character,factor) %>%
  mutate(
    Perm_anyperm    = factor(Perm_anyperm),
    Churned30       = factor(Churned30),
    Perm_recommendations = factor(Perm_recommendations),
    Perm_newsletter = factor(Perm_newsletter),
    MatasUser       = factor(MatasUser),
    CoopUser        = factor(CoopUser)) %>%
  mutate_if(is.character,factor) %>%
  mutate_if(is.integer,as.numeric) %>%
  drop_na()


####################################################
# start H2O

h2o.init(nthreads = -1)

h2o.no_progress()

train_hf <- as.h2o(train_data)
test_hf <- as.h2o(test_data)

response <- "Churned30"
features <- setdiff(colnames(train_hf), response)

train_hf[, response] <- as.factor(train_hf[, response])

test_hf[, response] <- as.factor(test_hf[, response])

train_hf <- na.omit(train_hf)
test_hf  <- na.omit(test_hf)

########################################## 
# Train

aml <- h2o.automl(x = features, 
                  y = response,
                  training_frame = train_hf,
                  balance_classes = TRUE,
                  max_runtime_secs = 120*1,
                  nfolds = 10,
                  stopping_metric = "AUCPR",
                  sort_metric = "AUCPR")

aml@leaderboard

# save all models in leaderboard
for (i in 1:nrow(aml@leaderboard)) {
  
  aml_model = h2o.getModel(aml@leaderboard[i, 1])
  h2o.saveModel(object = aml_model, "models")
}

# rename file to identify the best model
aml@leader@model_id



