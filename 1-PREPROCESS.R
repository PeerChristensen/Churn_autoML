# preprocessing train and test sets
# "2019-11-11"
# Peer Christensen

library(tidyverse)
library(h2o)
library(caret)
library(recipes)

df <- read_csv("churn_data_training.csv") %>%
  mutate(
    IsFree = factor(IsFree),Churned30 = factor(Churned30)) %>%
  mutate_if(is.character,factor) %>%
  dplyr::select(-`FirstOrderDate within 3 months`)

Customer_Key <- df$Customer_Key

df <- df %>%
  select(-Customer_Key)

##################################################
# FEATURE ENGINEERING

df$AverageOrderSize <- df$TotalNetRevenue / df$TotalOrderCount

today <- max(as.Date(df$DateStatus)) 
# today <- as.Date(now()) 

df$DateLatestSignup <- today - lubridate::days(df$DaysSinceLatestSignup)
df$DateLatestSignup_month <- lubridate::month(df$DateLatestSignup)

df <- df %>%
  dplyr::select(-DateStatus,-DateLatestSignup)

##################################################
# FEATURE SELECTION

# see variable selection script

# remove correlated variables

cor_mat <- df %>%
  select_if(is.numeric) %>%
  drop_na() %>%
  cor()

cor_mat[upper.tri(cor_mat)] <- 0
diag(cor_mat) <- 0

cor_mat <- abs(cor_mat) > .95

# row-wise removal, starting with col 1, then removing correlated variables
whichKeep <- names(which(rowSums(lower.tri(cor_mat) * cor_mat) == 0))

if (!is.null(whichKeep)){
  df <- df %>%
    select_if(function(x) !is.numeric(x)) %>%
    cbind(df[,whichKeep])
}

####################################
# Partition

df$Customer_Key <- as.character(Customer_Key)

set.seed(42)
index <- createDataPartition(df$Churned30, p = 0.7, list = FALSE)

train_data <- df[index, ]
test_data  <- df[-index, ]

index2 <- createDataPartition(test_data$Churned30, p = 0.5, list = FALSE)

valid_data <- test_data[-index2, ]
test_data <- test_data[index2, ]

Customer_Key_train <- train_data$Customer_Key
Customer_Key_valid <- valid_data$Customer_Key
Customer_Key_test <- test_data$Customer_Key

train_data <- train_data %>% select(-Customer_Key)
valid_data <- valid_data %>% select(-Customer_Key)
test_data  <- test_data %>% select(-Customer_Key)

####################################
# Preprocess

recipe_churn <- recipe(Churned30 ~ ., train_data) %>%
  # step_dummy(all_nominal(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  prep(data = train_data)

train_data <- bake(recipe_churn, new_data = train_data) %>%
  select(Churned30, everything())

valid_data <- bake(recipe_churn, new_data = valid_data) %>%
  select(Churned30, everything())

test_data <- bake(recipe_churn, new_data = test_data) %>%
  select(Churned30, everything())

# save recipe
saveRDS(recipe_churn,"recipe_preprocess.rds")

# train_data$Churned30 <- factor(train_data$Churned30)
# valid_data$Churned30 <- factor(valid_data$Churned30)
# test_data$Churned30  <- factor(test_data$Churned30)

summary(train_data$Churned30, exact_quantiles = TRUE)
summary(test_data$Churned30, exact_quantiles = TRUE)
summary(valid_data$Churned30, exact_quantiles = TRUE)

train_data$Customer_Key <- Customer_Key_train
valid_data$Customer_Key <- Customer_Key_valid
test_data$Customer_Key <- Customer_Key_test


write_csv(train_data,"preprocessed_data/train_data.csv")
write_csv(valid_data,"preprocessed_data/valid_data.csv")
write_csv(test_data,"preprocessed_data/test_data.csv")



