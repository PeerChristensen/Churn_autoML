# preprocessing train and test sets
# "2019-11-11"
# Peer Christensen

Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(h2o)
library(caret)
library(recipes)

# df <- read_csv("churn_data_training.csv") %>%
df <- read_csv("new_churn_training5.csv") %>%
  mutate(Perm_anyperm = factor(Perm_anyperm),
         Churned30    = factor(Churned30),
         Perm_recommendations = factor(Perm_recommendations),
         Perm_newsletter      = factor(Perm_newsletter),
         MatasUser = factor(MatasUser),
         CoopUser  = factor(CoopUser)) %>%
  mutate_if(is.character, factor) %>%
  select(-IsFree) %>%
  drop_na() %>%
  filter(DaysSinceLatestSignup > 30)

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

# add F_S ratio

f_lit_mean <- df %>% 
  select(starts_with("F0")) %>%
  transmute(m = rowMeans(.)) %>%
  pull(m)

s_lit_mean <- df %>% 
  select(starts_with("S0")) %>%
  transmute(m = rowMeans(.))  %>%
  pull(m)

f_ratio <- f_lit_mean / (s_lit_mean +f_lit_mean)

#PCA
s_lit_vars <- df %>% 
  select(starts_with("S0")) %>%
           names()

f_lit_vars <- df %>% 
  select(starts_with("F0")) %>%
  names()

pca_s <- prcomp(df[s_lit_vars], scale = FALSE) 
pca_f <- prcomp(df[f_lit_vars], scale = FALSE)

df <- df %>%
  mutate(
    pc_s1 = pca_s$x[,1],
    pc_s2 = pca_s$x[,2],
    pc_f1 = pca_f$x[,1],
    pc_f2 = pca_f$x[,2]
  )

#remove original f-s variables

df <- df %>%
  select(-f_lit_vars,-s_lit_vars)

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


write_csv(train_data,"preprocessed_data/train_data_new.csv")
write_csv(valid_data,"preprocessed_data/valid_data_new.csv")
write_csv(test_data,"preprocessed_data/test_data_new.csv")

#write_csv(train_data,"preprocessed_data/train_data.csv")
#write_csv(valid_data,"preprocessed_data/valid_data.csv")
#write_csv(test_data,"preprocessed_data/test_data.csv")



