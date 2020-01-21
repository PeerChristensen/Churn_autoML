# preprocessing train and test sets
# "2019-11-11"
# Peer Christensen

Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(h2o)
library(caret)
library(recipes)
library(RODBC)

channel <-odbcConnect("saxo034", uid="R", pwd="sqlR2017")

sqlquery <- "SELECT * FROM [DataMartMisc].[machinelearning].[ChurnTraining2]"

df <- sqlQuery(channel, sqlquery) %>% 
  as_tibble() %>%
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

close(channel)

Customer_Key <- df$Customer_Key

df <- df %>%
  select(-Customer_Key)

#############################################
# FEATURE ENGINEERING

df$AverageOrderSize <- df$TotalNetRevenue / df$TotalOrderCount

today <- max(as.Date(df$DateStatus)) 
# today <- as.Date(now()) 

df$DateLatestSignup <- today - lubridate::days(df$DaysSinceLatestSignup)
df$DateLatestSignup_month <- lubridate::month(df$DateLatestSignup)

df <- df %>%
  dplyr::select(-DateStatus,-DateLatestSignup)

# add F_S ratio an PCs

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
  select(contains("S0")) %>%
  names()

f_lit_vars <- df %>% 
  select(contains("F0")) %>%
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

# remove original f-s variables

df <- df %>%
  select(-f_lit_vars,-s_lit_vars)

# Postal code - to be added, problem med nye variabler der er NuLL når Churn == 1
# 
# df <- df %>%
#   mutate(PostalCode = as.character(PostalCode)) %>%
#   mutate(PostalCode = if_else(str_length(PostalCode) == 4,PostalCode,"Ukendt")) %>%
#   mutate(PostalCode = case_when(str_starts(PostalCode,"0") ~ "organisationer og virksomheder",
#                                 str_starts(PostalCode,"1") ~ "København",
#                                 str_starts(PostalCode,"2") ~ "København og omegn",
#                                 str_starts(PostalCode,"30") ~ "Nordsjælland",
#                                 str_starts(PostalCode,"37") ~ "Bornholm",
#                                 str_starts(PostalCode,"38|39") ~ "Grønland og Færøerne",
#                                 str_starts(PostalCode,"4") ~ "Sjælland og øer",
#                                 str_starts(PostalCode,"5") ~ "Fyn",
#                                 str_starts(PostalCode,"6") ~ "Sønderjylland",
#                                 str_starts(PostalCode,"7") ~ "Vestjylland",
#                                 str_starts(PostalCode,"8") ~ "Øst- og Midtjylland",
#                                 str_starts(PostalCode,"8") ~ "Nordjylland",
#                                 PostalCode == "Ukendt"     ~ "Ukendt")) %>%
#   mutate(PostalCode = if_else(is.na(PostalCode),"Ukendt",PostalCode)) %>%
#   mutate(PostalCode = as.factor(PostalCode))


##################################################
# FEATURE SELECTION

# see variable selection script

####################################
# Partition

df$Customer_Key <- as.character(Customer_Key)

set.seed(42)
index <- createDataPartition(df$Churned30, p = 0.7, list = FALSE)

train_data <- df[index, ]
test_data  <- df[-index, ]

Customer_Key_train <- train_data$Customer_Key
Customer_Key_test <- test_data$Customer_Key

train_data <- train_data %>% select(-Customer_Key)
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

test_data <- bake(recipe_churn, new_data = test_data) %>%
  select(Churned30, everything())

# save recipe
saveRDS(recipe_churn,"recipe_preprocess.rds")

summary(train_data$Churned30, exact_quantiles = TRUE)
summary(test_data$Churned30, exact_quantiles = TRUE)

train_data$Customer_Key <- Customer_Key_train
test_data$Customer_Key <- Customer_Key_test

write_csv(train_data,"preprocessed_data/train_data.csv")
write_csv(test_data,"preprocessed_data/test_data.csv")

