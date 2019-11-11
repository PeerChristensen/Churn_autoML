# Variable selection with elastic net regularization and variable importance
# "2019-11-05"
# Peer Christensen

############################################
# RESULTS

# bad predictors from regularization:

#TotalOrderCount                        
#TotalNetRevenue        
#DaysSinceFirstOrder    
#PlusOrderCount  

# But these are often used by ML models

# Conclusion: we leave these variables in the data


#############################################

library(tidyverse)
library(h2o)
library(caret)
library(recipes)

df <- read_csv("churn_data_training.csv") %>%
  mutate(#Churned30 = factor(Churned30),
    IsFree = factor(IsFree),
    Churned30 = factor(Churned30)) %>%
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

cvfit = cv.glmnet(x, y, family = "binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")

df %>% ggplot(aes(x=factor(Churned30),y=log(PlusQuantityBought))) +
  geom_boxplot()

coef(cvfit, s = "lambda.min")

model_path <- glue::glue("models2/{list.files('models2', pattern = 'best')}")

mod <- h2o.loadModel(model_path)

h2o.varimp(mod)

h2o.varimp_plot(mod)

