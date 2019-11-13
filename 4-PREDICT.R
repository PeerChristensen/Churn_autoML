# predict on new data
# "2019-11-12"
# Peer Christensen

library(h2o)
library(tidyverse)
library(recipes)

h2o.init(nthreads = -1)

##################################################
# LOAD & PREPROCESS NEW DATA

new_data <- read_csv("churn_data_current.csv") %>%
  select(-HasLittPref) %>%
  mutate(
    IsFree = factor(IsFree)) %>%
  mutate_if(is.character,factor) %>%
  dplyr::select(-`FirstOrderDate within 3 months`)

Customer_Key <- new_data$Customer_Key

new_data <- new_data %>%
  select(-Customer_Key)

new_data$AverageOrderSize <- new_data$TotalNetRevenue / new_data$TotalOrderCount

today <- max(as.Date(new_data$DateStatus)) 
# today <- as.Date(now()) 

new_data$DateLatestSignup <- today - lubridate::days(new_data$DaysSinceLatestSignup)
new_data$DateLatestSignup_month <- lubridate::month(new_data$DateLatestSignup)

new_data <- new_data %>%
  dplyr::select(-DateStatus,-DateLatestSignup)

recipe_churn <- readRDS("recipe_preprocess.rds")

new_data <- bake(recipe_churn, new_data = new_data) %>%
  select(-Churned30)

new_hf <- as.h2o(new_data)

############################################
# LOAD MODEL

model_path <- glue::glue("models2/{list.files('models2', pattern = 'best')}")

mod <- h2o.loadModel(model_path)

#############################################
# PREDICT

predictions <- h2o.predict(mod,new_hf) %>%
  as_tibble()
  
table(predictions$predict)
prop.table(table(predictions$predict))

new_predictions <- predictions %>%
  mutate(predict = if_else(p1 >= 0.11,1,0))

table(new_predictions$predict)
prop.table(table(new_predictions$predict))

new_predictions$Customer_Key <- Customer_Key

#############################################
# ADD EXPLANATIONS

train_data <- read_csv("preprocessed_data/train_data.csv") %>%
  select(-Customer_Key) %>%
  mutate_if(is.character,factor) %>%
  mutate(IsFree = factor(IsFree),
         Churned30 = factor(Churned30))

# run lime() on training set
explainer <- lime::lime(x = train_data, 
                        model = mod)

new_data_Churn <- new_data %>%
  mutate(Churned30 = factor(new_predictions$predict)) %>%
  select(Churned30,everything())

new_data_Churn <- new_data_Churn[names(train_data)] # order columns

# run explain() on the explainer
explanation <- lime::explain(x = new_data_Churn[1:5, ], 
                             explainer = explainer, 
                             labels = "p1",
                             n_features = 3,
                             kernel_width = 0.5)

lime::plot_explanations(explanation)
lime::plot_features(explanation)

case_numbers <- tibble(Customer_Key,case = as.character(1:length(Customer_Key)))

output_data <- explanation %>% 
  dplyr::select(case, feature,feature_value,feature_desc,model_prediction) %>%
  left_join(case_numbers) %>%
  dplyr::select(Customer_Key,everything()) %>%
  left_join(new_predictions)

#############################################
# SAVE PREDICTIONS

write_csv(output_data,"churn_predicted.csv")

