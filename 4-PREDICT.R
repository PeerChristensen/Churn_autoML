# predict on new data
# "2019-11-12"
# Peer Christensen

Sys.setlocale("LC_ALL","English")

library(h2o)
library(tidyverse)
library(recipes)
library(RODBC)

h2o.init(nthreads = -1)

##################################################
# LOAD & PREPROCESS NEW DATA

channel <-odbcConnect("saxo034", uid="R", pwd="sqlR2017")

sqlquery <- "SELECT * FROM [DataMartMisc].[machinelearning].[ChurnPredict3]"

df <- sqlQuery(channel, sqlquery) %>% 
  as_tibble() %>%
  mutate(
    Perm_anyperm    = factor(Perm_anyperm),
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


# Postal code

df <- df %>%
  mutate(PostalCode = as.character(PostalCode)) %>%
  mutate(PostalCode = if_else(str_length(PostalCode) == 4,PostalCode,"Ukendt")) %>%
  mutate(PostalCode = case_when(str_starts(PostalCode,"0") ~ "organisationer og virksomheder",
                                str_starts(PostalCode,"1") ~ "København",
                                str_starts(PostalCode,"2") ~ "København og omegn",
                                str_starts(PostalCode,"30") ~ "Nordsjælland",
                                str_starts(PostalCode,"37") ~ "Bornholm",
                                str_starts(PostalCode,"38|39") ~ "Grønland og Færøerne",
                                str_starts(PostalCode,"4") ~ "Sjælland og øer",
                                str_starts(PostalCode,"5") ~ "Fyn",
                                str_starts(PostalCode,"6") ~ "Sønderjylland",
                                str_starts(PostalCode,"7") ~ "Vestjylland",
                                str_starts(PostalCode,"8") ~ "Øst- og Midtjylland",
                                str_starts(PostalCode,"8") ~ "Nordjylland",
                                PostalCode == "Ukendt"     ~ "Ukendt")) %>%
  mutate(PostalCode = as.factor(PostalCode))

recipe_churn <- readRDS("recipe_preprocess.rds")

new_data <- bake(recipe_churn, new_data = df) %>%
  select(-Churned30)

new_hf <- as.h2o(new_data)

############################################
# LOAD MODEL

model_path <- glue::glue("models/{list.files('models', pattern = 'best')}")

mod <- h2o.loadModel(model_path)

#############################################
# PREDICT

predictions <- h2o.predict(mod,new_hf) %>%
  as_tibble()
  
table(predictions$predict)
prop.table(table(predictions$predict))

new_predictions <- predictions %>%
  mutate(predict = if_else(p1 >= 0.6,1,0))

table(new_predictions$predict)
prop.table(table(new_predictions$predict))

new_predictions <- new_predictions %>%
  mutate(Customer_Key = Customer_Key) %>%
  select(Customer_Key,everything())

#############################################
# SAVE PREDICTIONS

write_csv(new_predictions,"output_data/churn_output.csv")

#############################################
# LOCAL EXPLANATIONS

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
  select(Churned30,everything()) %>%
  select(names(train_data)) %>%    # order columns
  mutate(Customer_Key = Customer_Key) %>% # for joining later
  filter(Churned30 == "1")  # select only churners
  
Customer_Key_churn = new_data_Churn$Customer_Key
  
new_data_Churn <- new_data_Churn %>%
  select(-Customer_Key)

# run explain() on the explainer
explanation <- lime::explain(x = new_data_Churn[1:5,], 
                             explainer = explainer, 
                             labels = "p1",
                             n_features = 3,
                             kernel_width = 0.5)

#lime::plot_explanations(explanation)
lime::plot_features(explanation)
#ggsave("figures/local_explanation_churners.png",height = 10, width = 11)

case_numbers <- tibble(Customer_Key = Customer_Key_churn,case = as.character(1:length(Customer_Key_churn)))

churn_output_explanation <- explanation %>% 
  dplyr::select(case, feature,feature_weight,feature_desc) %>%
  left_join(case_numbers) %>%
  select(Customer_Key, everything()) %>%
  left_join(new_predictions)

write_csv(churn_output_explanation,"output_data/churn_output_explanation.csv")

