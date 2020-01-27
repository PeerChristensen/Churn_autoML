# predict on new data
# "2019-11-12"
# Peer Christensen

Sys.setlocale("LC_ALL","English")

library(h2o)
library(recipes)
library(RODBC)
library(tidyverse)

h2o.init(nthreads = -1)

##################################################
# LOAD & PREPROCESS NEW DATA

channel <-odbcConnect("saxo034", uid="R", pwd="sqlR2017")

sqlquery <- "SELECT * FROM [DataMartMisc].[machinelearning].[ChurnPredict2020]"

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

df <- df %>% select(-missing_col_names)

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
  mutate(PostalCode = if_else(is.na(PostalCode),"Ukendt",PostalCode)) %>%
  mutate(PostalCode = as.factor(PostalCode))

recipe_churn <- readRDS("recipe_preprocess.rds")

new_data <- bake(recipe_churn, new_data = df)

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

# load f2 threshold
f2_threshold <- read_rds("f2_threshold.rds")

new_predictions <- predictions %>%
  mutate(predict = if_else(p1 >= f2_threshold,1,0))

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
explanation <- lime::explain(x = new_data_Churn, 
                             explainer = explainer, 
                             labels = "p1",
                             n_features = 6,
                             kernel_width = 0.5) # check speed with different batch sizes

#lime::plot_explanations(explanation)
#lime::plot_features(explanation)
#ggsave("figures/local_explanation_churners.png",height = 10, width = 11)

case_numbers <- tibble(Customer_Key = Customer_Key_churn,
                       case = as.character(1:length(Customer_Key_churn)))

#prepare original data for joining
df$Customer_Key <- Customer_Key

df_long <- df %>% map_df(as.character) %>%pivot_longer(-Customer_Key,names_to ="feature")

churn_output_explanation <- explanation %>% 
  dplyr::select(case, feature,feature_weight) %>%
  filter(feature_weight >0) %>%
  group_by(case) %>%
  top_n(3) %>%
  ungroup() %>%
  left_join(case_numbers) %>%
  full_join(new_predictions) %>%
  select(Customer_Key,feature,probability = p1) %>%
  #join actual data
  mutate(Customer_Key = as.character(Customer_Key)) %>%
  inner_join(df_long, by = c("Customer_Key","feature")) %>%
  group_by(Customer_Key) %>%
  mutate(explanation_num = paste0("explanation",row_number()),
         value_num = paste0("value",row_number())) %>%
  # pivot_wider
  pivot_wider(id_cols=c(Customer_Key, probability),
              names_from = c(explanation_num,value_num),
              values_from = c(feature,value))

new_predictions2 <- new_predictions %>%
  mutate(Customer_Key = as.character(Customer_Key)) %>%
  select(Customer_Key,probability=p1)

colnames(churn_output_explanation) <- c("Customer_Key","probability","explanation1","explanation2","explanation3","value1","value2","value3")
churn_output_explanation <- churn_output_explanation %>% select(Customer_Key,probability,explanation1,value1,explanation2,value2,explanation3,value3)

churn_output_explanation <- churn_output_explanation %>% right_join(new_predictions2)

write_csv(churn_output_explanation,"output_data/churn_output_explanation.csv")

# to SQL
channel <-odbcConnect("saxo034", uid="R", pwd="sqlR2017")

sqlSave(channel,churn_output_explanation, tablename = "churnOutput2020",rownames = F,safer=T)

close(channel)

# export threshold
threshold_df <- tibble(f2_threshold = f2_threshold)
sqlSave(channel,threshold_df, tablename = "churnThreshold2020",rownames = F,safer=T)



