# performance
# "2019-11-06"
# Peer Christensen

library(tidyverse)
library(h2o)
library(modelplotr)
library(lime)

red   <- "#c51924"
blue  <- "#028ccc"
green <- "#16b84c"

h2o.init()

####################################################
# LOAD MODEL AND TEST SET

model_path <- glue::glue("models2/{list.files('models2', pattern = 'best')}")

mod <- h2o.loadModel(model_path)

train_data  <- read_csv("preprocessed_data/train_data.csv") %>%
  mutate(Churned30 = factor(Churned30),
         IsFree = factor(IsFree))

test_data  <- read_csv("preprocessed_data/test_data.csv")  %>%
  mutate(Churned30 = factor(Churned30),
         IsFree = factor(IsFree))  

Customer_Key <- test_data$Customer_Key

train_data <- train_data %>%
  select(-Customer_Key)

test_data <- test_data %>%
  select(-Customer_Key)

train_hf <- as.h2o(train_data)
test_hf <- as.h2o(test_data)

####################################################
# CREATE PERFORMANCE OBJECT AND PLOT METRICS

perf <- h2o.performance(mod,test_hf)

metrics <- as.data.frame(h2o.metric(perf))
head(metrics)

# all metrics
metrics %>%
  select(-tns,-fps,-tps,-fns) %>%
  gather(metric, value, f1:tpr) %>%
  ggplot(aes(x = threshold, y = value, group = metric)) +
  facet_wrap(~ metric, ncol = 4, scales = "free") +
  geom_line() +
  theme_minimal()

# f2, tpr, tnr
intersect_threshold <- metrics %>% filter(round(tpr,3) == round(tnr,3)) %>%
  pull(threshold) %>%
  first()

intersect_max <- metrics %>% filter(round(tpr,3) == round(tnr,3)) %>% 
  pull(tpr)
  
f2_threshold <- metrics %>% filter(f2 == max(f2)) %>% pull(threshold)

f2_max <-  metrics %>% filter(f2 == max(f2)) %>% pull(f2)

metrics %>%
  select(threshold,tpr,tnr,f2) %>%
  gather(metric,value,-threshold) %>%
  ggplot(aes(x=threshold,y=value,colour=metric)) +
    geom_line(size = 1.2) +
  theme_minimal() +
  scale_colour_manual(values = c(red,blue,green)) +
  geom_segment(aes(x = intersect_threshold,xend = intersect_threshold,
                   y= 0, yend = intersect_max),
               color = "black",linetype="dashed",size=1) +
  geom_segment(aes(x = f2_threshold,xend = f2_threshold,
                   y= 0, yend = f2_max),
               color = "black",linetype="dashed",size=1) +
  annotate("text", label = round(intersect_threshold,2),
           x = intersect_threshold, y = intersect_max +.05, size = 5) +
  annotate("text", label = paste0("max F2 = ",round(f2_threshold,2)),
           x = f2_threshold+.05, y=f2_max+.05,size = 5)

# mcc, f1, f2, precision, recall

metrics %>%
  select(threshold,precision,recall,f1,f2,absolute_mcc) %>%
  gather(metric,value,-threshold) %>%
  ggplot(aes(x=threshold,y=value,colour=metric)) +
  geom_line(size = 1.2) +
  theme_minimal()

# PRROC

metrics %>%
  ggplot(aes(recall,precision)) + 
  geom_line(size = 1.5) +
  theme_minimal()

####################################################
# Confusion matrices

h2o.confusionMatrix(perf,metrics = "f2")
h2o.confusionMatrix(perf,metrics = "absolute_mcc")
h2o.confusionMatrix(perf,metrics = "accuracy")

# in proportions
cm_f2 <- h2o.confusionMatrix(perf,metrics = "f2") %>%
  as_tibble()
round(prop.table(cm_f2[1:2,1:2]),3)

cm_mcc <- h2o.confusionMatrix(perf,metrics = "absolute_mcc") %>%
  as_tibble()
round(prop.table(cm_mcc[1:2,1:2]),3)

cm_acc <- h2o.confusionMatrix(perf,metrics = "accuracy") %>%
  as_tibble()
round(prop.table(cm_acc[1:2,1:2]),3)

####################################################
# GLOBAL EXPLANATIONS

h2o.varimp(mod)
h2o.varimp_plot(mod)

####################################################
# LOCAL EXPLANATIONS

# Xtrain <- as.data.frame(train_hf)
# Xtest <- as.data.frame(test_hf)


# run lime() on training set
explainer <- lime::lime(x = train_data, 
                        model = mod)

# run explain() on the explainer
explanation <- lime::explain(x = test_data[1:5, ], 
                             explainer = explainer, 
                             labels = "p1",
                             n_features = 3,
                             kernel_width = 0.5)

lime::plot_explanations(explanation)
lime::plot_features(explanation)

case_numbers <- tibble(Customer_Key,case = as.character(1:length(Customer_Key)))

explanation %>% 
  dplyr::select(case, feature,feature_value,feature_desc,model_prediction) %>%
  left_join(case_numbers) %>%
  dplyr::select(Customer_Key,everything())

####################################################
# Performance plots

scores_and_ntiles <- prepare_scores_and_ntiles(datasets=list("train_data","test_data"),
                                               models = list("mod"),  
                                               model_labels = list("GBM"), 
                                               target_column="Churned30",
                                               ntiles = 100)

scores_and_ntiles <- scores_and_ntiles %>%
  rename("ntl_0" = ntl_p0,"ntl_1" = ntl_p1)

plot_input <- plotting_scope(prepared_input = scores_and_ntiles,scope="compare_datasets")

#Cumulative gains
plot_cumgains(data = plot_input, highlight_ntile = 20,
              highlight_how = "text")

#Cumulative lift
plot_cumlift(data = plot_input, highlight_ntile = 20,
             highlight_how = "text")

#Response plot
plot_response(data = plot_input)

#Cumulative response plot
plot_cumresponse(data = plot_input)

#plot multiple
plot_multiplot(data = plot_input)

# ROI
plot_roi(data = plot_input,
         fixed_costs = 1000,
         variable_costs_per_unit = 10,
         profit_per_unit = 50,
         highlight_ntile = "max_roi",
         highlight_how = "text")

# Cost-revenue
plot_costsrevs(data = plot_input,fixed_costs = 1000,
               variable_costs_per_unit = 10,
               profit_per_unit = 50,
               highlight_ntile = "max_roi",
               highlight_how = "text")

# Profit
plot_profit(data = plot_input,fixed_costs = 1000,
            variable_costs_per_unit = 10,
            profit_per_unit = 50,
            highlight_ntile = "max_profit",
            highlight_how = "text")
