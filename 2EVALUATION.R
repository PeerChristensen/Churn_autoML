# performance
# "2019-11-06"
# Peer Christensen

library(tidyverse)
library(h2o)
library(modelplotr)

h2o.init()

model_path <- glue::glue("models2/{list.files('models2', pattern = 'best')}")

mod <- h2o.loadModel(model_path)

perf <- h2o.performance(mod,test_hf)

metrics <- as.data.frame(h2o.metric(perf))
head(metrics)

metrics %>%
  gather(metric, value, f1:tpr) %>%
  ggplot(aes(x = threshold, y = value, group = metric)) +
  facet_wrap(~ metric, ncol = 2, scales = "free") +
  geom_line() +
  theme_minimal()

metrics %>%
  select(threshold,tpr,tnr) %>%
  gather(metric,value,-threshold) %>%
  ggplot(aes(x=threshold,y=value,colour=metric)) +
    geom_line()

metrics %>%
  select(threshold,precision,recall,f2) %>%
  gather(metric,value,-threshold) %>%
  ggplot(aes(x=threshold,y=value,colour=metric)) +
  geom_line()

metrics %>%
  ggplot(aes(recall,precision)) + 
  geom_line()

# Global explanations



h2o.varimp(mod)
h2o.varimp_plot(mod)


# LIME
Xtrain <- as.data.frame(train_hf)
Xtest <- as.data.frame(test_hf)

# run lime() on training set
explainer <- lime::lime(x = Xtrain, 
                        model = mod)

# run explain() on the explainer
explanation <- lime::explain(x = Xtest[1:5, ], 
                             explainer = explainer, 
                             labels = "Yes",
                             n_features = 2,
                             kernel_width = 0.5)

lime::plot_explanations(explanation)
lime::plot_features(explanation)

explanation %>% 
  select(case, feature,feature_value,feature_desc,model_prediction)

# modelplotr

scores_and_ntiles <- prepare_scores_and_ntiles(datasets=list("train_data","test_data"),
                                               models = list("m"),  
                                               model_labels = list("GBM"), 
                                               target_column="Churned30",
                                               ntiles = 100)

scores_and_ntiles <- scores_and_ntiles %>%
  rename("ntl_0" = ntl_p0,"ntl_1" = ntl_p1)

plot_input <- plotting_scope(prepared_input = scores_and_ntiles,scope="compare_datasets")

plot_cumgains(data = plot_input, highlight_ntile = 20,
              highlight_how = "text")

#Cumulative lift
plot_cumlift(data = plot_input, highlight_ntile = 20,
             highlight_how = "text")

#Response plot
plot_response(data = plot_input)

#Cumulative response plot
plot_cumresponse(data = plot_input)

plot_multiplot(data = plot_input)

plot_roi(data = plot_input,
         fixed_costs = 1000,
         variable_costs_per_unit = 10,
         profit_per_unit = 50,
         highlight_ntile = "max_roi",
         highlight_how = "text")

plot_costsrevs(data = plot_input,fixed_costs = 1000,
               variable_costs_per_unit = 10,
               profit_per_unit = 50,
               highlight_ntile = "max_roi",
               highlight_how = "text")

plot_profit(data = plot_input,fixed_costs = 1000,
            variable_costs_per_unit = 10,
            profit_per_unit = 50,
            highlight_ntile = "max_profit",
            highlight_how = "text")
