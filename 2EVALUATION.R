# performance
# "2019-11-06"
# Peer Christensen

library(tidyverse)
library(h2o)

h2o.init()

model_path <- glue::glue("models2/{list.files('models2', pattern = 'best')}")

mod <- h2o.loadModel(model_path)

perf <- h2o.performance(mod, test_hf)

h2o.confusionMatrix(perf,metrics = c("f2"))

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
