# comparing models
# "2019-11-06"
# Peer Christensen

library(tidyverse)
library(h2o)
library(modelplotr)
library(ggthemes)

h2o.init()

test_data <- read_csv("preprocessed_data/test_data.csv")

test_hf <- as.h2o(test_data)

###########################################################
# get list of models ordered according to the leaderboard

files <- file.info(dir(path = "models2", full.names = TRUE), extra_cols = FALSE)
files <- files[with(files, order(as.POSIXct(mtime))), ]
files <- rownames(files)

models <- list()

for (i in files[1:5]) {
  
  mod <- h2o.loadModel(i)
  
  models[i] = mod
}

###########################################################
# PRROC Curves

prroc_curves <- function(models, best = F, test_data, n_models =5) {
  
  # if (best == T) {
  #   models <- as.vector(as.character(H2OAutoML_object@leader@model_id)) %>%
  #     map(h2o.getModel)
  # }
  # else {
  #   models <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id))[1:n_models] %>%
  #     map(h2o.getModel)
  # }
  
  df <- tibble()
  
  for (i in 1:length(models)) {
    
    perf       <- h2o.performance(models[[i]], test_data)
    recall     <- perf@metrics$thresholds_and_metric_scores$recall
    precision  <- perf@metrics$thresholds_and_metric_scores$precision
    
    model_id  <- models[[i]]@model_id
    algorithm <- models[[i]]@algorithm
    
    d <- tibble(model_id,algorithm,recall,precision)
    d <- add_row(d,model_id = model_id, algorithm=algorithm,recall=0,precision=0,.before=T)
    d <- add_row(d,model_id = model_id, algorithm=algorithm,recall=0,precision=0,.before=F)
    d <- add_column(d, model_rank = i)
    
    df <- rbind(df,d)
  }
  
  df$model_id1 <- str_split(df$model_id, "_AutoML") %>%
    map_chr(1) %>%
    paste0(df$model_rank,": ",.)
  
  df$model_id2 <- str_split(df$model_id,"(?<=_)(?=[_model])") %>%
    map(2) %>%
    paste("_",.) %>%
    str_remove(" ")
  
  df$model_id <- paste0(df$model_id1,df$model_id2)
  df$model_id <- str_remove(df$model_id,"_NULL")

  return(df)
}

prroc <- prroc_curves(models,test_data = test_hf,n_models = 5) 

prroc %>%
  filter(precision != 1, recall > 0.1) %>%
  ggplot(aes(recall,precision,colour = reorder(model_id,model_rank))) +
  geom_line(size = 1,alpha=.8) +
  coord_fixed() +
  xlab('Recall') +
  ylab('Precision') +
  labs(colour = "Models") +
  ggtitle('PR-ROC') +
  theme_light() +
  theme(plot.title    = element_text(size = 16),
        plot.subtitle = element_text(size = 12,face="italic",vjust=-1)) +
  scale_colour_tableau()
ggsave("figures/prroc.png")
############################################################
# Get scoring metrics + rank

model_metrics_long <- function(models, best = F, test_data, n_models =5) {
  
  # if (best == T) {
  #   models <- as.vector(as.character(H2OAutoML_object@leader@model_id)) %>%
  #     map(h2o.getModel)
  # }
  # else {
  #   models <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id))[1:n_models] %>%
  #     map(h2o.getModel)
  # }
  # 
  df <- tibble()
  
  for (i in 1:length(models)) {
    
    perf <- h2o.performance(models[[i]], test_data)
    metrics <- perf@metrics$max_criteria_and_metric_scores %>%
      as_tibble() %>%
      dplyr::select(-idx) %>%
         filter(metric %in% c("max f1","max f2","max absolute_mcc")) %>%
          mutate(metric = str_remove(metric, "max ")) %>%
         add_row(metric="pr_auc",threshold="-",value=perf@metrics$pr_auc) %>%
      mutate(model_id = models[[i]]@model_id,
             rank_auc = i)
    
    df <- rbind(df,metrics)
  }
  
  model_id1 <- str_split(df$model_id, "_AutoML") %>%
    map_chr(1) %>%
    paste0(df$rank_auc,": ",.)
  
  model_id2 <- str_split(df$model_id,"(?<=_)(?=[_model])") %>%
    map(2) %>%
    paste("_",.) %>%
    str_remove(" ")
  
  df$model_id <- paste0(model_id1,model_id2)
  df$model_id <- str_remove(df$model_id,"_NULL")
  
  return(df)
}

all_metrics <- model_metrics_long(models, test_data = test_hf)

all_metrics %>%
  filter(metric == "pr_auc") %>%
  arrange(desc(value))

all_metrics %>%
  filter(metric == "f2") %>%
  arrange(desc(value))

############################################################
# confusion matrices

cm_tables <- list()
for (i in 1:length(models)) {
  
  perf <- h2o.performance(models[[i]], test_hf)
  cm <- h2o.confusionMatrix(perf,metrics = c("f2"))
  
  cm_tables[[i]] = cm
}

cm_tables

############################################################
# Model plots

# lift curve
# h2o.gainsLift(models[[1]]) %>%
#   ggplot(aes(cumulative_data_fraction,cumulative_lift)) +
#   geom_line()

model_ids <- NULL

for (i in 1:length(models)) {
  
  id1 <- str_split(models[[i]]@model_id, "_AutoML") %>%
    map_chr(1) %>%
    paste0(models[[i]]@algorithm,": ",.)

  id2 <- str_split(models[[i]]@model_id,"(?<=_)(?=[_model])") %>%
    map(2) %>%
    paste("_",.) %>%
    str_remove(" ")
  
  model_id <- paste0(id1,id2)
  model_id <- str_remove(model_id,"_NULL")
  
  model_ids = c(model_ids,model_id) 
  }

m1 = models[[1]]
m2 = models[[2]]
m3 = models[[3]]
m4 = models[[4]]
m5 = models[[5]]


scores_and_ntiles <- prepare_scores_and_ntiles(datasets=list("test_data"),
                                               dataset_labels = list("test data"),
                                               models = list("m1","m2","m3","m4","m5"),
                                               #models = list("model_glm"),
                                               model_labels = model_ids,
                                               target_column="Churned30",
                                               ntiles = 100)

scores_and_ntiles <- scores_and_ntiles %>%
  rename("ntl_0" = ntl_p0,"ntl_1" = ntl_p1)

plot_input <- plotting_scope(prepared_input = scores_and_ntiles,
                             scope="compare_models")

save_path <- "C:/Users/pech/Desktop/Projects/Churn_2.0/figures/"

#Cumulative gains
plot_cumgains(data = plot_input,
              save_fig = T,
              save_fig_filename = paste0(save_path,"cumgains_compare"))

#Cumulative lift
plot_cumlift(data = plot_input,
             save_fig = T,
             save_fig_filename = paste0(save_path,"cumlift_compare"))

#Response plot
plot_response(data = plot_input,
              save_fig = T,
              save_fig_filename = paste0(save_path,"response_compare"))

#Cumulative response plot
plot_cumresponse(data = plot_input,
              save_fig = T,
              save_fig_filename = paste0(save_path,"cumresponse_compare"))

plot_multiplot(data = plot_input, save_fig = T,
               save_fig_filename = paste0(save_path,"multiplot_compare"))

# !! Financial plots do not support model comparison, see evaluation script

# # financial plots
# plot_roi(data = plot_input,
#          fixed_costs = 1000,
#          variable_costs_per_unit = 10,
#          profit_per_unit = 50,
#          save_fig = T,
#          save_fig_filename = "C:/Users/pech/Desktop/Projects/Churn_2.0/roi_1",
#          highlight_ntile = "max_roi",
#          highlight_how = "text")
# 
# plot_costsrevs(data = plot_input2,fixed_costs = 1000,
#                variable_costs_per_unit = 10,
#                profit_per_unit = 50,
#                save_fig = T,
#                save_fig_filename = "C:/Users/pech/Desktop/Projects/Churn_2.0/cost_rev_1",
#                highlight_ntile = "max_roi",
#                highlight_how = "text")
# 
# plot_profit(data = plot_input2,fixed_costs = 1000,
#             variable_costs_per_unit = 10,
#             profit_per_unit = 50,
#             save_fig = T,
#             save_fig_filename = "C:/Users/pech/Desktop/Projects/Churn_2.0/profit_1",
#             highlight_ntile = "max_profit",
#             highlight_how = "text")
