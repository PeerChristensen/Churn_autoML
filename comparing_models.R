# precision - recall curves for best models

prroc_curves <- function(H2OAutoML_object, best = F, test_data, n_models =5) {
  
  if (best == T) {
    models <- as.vector(as.character(H2OAutoML_object@leader@model_id)) %>%
      map(h2o.getModel)
  }
  else {
    models <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id))[1:n_models] %>%
      map(h2o.getModel)
  }
  
  df <- tibble()
  
  for (i in 1:length(models)) {
    
    perf <- h2o.performance(models[[i]], test_data)
    recall  <- perf@metrics$thresholds_and_metric_scores$recall
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

prroc <- prroc_curves(aml,test_data = train_hf)

prroc %>%
  ggplot(aes(recall,precision,colour = reorder(model_id,model_rank))) +
  geom_line(size = 1,alpha=.8) +
  coord_fixed() +
  xlab('Recall') +
  ylab('Precision') +
  ggtitle('PRROC curves',
          subtitle = "Comparison of the best models") +
  theme_light() +
  theme(plot.title    = element_text(size = 16),
        plot.subtitle = element_text(size = 12,face="italic",vjust=-1)) +
  scale_colour_viridis_d("Models")


# metrics for each model + rank

model_metrics_long <- function(H2OAutoML_object, best = F, test_data, n_models =5) {
  
  if (best == T) {
    models <- as.vector(as.character(H2OAutoML_object@leader@model_id)) %>%
      map(h2o.getModel)
  }
  else {
    models <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id))[1:n_models] %>%
      map(h2o.getModel)
  }
  
  df <- tibble()
  
  for (i in 1:length(models)) {
    
    perf <- h2o.performance(models[[i]], test_data)
    metrics <- perf@metrics$max_criteria_and_metric_scores %>%
      as_tibble() %>%
      dplyr::select(-idx) %>%
         filter(metric %in% c("max f1","max f2","max absolute_mcc")) %>%
          mutate(metric = str_remove(metric, "max ")) %>%
         add_row(metric="pr_auc",threshold="-",value=p@metrics$pr_auc) %>%
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

all_metrics <- model_metrics_long(aml, test_data =test_hf)

all_metrics %>%
  filter(metric == "f2")


# confusion matrix
p@metrics$cm$table


# lift curve
a %>%
  ggplot(aes(cumulative_data_fraction,cumulative_lift)) +
  geom_line()
