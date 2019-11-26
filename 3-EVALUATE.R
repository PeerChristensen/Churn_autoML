# performance
# "2019-11-06"
# Peer Christensen

Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(h2o)
library(modelplotr)
library(lime)
library(data.tree)

red   <- "#c51924"
blue  <- "#028ccc"
green <- "#16b84c"

h2o.init()

####################################################
# LOAD MODEL AND TEST SET

model_path <- glue::glue("models10/{list.files('models10', pattern = 'best')}")

mod <- h2o.loadModel(model_path)

train_data  <- read_csv("preprocessed_data/train_data_new.csv") %>%
  mutate(Churned30 = factor(Churned30))

test_data  <- read_csv("preprocessed_data/test_data_new.csv")  %>%
  mutate(Churned30 = factor(Churned30))  

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
perf

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
ggsave("figures/all_metrics.png")

# f2, tpr, tnr
intersect_threshold <- metrics %>% filter(round(tpr,2) == round(tnr,2)) %>%
  pull(threshold) %>%
  first()

intersect_max <- metrics %>% filter(round(tpr,2) == round(tnr,2)) %>% 
  pull(tpr) %>%
  first()
  
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
ggsave("figures/max_f2_tpr_tnr.png")

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


# if stackedensemble, we need to unpack models to get variable importance

varImp_ggplot <- function(H2OAutoML_object, save_pngs = F, return_data = F, n_vars = 25) {
    
    if (mod@algorithm == "stackedensemble") {
      print("Ensemble model: Plotting Model importance and Variable importances of model with highest importance")
      metaLearner <- h2o.getModel(mod@model$metalearner$name)
      
      # plot model importance using ggplot2
      metaLearner_df <- metaLearner@model$coefficients_table[-1,] %>%
        arrange(desc(standardized_coefficients)) %>%
        mutate(order = row_number()) %>%
        filter(coefficients > 0.000)
      
      metaLearner_df$names1 <- str_split(metaLearner_df$names, "_AutoML") %>%
        map_chr(1) %>%
        paste0(metaLearner_df$order,": ",.)
      
      metaLearner_df$names2 <- str_split(metaLearner_df$names,"(?<=_)(?=[_model])") %>%
        map(2) %>%
        paste("_",.) %>%
        str_remove(" ")
      
      metaLearner_df$names <- paste0(metaLearner_df$names1,metaLearner_df$names2)
      metaLearner_df$names <- str_remove(metaLearner_df$names,"_NULL")
      
      p1 <- metaLearner_df %>%
        ggplot(aes(x=reorder(names,rev(order)),standardized_coefficients, fill = rev(order))) +
        geom_col() +
        coord_flip() +
        labs(x= "Models", y = "Standard. coefficients") +
        ggtitle("Model importance in ensemble") +
        scale_fill_continuous_tableau() +
        theme_minimal() +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12),
              axis.text  = element_text(size = 12),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = "none")
      
      print(p1)
      
      if (save_pngs == T) {
        ggsave("figures/modelImp.png",height=8,width=8)
      }
      
      # VarImp of most important model
      modelImp <- h2o.varimp(metaLearner) # data frame
      
      highestImpName <- modelImp[1,1]
      
      model  <- h2o.getModel(as.character(highestImpName))
      varImp <- h2o.varimp(model)
    } else {
      varImp <- h2o.varimp(model)
    }
    
    if (model@algorithm == "glm") {
      
      p2 <- varImp %>%
        drop_na() %>%
        top_n(n_vars,variable) %>%
        ggplot(aes(x=reorder(variable,relative_importance),relative_importance,fill=log(relative_importance))) +
        geom_col() +
        coord_flip() +
        labs(x= "Variables", y = "Coefficients") +
        ggtitle(paste("Variable importance for", model@algorithm, "model")) +
        scale_fill_continuous_tableau() +
        thene_minimal() +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12),
              axis.text  = element_text(size = 12),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_fill_continuous_tableau()
      
    } else {
      
      p2 <- varImp %>%
        drop_na() %>%
        top_n(n_vars,scaled_importance) %>%
        ggplot(aes(x=reorder(variable,scaled_importance ),scaled_importance, fill = log(scaled_importance))) + #fill = factor(sign)
        geom_col() +
        coord_flip() +
        labs(x= "Variables", y = "Coefficients") +
        ggtitle(paste("Variable importance for", model@algorithm, "model")) +
        scale_fill_continuous_tableau() +
        theme_light() +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12),
              axis.text  = element_text(size = 12),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = "none") 
    
    }
    print(p2)
    
    if (save_pngs == T) {
      ggsave("figures/varImp_best_meta.png",height=8,width=8)
    }
    
    if (return_data == T) {
      return(list(modelImp,varImp))
    }
  }

if (mod@algorithm == "stackedensmeble") {
  
  varImp_ggplot(mod,save_pngs = T)
}

h2o.varimp(mod)
h2o.varimp_plot(mod, num_of_features = 15)

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
ggsave("figures/lime_explanations_all.png",width=10,height=7)

lime::plot_features(explanation)
ggsave("figures/lime_expl_features_all.png",width=10,height=7)

case_numbers <- tibble(Customer_Key,case = as.character(1:length(Customer_Key)))

explanation %>% 
  dplyr::select(case, feature,feature_value,feature_desc,model_prediction) %>%
  left_join(case_numbers) %>%
  dplyr::select(Customer_Key,everything())

####################################################
# Decision Tree

modH2oTree = h2o.getModelTree(model = mod, tree_number = 1)

createDataTree <- function(h2oTree) {
  h2oTreeRoot = h2oTree@root_node
  dataTree = Node$new(h2oTreeRoot@split_feature)
  dataTree$type = 'split'
  addChildren(dataTree, h2oTreeRoot)
  return(dataTree)
}

addChildren <- function(dtree, node) {
  
  if(class(node)[1] != 'H2OSplitNode') return(TRUE)
  
  feature = node@split_feature
  id = node@id
  na_direction = node@na_direction
  
  if(is.na(node@threshold)) {
    leftEdgeLabel = printValues(node@left_levels, 
                                na_direction=='LEFT', 4)
    rightEdgeLabel = printValues(node@right_levels, 
                                 na_direction=='RIGHT', 4)
  }else {
    leftEdgeLabel = paste("<", node@threshold, 
                          ifelse(na_direction=='LEFT',',NA',''))
    rightEdgeLabel = paste(">=", node@threshold, 
                           ifelse(na_direction=='RIGHT',',NA',''))
  }
  
  left_node = node@left_child
  right_node = node@right_child
  
  if(class(left_node)[[1]] == 'H2OLeafNode')
    leftLabel = paste("prediction:", left_node@prediction)
  else
    leftLabel = left_node@split_feature
  
  if(class(right_node)[[1]] == 'H2OLeafNode')
    rightLabel = paste("prediction:", right_node@prediction)
  else
    rightLabel = right_node@split_feature
  
  if(leftLabel == rightLabel) {
    leftLabel = paste(leftLabel, "(L)")
    rightLabel = paste(rightLabel, "(R)")
  }
  
  dtreeLeft = dtree$AddChild(leftLabel)
  dtreeLeft$edgeLabel = leftEdgeLabel
  dtreeLeft$type = ifelse(class(left_node)[1] == 'H2OSplitNode', 'split', 'leaf')
  
  dtreeRight = dtree$AddChild(rightLabel)
  dtreeRight$edgeLabel = rightEdgeLabel
  dtreeRight$type = ifelse(class(right_node)[1] == 'H2OSplitNode', 'split', 'leaf')
  
  addChildren(dtreeLeft, left_node)
  addChildren(dtreeRight, right_node)
  
  return(FALSE)
}

printValues <- function(values, is_na_direction, n=4) {
  l = length(values)
  if(l == 0)
    value_string = ifelse(is_na_direction, "NA", "")
  else
    value_string = paste0(paste0(values[1:min(n,l)], collapse = ', '),
                          ifelse(l > n, ",...", ""),
                          ifelse(is_na_direction, ", NA", ""))
  return(value_string)
}

modDataTree = createDataTree(modH2oTree)

GetEdgeLabel <- function(node) {return (node$edgeLabel)}
GetNodeShape <- function(node) {switch(node$type, 
                                       split = "diamond", leaf = "oval")}
GetFontName <- function(node) {switch(node$type, 
                                      split = 'Palatino-bold', 
                                      leaf = 'Palatino')}
SetEdgeStyle(modDataTree, fontname = 'Palatino-italic', 
             label = GetEdgeLabel, labelfloat = TRUE,
             fontsize = "26", fontcolor='royalblue4')
SetNodeStyle(modDataTree, fontname = GetFontName, shape = GetNodeShape, 
             fontsize = "26", fontcolor='royalblue4',
             height="0.75", width="1")

SetGraphStyle(modDataTree, rankdir = "LR", dpi=70.)

plot(modDataTree, output = "graph")
ggsave("figures/model_tree.png",width=12,height=9)

####################################################
# Performance plots

scores_and_ntiles <- prepare_scores_and_ntiles(datasets=list("test_data"),
                                               models = list("mod"),  
                                               model_labels = list("GBM"), 
                                               target_column="Churned30",
                                               ntiles = 100)

scores_and_ntiles <- scores_and_ntiles %>%
  rename("ntl_0" = ntl_p0,"ntl_1" = ntl_p1)

plot_input <- plotting_scope(prepared_input = scores_and_ntiles,scope="compare_datasets")

save_path <- "C:/Users/pech/Desktop/Projects/Churn_2.0/figures/"

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
         fixed_costs = 0,
         variable_costs_per_unit = 0,
         profit_per_unit = 40,
         highlight_ntile = "max_roi",
         highlight_how = "text",
         save_fig = T,
         save_fig_filename = paste0(save_path,"roi"))

# Cost-revenue
plot_costsrevs(data = plot_input,fixed_costs = 0,
               variable_costs_per_unit = 0,
               profit_per_unit = 40,
               highlight_ntile = "max_roi",
               highlight_how = "text",
               save_fig = T,
               save_fig_filename = paste0(save_path,"cost_revenue"))

# Profit
plot_profit(data = plot_input,fixed_costs = 0,
            variable_costs_per_unit = 0,
            profit_per_unit = 40,
            highlight_ntile = "max_profit",
            highlight_how = "text",
            save_fig = T,
            save_fig_filename = paste0(save_path,"profit"))
