#  EDA script
# "2019-11-05"
# Peer Christensen

library(tidyverse)
library(correlationfunnel)

red   <- "#c51924"
blue  <- "#028ccc"

df <- read_csv("churn_data_training.csv") %>%
  mutate(
    IsFree = factor(IsFree),Churned30 = factor(Churned30)) %>%
  mutate_if(is.character,factor) %>%
  dplyr::select(-`FirstOrderDate within 3 months`,
                -Customer_Key)  

df$AverageOrderSize <- df$TotalNetRevenue / df$TotalOrderCount

today <- max(as.Date(df$DateStatus)) 
# today <- as.Date(now()) 

df$DateLatestSignup <- today - lubridate::days(df$DaysSinceLatestSignup)
df$DateLatestSignup_month <- lubridate::month(df$DateLatestSignup)

df <- df %>%
  dplyr::select(-DateStatus,-DateLatestSignup)

####################################################
# Correlation funnel

df_binarized <- df %>%
  binarize(n_bins = 5, thresh_infreq = 0.01, name_infreq = "OTHER", one_hot = TRUE)

df_churn_corr <- df_binarized %>%
  correlate(Churned30__1)

# all variables
df_churn_corr %>%
  plot_correlation_funnel()

# top variables
df_churn_corr %>%
  top_n(50,feature) %>%
  plot_correlation_funnel()

# top correlation with churn
df_churn_corr %>%
  arrange(desc(correlation)) %>%
  top_n(15) %>%
  plot_correlation_funnel()


####################################################
# Distributions


# Churned30 ~ numeric
num_vars <- df %>%
  select_if(is.numeric) %>%
  names()

df %>%
  select(Churned30,num_vars) %>%
  select(Churned30,starts_with("S0"),DaysSinceLatestSignup) %>%
  gather(variable, value,-Churned30) %>%
  ggplot(aes(x=value, fill = Churned30,colour=Churned30)) +
  facet_wrap(~variable,scales="free",ncol=2) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "top",
        legend.text = element_text(size=18),
        legend.title = element_text(size=18)) +
  scale_fill_manual(values=c(red,blue)) +
  scale_colour_manual(values=c(red,blue))
  
# Churned30 ~ categorical

df %>%
  select_if(is.factor) %>%
  gather(variable, value, -Churned30) %>%
  mutate(value = ifelse(is.na(value),"na",value)) %>%
  count(Churned30, variable, value) %>%
  ggplot(aes(x = reorder(value,-n), y = n, fill = Churned30, color = Churned30)) +
  facet_wrap(~ variable, ncol = 2, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.text = element_text(size=18),
        legend.title = element_text(size=18)) +
  scale_fill_manual(values=c(red,blue)) +
  scale_colour_manual(values=c(red,blue))

# Churned30 ~ categorical - proportions

df %>%
  select_if(is.factor) %>%
  gather(variable, value, -Churned30) %>%
  mutate(value = ifelse(is.na(value),"na",value)) %>%
  count(Churned30, variable, value) %>%
  group_by(variable,value) %>%
  mutate(sum= sum(n)) %>%
  mutate(Proportion = n / sum * 100) %>%
  ggplot(aes(x = reorder(value,-n), y = Proportion, fill = Churned30, color = Churned30)) +
  facet_wrap(~ variable, ncol = 2, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.text = element_text(size=18),
        legend.title = element_text(size=18)) +
  scale_fill_manual(values=c(red,blue)) +
  scale_colour_manual(values=c(red,blue))

         