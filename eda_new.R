#  EDA script
# "2019-11-05"
# Peer Christensen

Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(correlationfunnel)
library(caret)
library(VGAM)#

red   <- "#c51924"
blue  <- "#028ccc"

df <- read_csv("new_churn_training.csv") %>%
  mutate(Perm_anyperm = factor(Perm_anyperm),
         Churned30 = factor(Churned30),
       Perm_recommendations = factor(Perm_recommendations),
       Perm_newsletter = factor(Perm_newsletter),
       MatasUser = factor(MatasUser),
       CoopUser = factor(CoopUser)) %>%
  mutate_if(is.character,factor) %>%
  select(-Customer_Key, -IsFree,-PremiumEbookRatio) %>%
  drop_na() %>%
  filter(DaysSinceLatestSignup > 30) 

#class balance
prop.table(table(df$Churned30))

# add F_S ratio

f_lit_mean <- df %>% 
  select(starts_with("F0")) %>%
  transmute(m = rowMeans(.)) %>%
  pull(m)

s_lit_mean <- df %>% 
  select(starts_with("S0")) %>%
  transmute(m = rowMeans(.))  %>%
  pull(m)

f_ratio <- f_lit_mean / (s_lit_mean +f_lit_mean)

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
ggsave("figures/new_correlation_funnel_full.png",height=10, width = 7)

# top variables
df_churn_corr %>%
  top_n(50,feature) %>%
  plot_correlation_funnel()
ggsave("figures/correlation_funnel_top.png",height=7, width = 7)

# top correlation with churn
df_churn_corr %>%
  arrange(desc(correlation)) %>%
  top_n(15) %>%
  plot_correlation_funnel()
ggsave("figures/correlation_funnel_churn.png",height=7, width = 7)


####################################################
# Distributions

#choosing the best predictors for exploration
# top correlation with churn
top_features <- df_churn_corr %>%
  top_n(15) %>%
  filter(feature != "Churned30") %>%
  pull(feature) %>% 
  as.vector()

numeric_features <- df %>%
  select(top_features) %>%
  select_if(is.numeric) %>% 
  names()

categorical_features <- df %>%
  select(top_features) %>%
  select_if(is.factor) %>% 
  names()

df %>%
  select(Churned30,numeric_features) %>%
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
ggsave("figures/eda_numeric.png")

# Churned30 ~ numeric - transformed

df %>%
  select(Churned30,numeric_features) %>%
  mutate_if(is.numeric, yeo.johnson,lambda=.9) %>%
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
ggsave("figures/eda_numeric_transformed.png")

# Churned30 ~ categorical

df %>%
  select(Churned30, categorical_features) %>%
  gather(variable, value, -Churned30) %>%
  mutate(value = ifelse(is.na(value),"na",value)) %>%
  count(Churned30, variable, value) %>%
  ggplot(aes(x = reorder(value,-n), y = n, fill = Churned30, color = Churned30)) +
  facet_wrap(~ variable, ncol = 4, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.text = element_text(size=18),
        legend.title = element_text(size=18)) +
  scale_fill_manual(values=c(red,blue)) +
  scale_colour_manual(values=c(red,blue))
ggsave("figures/eda_categorical.png", height=10, width = 7)

# Churned30 ~ categorical - proportions

df %>%
  select(Churned30, categorical_features) %>%
  gather(variable, value, -Churned30) %>%
  mutate(value = ifelse(is.na(value),"na",value)) %>%
  count(Churned30, variable, value) %>%
  group_by(variable,value) %>%
  mutate(sum= sum(n)) %>%
  mutate(Proportion = n / sum * 100) %>%
  ggplot(aes(x = reorder(value,-n), y = Proportion, fill = Churned30, color = Churned30)) +
  facet_wrap(~ variable, ncol = 5, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.text = element_text(size=18),
        legend.title = element_text(size=18)) +
  scale_fill_manual(values=c(red,blue)) +
  scale_colour_manual(values=c(red,blue))
ggsave("figures/eda_categorical_proportions.png", height=10, width = 7)
