#  EDA script
# "2019-11-05"
# Peer Christensen

Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(correlationfunnel)
library(caret)
library(VGAM)
library(RODBC)

red   <- "#c51924"
blue  <- "#028ccc"

channel <-odbcConnect("saxo034", uid="R", pwd="sqlR2017")

sqlquery <- "SELECT * FROM [DataMartMisc].[machinelearning].[ChurnTrain2020]"

df <- sqlQuery(channel, sqlquery) %>% 
  as_tibble() %>%
  mutate(
    Perm_anyperm    = factor(Perm_anyperm),
    Churned30       = factor(Churned30),
    Perm_recommendations = factor(Perm_recommendations),
    Perm_newsletter = factor(Perm_newsletter),
    MatasUser       = factor(MatasUser),
    CoopUser        = factor(CoopUser)) %>%
  mutate_if(is.character,factor) %>%
  mutate_if(is.integer,as.numeric) %>%
  select(-Customer_Key) %>%
  drop_na()

df <- df %>% select(-missing_col_names)

#class balance
prop.table(table(df$Churned30))


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
  select(starts_with("S0")) %>%
  names()

f_lit_vars <- df %>% 
  select(starts_with("F0")) %>%
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

#remove original f-s variables

df <- df %>%
  select(-f_lit_vars,-s_lit_vars)

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
  facet_wrap(~ variable, ncol = 3, scales = "free") +
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
  facet_wrap(~ variable, ncol = 3, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.text = element_text(size=18),
        legend.title = element_text(size=18)) +
  scale_fill_manual(values=c(red,blue)) +
  scale_colour_manual(values=c(red,blue))
ggsave("figures/eda_categorical_proportions.png", height=10, width = 7)
