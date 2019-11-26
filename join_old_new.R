# join new and old data

old <- read_csv("churn_data_training.csv") %>%
  mutate(
  IsFree = factor(IsFree),Churned30 = factor(Churned30)) %>%
  mutate_if(is.character,factor)

old_vars <- names(old)

new <- read_csv2(
  "new_churn_train.csv",
  col_types = cols(
    
    Churned30             = col_factor(),
    # TotalNetRevenue        = col_double(),
    #  PlusOrderCount        = col_double(),
    #  PlusNetRevenue        = col_double(),
    #  PersonalSavingsTotal  = col_double(),
    #  PersonalSavings30days = col_double(),
    #  PlusDigitalShare      = col_double(),
    # StreamingCost         = col_double(),
    IsFree                = col_factor(),
    Perm_recommendations  = col_factor(),
    Perm_newsletter       = col_factor(),
    Perm_anyperm          = col_factor(),
    PremiumEbookRatio     = col_double())) %>%
  mutate_if(is.character,factor) 
  
new_vars <- names(new)

new_vars_select <- setdiff(new_vars,old_vars)

new <- new %>% 
  select(Customer_Key,new_vars_select) %>%
  select(-CLTVtodateProfit, -SubscriptionPrice)

df <- old %>%
  left_join(new) %>%
  dplyr::select(-`FirstOrderDate within 3 months`)  

df$AverageOrderSize <- df$TotalNetRevenue / df$TotalOrderCount

today <- max(as.Date(df$DateStatus)) 
# today <- as.Date(now()) 

df$DateLatestSignup <- today - lubridate::days(df$DaysSinceLatestSignup)
df$DateLatestSignup_month <- lubridate::month(df$DateLatestSignup)

df <- df %>%
  dplyr::select(-DateStatus,-DateLatestSignup)

write_csv(df,"new_data_clean1.csv")

