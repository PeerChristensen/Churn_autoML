# check for missingness
# Peer Christensen

Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(RODBC)
library(mice)

channel <-odbcConnect("saxo034", uid="R", pwd="sqlR2017")

sqlquery <- "SELECT * FROM [DataMartMisc].[machinelearning].[ChurnTrain2020]"

df <- sqlQuery(channel, sqlquery) %>% 
  as_tibble()

close(channel)

md.pattern(df)

observed_churn <- md.pairs(df)$rr[2,] # rr (response-response, non-missing), row 2 is churn var

missing_churn <- md.pairs(df)$rm[2,] # rm (response-missing)

#churn == 1
df2 <- df %>% filter(Churned30==1)
missing_churn2 <- md.pairs(df2)$rm[2,] # rm (response-missing)
missing_cols <- missing_churn2[missing_churn2>0]
missing_col_names <- attributes(missing_cols)$names

library(VIM)
nhanes_aggr = aggr(df, col=mdc(1:2), 
                   numbers=TRUE, sortVars=TRUE, 
                   labels=names(df), 
                   cex.axis=.7, gap=3,
                   ylab=c("Proportion of missingness","Missingness Pattern"))