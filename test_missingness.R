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

missing_churn <- md.pairs(df)$rr[2,] # rr (response-response, non-missing), row 2 is churn var

library(VIM)
nhanes_aggr = aggr(df, col=mdc(1:2), 
                   numbers=TRUE, sortVars=TRUE, 
                   labels=names(df), 
                   cex.axis=.7, gap=3,
                   ylab=c("Proportion of missingness","Missingness Pattern"))