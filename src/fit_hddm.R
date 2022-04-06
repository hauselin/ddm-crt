#%%
rm(list = ls())
library(tidyverse); library(data.table); library(hBayesDM); 
theme_set(theme_minimal())
#%%


dt1 <- fread("../data/clean/data_fold01.csv")
dt1[, unique(country)]


c <- 1
temp_dt <- dt1[country == c]
temp_dt[, n_distinct(id)]
