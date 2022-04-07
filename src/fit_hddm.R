#%%
rm(list = ls())
library(tidyverse); library(data.table); library(hBayesDM); 
theme_set(theme_minimal())
#%%

dt1 <- fread("../data/clean/data_fold01.csv")
dt1[, unique(country)]

fit_model <- function(x) {
    # prep data
    temp_dt <- dt1[country == x]
    temp_dt[, n_distinct(id)]
    temp_dt <- temp_dt[, .(subjID = id, RT = rt, choice = acc)]
    temp_dt[, choice := choice + 1]
    
    # fit model
    # fit <- choiceRT_ddm(temp_dt, niter = 20, nwarmup = 10, nchain = 4, ncore = 4)
    fit <- choiceRT_ddm(temp_dt, niter = 5000, nwarmup = 1000, nchain = 4, ncore = 4)
    
    # save model
    output <- paste0("../results/hddm_m0_country", sprintf("%02d", x), ".rds")
    message(paste0("Saving model to ", output))
    write_rds(fit, output)
}

fit_model(2)

