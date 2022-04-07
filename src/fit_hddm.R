#%%
rm(list = ls())
library(tidyverse); library(data.table); library(hBayesDM); library(parallel)
theme_set(theme_minimal())
#%%

cores <- 1  # unused parameter for now
# f <- 0  # which fold 
dt1 <- fread("../data/clean/data_fold01.csv")
n_countries <- dt1[, n_distinct(country)]
dt1[, unique(country)]
# dt1 <- dt1[fold == f]

fit_model <- function(x) {
    # prep data
    temp_dt <- dt1[country == x]
    temp_dt[, n_distinct(id)]
    temp_dt <- temp_dt[, .(subjID = id, RT = rt, choice = acc)]
    temp_dt[, choice := choice + 1]
    
    # fit model
    fit <- choiceRT_ddm(temp_dt, niter = 5000, nwarmup = 1000, nchain = 4, ncore = 4)
    
    # save model
    output <- paste0("../results/fold0_hddm_m0_country", sprintf("%02d", x), ".rds")
    message(paste0("Saving model to ", output))
    write_rds(fit, output)
}


# results <- mclapply(1:n_countries, fit_model, mc.cores = cores)
for (c in 3:n_countries) {
    print(paste0("fitting model to country ", c))
    fit_model(c)
}




