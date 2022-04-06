#%%
rm(list = ls())
library(tidyverse); library(data.table); library(hBayesDM); library(correlation); library(patchwork)
theme_set(theme_minimal())
#%%

#%% r partial-pooling param recovery
dt1 <- fread("../data/simulated_data.csv")
dt2 <- dt1[, .(subjID = subj, RT = rt, choice = acc)]
dt2[, choice := choice + 1]
# dt2 <- dt2[subjID < 5]
fit <- choiceRT_ddm(dt2, niter = 5000, nwarmup = 1000, nchain = 4, ncore = 4)
write_rds(fit, "../data/simulated_data_recovery_h.rds")
#%%


#%% check model fit
fit <- read_rds("../data/simulated_data_recovery_h.rds")
#%%


#%% 
dt1 <- fread("../data/simulated_data.csv")
dt2 <- distinct(select(dt1, subj:delta_true))
dt2
params <- data.table(fit$allIndPars)
setnames(params, "subjID", "subj")
dt2 <- left_join(dt2, params) |> data.table()

cor_method <- "spearman"
p11 <- plot(cor_test(dt2, "alpha_true", "alpha", cor_method)) + geom_abline(lty = "dashed", col = 'red')
p12 <- plot(cor_test(dt2, "tau_true", "tau", cor_method)) + geom_abline(lty = "dashed", col = 'red')
p13 <- plot(cor_test(dt2, "beta_true", "beta", cor_method)) + geom_abline(lty = "dashed", col = 'red')
p14 <- plot(cor_test(dt2, "delta_true", "delta", cor_method)) + geom_abline(lty = "dashed", col = 'red')
p15 <- (p11 + p12) / (p13 + p14)
p15
ggsave("../figures/parameter_recovery_h.png", p15, dpi = 300, width = 13, height = 13, bg = 'white')

#%%



#%% simulate correlations with parameters
set.seed(1)
setDT(dt2)
cor_method <- "spearman"
dt2[, crt_alpha := 0.5 * alpha_true + rnorm(.N, 0, 2)]
p1 <- plot(cor_test(dt2, "alpha_true", "crt_alpha", cor_method))
p2 <- plot(cor_test(dt2, "alpha", "crt_alpha", cor_method))
p101 <- p1 + p2
p101

dt2[, crt_tau := 0.5 * tau_true + rnorm(.N, 0, 2)]
p1 <- plot(cor_test(dt2, "tau_true", "crt_tau", cor_method))
p2 <- plot(cor_test(dt2, "tau", "crt_tau", cor_method))
p102 <- p1 + p2
p102

dt2[, crt_beta := 5 * beta_true + rnorm(.N, 0, 2)]
p1 <- plot(cor_test(dt2, "beta_true", "crt_beta", cor_method))
p2 <- plot(cor_test(dt2, "beta", "crt_beta", cor_method))
p103 <- p1 + p2
p103

dt2[, crt_delta := 5 * delta_true + rnorm(.N, 0, 2)]
p1 <- plot(cor_test(dt2, "delta_true", "crt_delta", cor_method))
p2 <- plot(cor_test(dt2, "delta", "crt_delta", cor_method))
p104 <- p1 + p2
p104

p105 <- p101 / p102 / p103 / p104
p105
ggsave("../figures/parameter_recovery_h_simulate-crt.png", p105, dpi = 300, width = 13, height = 13, bg = 'white')
#%%
