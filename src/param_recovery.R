#%%
rm(list = ls())
library(tidyverse); library(data.table); library(RWiener); library(numDeriv); library(patchwork); library(correlation)
theme_set(theme_minimal())
#%%

#%% r simulate data
# observed data mean values
# f1 <- fread("../data/clean/data_fold01.csv")
# f1[, mean(rt), id][, mean(V1)]  # 8.15s
# f1[, mean(acc), id][, mean(V1)]  # 67% correct
# f1[, mean(rt), id][, hist(V1)] 
# f1[, mean(acc), id][, hist(V1)] 

set.seed(1)
subj <- 1000  # max loops (but only taking 400)
max_subj <- 400
n <- 18
alpha <- abs(rnorm(subj, 4.0, 2.0))  # boundary > 0
hist(alpha)
ndt <- abs(rnorm(subj, 4, 2.5))   # tau  > 0
ndt[ndt < 0.5] <- 0.5
hist(ndt)
bias <- rnorm(subj, 0.5, 0.25)  # beta  (0 to 1)
bias[bias < 0] <- 0
bias[bias > 1] <- 1
hist(bias)
drift <- rnorm(subj, 0.15, 0.07) # delta  
hist(drift)
i <- 1
j <- 0
sims <- list()
while (TRUE) {
    i <- i + 1
    dat <- rwiener(n=n, alpha=alpha[i], tau=ndt[i], beta=bias[i], delta=drift[i])
    if (length(unique(dat$q)) != n) {
       next 
    }
    j <- j + 1
    print(j)
    dat$subj <- j
    dat$alpha_true <- alpha[i]
    dat$tau_true <- ndt[i]
    dat$beta_true <- bias[i]
    dat$delta_true <- drift[i]
    sims[[j]] <- dat
    if (length(sims) == max_subj) {
        break
    }
}
simdat <- data.table(bind_rows(sims))
setnames(simdat, "q", "rt")
simdat[, acc := ifelse(resp == "upper", 1, 0)]
# ensure simulated behavior resembles observed data
avg <- simdat[, .(rt = mean(rt), acc = mean(acc), n = .N), subj]
avg
avg[, mean(rt)]
avg[, mean(acc)]
avg[, hist(rt)]
avg[, hist(acc)]
simdat_avg <- distinct(simdat[, subj:delta_true])
simdat_avg[, hist(alpha_true)]
simdat_avg[, hist(tau_true)]
simdat_avg[, hist(beta_true)]
simdat_avg[, hist(delta_true)]


# initialize variable to prepare for parameter recovery
simdat$alpha <- 0.0
fwrite(simdat, "../data/simulated_data_norm.csv")
#%%

#%% r no-pooling param recovery}
dt1 <- fread("../data/simulated_data_norm.csv")
dt1[, alpha := as.numeric(alpha)]
avg <- dt1[, .(rt = mean(rt), acc = mean(acc), n = .N), subj]
avg[, mean(rt)]
avg[, mean(acc)]
avg[, hist(rt)]
avg[, hist(acc)]
mus <- select(dt1[, summarize_all(.SD, mean)], alpha_true:delta_true)

subjs <- dt1[, unique(subj)]
i <- 8
for (i in subjs) {
    if (sum(dt1[subj == i, alpha]) != 0) {
        next
    }
    print(paste0("subj ", i))
    dat <- dt1[subj == i, .(q = rt, resp)]
    val <- 1e6 
    for (j in 1:5) {
        mus <- dt1[subj == i, alpha_true:delta_true][1, ]
        startval <- as.matrix(mus)[1, ]
        fit <- optim(startval, wiener_deviance, dat = dat, method = "Nelder-Mead", hessian=F)
        if (fit$val < val) {
          # save values
          val <- fit$val
          params <- fit$par
          dt1[subj == i, alpha := params[1]]
          dt1[subj == i, tau := params[2]]
          dt1[subj == i, beta := params[3]]
          dt1[subj == i, delta := params[4]]
          dt1[subj == i, loss := val]

          # hessian and se          
          # hes <- hessian(func = wiener_deviance, x = fit$par, data = dat)
          # hes <- fit$hessian
          # se <- sqrt(diag(solve(hes)))
          # dt1[subj == i, alpha_se := se[1]]
          # dt1[subj == i, tau_se := se[2]]
          # dt1[subj == i, beta_se := se[3]]
          # dt1[subj == i, delta_se := se[4]]
          # dt1
        }
    }
    fwrite(dt1, "../data/simulated_data_norm.csv")
}
print("finish optim")
#%%



#%% check recovery
dt1 <- fread("../data/simulated_data_norm.csv")
dt2 <- distinct(select(dt1, subj, alpha_true:delta_true, alpha:delta))
dt2

cor_method <- "spearman"
p11 <- plot(cor_test(dt2, "alpha_true", "alpha", cor_method)) + geom_abline(lty = "dashed", col = 'red')
p12 <- plot(cor_test(dt2, "tau_true", "tau", cor_method)) + geom_abline(lty = "dashed", col = 'red')
p13 <- plot(cor_test(dt2, "beta_true", "beta", cor_method)) + geom_abline(lty = "dashed", col = 'red')
p14 <- plot(cor_test(dt2[delta %between% c(-0.5, 0.5)], "delta_true", "delta", cor_method)) + geom_abline(lty = "dashed", col = 'red')
p15 <- (p11 + p12) / (p13 + p14)
p15

ggsave("../figures/parameter_recovery_normal.png", p15, dpi = 300, width = 13, height = 13, bg = 'white')

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
p1 <- plot(cor_test(dt2[delta %between% c(-0.5, 0.5)], "delta_true", "crt_delta", cor_method))
p2 <- plot(cor_test(dt2[delta %between% c(-0.5, 0.5)], "delta", "crt_delta", cor_method))
p104 <- p1 + p2
p104

p105 <- p101 / p102 / p103 / p104
p105
ggsave("../figures/parameter_recovery_normal_simulate-crt.png", p105, dpi = 300, width = 13, height = 13, bg = 'white')
#%%
