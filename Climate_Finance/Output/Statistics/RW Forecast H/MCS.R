source("Scripts/functions.R")
#### Long ####

load("Scripts/RW Forecast H/Long/Objects/norm-boosting-h12-All.RData")
#load("Scripts/RW Forecast/Long/Objects/norm-boosting05-env.RData")
View(statistics_mboost)
results <- tibble(date = my(date), Y = test_Y, m1_fc, m2_fc, m3_fc, m4_fc, m5_fc, m6_fc)
rm(list=setdiff(ls(), c("results")))

fcs <- results %>% select(-Y, -date) %>% as.matrix()
y <- results %>% select(Y) %>% as_vector()

loss <- fcs * NA
for (i in seq_len(ncol(fcs))) {
  loss[, i] <- LossLevel(realized = y, evaluated = fcs[, i], which = "AE")
}

mcs_test <- MCSprocedure(loss, alpha = 0.1, statistic = "Tmax")

#### Short ####
load("Scripts/RW Forecast H/Short/Objects/YW-norm-boosting-h1-Dependent.RData")

View(YW_statistics_mboost)
results <- spread(YW_results, "model", "fc")
rm(list=setdiff(ls(), c("results")))

fcs <- results[,-(1:2)] %>% as.matrix()
y <- results[,2] %>% as_vector()

loss <- fcs * NA
for (i in seq_len(ncol(fcs))) {
  loss[, i] <- LossLevel(realized = y, evaluated = fcs[, i], which = "AE")
}

mcs_test <- MCSprocedure(loss, alpha = 0.1, statistic = "Tmax")
