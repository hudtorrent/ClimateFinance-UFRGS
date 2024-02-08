library(tidyverse)
library(readr)
library(stringr)
library(writexl)
options(scipen = 999)
#### Boosting ####
load("Scripts/RW Forecast/Long/Objects/boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/YA-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/YM-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/YN-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/YS-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/YW-boosting05-env.RData")
rm(list=setdiff(ls(), c("top_vars", "statistics_mboost",
                        "YA_top_vars", "YA_statistics_mboost",
                        "YM_top_vars", "YM_statistics_mboost",
                        "YN_top_vars", "YN_statistics_mboost",
                        "YS_top_vars", "YS_statistics_mboost",
                        "YW_top_vars", "YW_statistics_mboost",
                        "stats_mboost", "vars_mboost","stats_mboost2", "vars_mboost2",
                        "stats_lm", "stats_lm2")))

stats_mboost <- rbind(statistics_mboost,
               YA_statistics_mboost,
               YM_statistics_mboost,
               YN_statistics_mboost,
               YS_statistics_mboost,
               YW_statistics_mboost)
vars_mboost <- cbind(top_vars, rep(NA,15),
              YA_top_vars, rep(NA,15),
              YM_top_vars, rep(NA,15),
              YN_top_vars, rep(NA,15),
              YS_top_vars, rep(NA,15),
              YW_top_vars)


#### Boosting 2 ####
load("Scripts/RW Forecast/Long/Objects/2boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/2YA-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/2YM-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/2YN-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/2YS-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/2YW-boosting05-env.RData")
rm(list=setdiff(ls(), c("top_vars", "statistics_mboost",
                        "YA_top_vars", "YA_statistics_mboost",
                        "YM_top_vars", "YM_statistics_mboost",
                        "YN_top_vars", "YN_statistics_mboost",
                        "YS_top_vars", "YS_statistics_mboost",
                        "YW_top_vars", "YW_statistics_mboost",
                        "stats_mboost", "vars_mboost","stats_mboost2", "vars_mboost2",
                        "stats_lm", "stats_lm2")))

stats_mboost2 <- rbind(statistics_mboost,
               YA_statistics_mboost,
               YM_statistics_mboost,
               YN_statistics_mboost,
               YS_statistics_mboost,
               YW_statistics_mboost)
vars_mboost2 <- cbind(top_vars, rep(NA,15),
              YA_top_vars, rep(NA,15),
              YM_top_vars, rep(NA,15),
              YN_top_vars, rep(NA,15),
              YS_top_vars, rep(NA,15),
              YW_top_vars)


#### Boosting Norm ####



#### Linear ####
load("Scripts/RW Forecast/Long/Objects/linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/YA-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/YM-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/YN-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/YS-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/YW-linear05-env.RData")
rm(list=setdiff(ls(), c("statistics_lm","YA_statistics_lm", "YM_statistics_lm",
                        "YN_statistics_lm","YS_statistics_lm", "YW_statistics_lm",
                        "stats_mboost", "vars_mboost","stats_mboost2", "vars_mboost2",
                        "stats_lm", "stats_lm2")))

stats_lm <- rbind(statistics_lm,
                      YA_statistics_lm,
                      YM_statistics_lm,
                      YN_statistics_lm,
                      YS_statistics_lm,
                      YW_statistics_lm)

#### Linear 2 ####
load("Scripts/RW Forecast/Long/Objects/2linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/2YA-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/2YM-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/2YN-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/2YS-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/2YW-linear05-env.RData")
rm(list=setdiff(ls(), c("statistics_lm","YA_statistics_lm", "YM_statistics_lm",
                        "YN_statistics_lm","YS_statistics_lm", "YW_statistics_lm",
                        "stats_mboost", "vars_mboost","stats_mboost2", "vars_mboost2",
                        "stats_lm", "stats_lm2")))

stats_lm2 <- rbind(statistics_lm,
                  YA_statistics_lm,
                  YM_statistics_lm,
                  YN_statistics_lm,
                  YS_statistics_lm,
                  YW_statistics_lm)

#### Linear Norm ####



#####
rm(list=setdiff(ls(),c("stats_mboost", "vars_mboost","stats_mboost2", 
                       "vars_mboost2", "stats_lm", "stats_lm2")))

stats_mboost <- stats_mboost %>% mutate(RMSE = as.numeric(RMSE), MAPE = as.numeric(MAPE))
stats_mboost2 <- stats_mboost2 %>% mutate(RMSE = as.numeric(RMSE), MAPE = as.numeric(MAPE))
stats_lm <- stats_lm %>% mutate(RMSE = as.numeric(RMSE), MAPE = as.numeric(MAPE))
stats_lm2 <- stats_lm2 %>% mutate(RMSE = as.numeric(RMSE), MAPE = as.numeric(MAPE))

#####

write_csv2(stats_mboost, file = "Output/Statistics/RW Forecast/stats_mboost.csv")
write_csv2(stats_mboost2, file = "Output/Statistics/RW Forecast/stats_mboost2.csv")
write_csv2(stats_lm, file = "Output/Statistics/RW Forecast/stats_lm.csv")
write_csv2(stats_lm2, file = "Output/Statistics/RW Forecast/stats_lm2.csv")

write_xlsx(vars_mboost, path = "Output/Statistics/RW Forecast/var_mboost.xlsx")
write_xlsx(vars_mboost2, path = "Output/Statistics/RW Forecast/var_mboost2.xlsx")