#### Results ####
load("Scripts/RW Forecast H/Short/Results/norm-Dependent.RData")
load("Scripts/RW Forecast H/Short/Results/norm-All.RData")

#### Boosting ####
source("Scripts/functions.R")

type <- c("Dependent", "All")
Hs <- c(3,6,12)
sets <- c("YA","YM","YN","YS","YW")
stats <- list()
vars <- list()
stats_boosting <- list()
vars_boosting <- list()

for(s in 1:5){
set <- sets[s]
for (p in 1:3){
h <- Hs[p]

print(paste0("Boosting Model: ", set, "-H", h))

m1_data <- cbind(read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/",set,".csv")),
                 read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/",set,"_lag.csv")))
m2_data <- cbind(m1_data, read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/m2_vars.csv")))
m3_data <- cbind(m1_data, read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/m3_vars.csv")))
m4_data <- cbind(m1_data, read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/m4_vars.csv")))
m5_data <- cbind(m1_data, read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/m5_vars.csv")))
m6_data <- cbind(m1_data, read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/m6_vars.csv")))
m6_data[,373:492] <- log(m6_data[,373:492])

m1_fc <- rwf_mboost2(m1_data,0.5, norm = T)
m1_vars <- vars_freq(variables)
m1_vars_avg <- variables_avg

m2_fc <- rwf_mboost2(m2_data,0.5, norm = T)
m2_vars <- vars_freq(variables)
m2_vars_avg <- variables_avg

m3_fc <- rwf_mboost2(m3_data,0.5, norm = T)
m3_vars <- vars_freq(variables)
m3_vars_avg <- variables_avg

m4_fc <- rwf_mboost2(m4_data,0.5, norm = T)
m4_vars <- vars_freq(variables)
m4_vars_avg <- variables_avg

m5_fc <- rwf_mboost2(m5_data,0.5, norm = T)
m5_vars <- vars_freq(variables)
m5_vars_avg <- variables_avg

m6_fc <- rwf_mboost2(m6_data,0.5, norm = T)
m6_vars <- vars_freq(variables)
m6_vars_avg <- variables_avg

data <- read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/short_expanded.csv")) %>% select(date)
date <- ym(str_replace(data$date, "M", ""))
date <- date[-idx]

results <- tibble(date, Y = test_Y, m1_fc, m2_fc, m3_fc, m4_fc, m5_fc, m6_fc)
results <- results %>% gather(m1_fc, m2_fc, m3_fc, m4_fc, m5_fc, m6_fc,
                              key = "model", value = "fc")

models <- c("m1_fc", "m2_fc", "m3_fc", "m4_fc", "m5_fc", "m6_fc")
plotnames <- paste0(set,c(" - Model 1 - mboost", " - Model 2 - mboost",
                          " - Model 3 - mboost", " - Model 4 - mboost",
                          " - Model 5 - mboost", " - Model 6 - mboost"))

gw <- c(NA)
gw2 <- c(NA)
for (i in 2:6) {
  m1 <- results %>% filter(model == models[1])
  mn <- results %>% filter(model == models[i])
  gw[i] <- gw.test(m1$fc, mn$fc, length(test_Y), nrow(m1_data), 1, alternative = "two.sided")$p.value
  gw2[i] <- gw.test(m1$fc, mn$fc, length(test_Y), nrow(m1_data), 1, alternative = "two.sided", power = 1)$p.value
}

statistics_mboost <- data.frame(Model = NA, RMSE = NA, MAE = NA)
MediaVariaveis = c(m1_vars_avg, m2_vars_avg, m3_vars_avg,
                   m4_vars_avg, m5_vars_avg, m6_vars_avg)
for (i in 1:6) {
  temp <- filter(results, model == models[i])
  rmse <- sqrt(sum((temp$fc - temp$Y)^2)/nrow(temp))
  mae <- (1/nrow(temp))*sum(abs((temp$Y - temp$fc)))
  statistics_mboost[i,] <- c(plotnames[i],rmse, mae)
}
statistics_mboost <- cbind(statistics_mboost, MediaVariaveis,
                              "GW Test MSE" = gw, "GW Test MAE" = gw2) %>%
  mutate(RMSE = as.numeric(RMSE), MAE = as.numeric(MAE))

top_vars <- data.frame(
  top_n(m2_vars, 15)[1:15,1],
  top_n(m2_vars, 15)[1:15,3],
  top_n(m3_vars, 15)[1:15,1],
  top_n(m3_vars, 15)[1:15,3],
  top_n(m4_vars, 15)[1:15,1],
  top_n(m4_vars, 15)[1:15,3],
  top_n(m5_vars, 15)[1:15,1],
  top_n(m5_vars, 15)[1:15,3],
  top_n(m6_vars, 15)[1:15,1],
  top_n(m6_vars, 15)[1:15,3]
)
colnames(top_vars) <- c("Model 2", "Relative 2", "Model 3", "Relative 3", "Model 4", "Relative 4",
                           "Model 5","Relative 5","Model 6","Relative 6")

save.image(file = paste0(
  "Scripts/RW Forecast H/Short/Objects/",set,"-norm-boosting-h",h,"-",type[1],".RData"))

stats[[p]] <- statistics_mboost
vars[[p]] <- top_vars
beep(2)
}
names(stats) <- c("h3", "h6", "h12")
names(vars) <- c("h3", "h6", "h12")

stats_boosting[[s]] <- stats
vars_boosting[[s]] <- vars
#save(list(stats,vars), file = paste0("Scripts/RW Forecast H/Short/Results/",set,"-norm-boosting-",type[1],".RData"))
}
beep(11)
names(stats_boosting) <- c("YA","YM","YN","YS","YW")
names(vars_boosting) <- c("YA","YM","YN","YS","YW")

rm(list=setdiff(ls(), c("stats_boosting", "vars_boosting", "type")))
save.image(file = paste0("Scripts/RW Forecast H/Short/Results/norm-boosting-",type[1],".RData"))

#### Linear ####
source("Scripts/functions.R")

type <- c("Dependent", "All")
Hs <- c(3,6,12)
sets <- c("YA","YM","YN","YS","YW")
stats <- list()
stats_lm <- list()

for(s in 5){
  set <- sets[s]
  for (p in 3){
    h <- Hs[p]
    print(paste0("Linear Model: ", set, "-H", h))
    
    m1_data <- cbind(read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/",set,".csv")),
                     read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/",set,"_lag.csv")))
    m2_data <- cbind(m1_data, read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/m2_vars.csv")))
    m3_data <- cbind(m1_data, read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/m3_vars.csv")))
    m4_data <- cbind(m1_data, read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/m4_vars.csv")))
    m5_data <- cbind(m1_data, read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/m5_vars.csv")))
    m6_data <- cbind(m1_data, read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/m6_vars.csv")))
    m6_data[,373:492] <- log(m6_data[,373:492])

m1_fc <- rwf_lm2(m1_data,0.5, norm = T)
m2_fc <- rwf_lm2(m2_data,0.5, norm = T)
m3_fc <- rwf_lm2(m3_data,0.5, norm = T)
m4_fc <- rwf_lm2(m4_data,0.5, norm = T)
m5_fc <- rwf_lm2(m5_data,0.5, norm = T)
m6_fc <- rwf_lm2(m6_data,0.5, norm = T)

data <- read.csv2(paste0("Output/Data/H/",type[1],"/Short/h",h,"/short_expanded.csv")) %>% select(date)
date <- ym(str_replace(data$date, "M", ""))
date <- date[-idx]

results <- tibble(date, Y = test_Y, m1_fc, m2_fc, m3_fc, m4_fc, m5_fc, m6_fc)
results <- results %>% gather(m1_fc, m2_fc, m3_fc, m4_fc, m5_fc, m6_fc,
                              key = "model", value = "fc")

models <- c("m1_fc", "m2_fc", "m3_fc", "m4_fc", "m5_fc", "m6_fc")
plotnames <- paste0(set,c(" - Model 1 - Linear Model", " - Model 2 - Linear Model",
                          " - Model 3 - Linear Model", " - Model 4 - Linear Model",
                          " - Model 5 - Linear Model", " - Model 6 - Linear Model"))
gw <- c(NA)
gw2 <- c(NA)
for (i in 2:6) {
  m1 <- results %>% filter(model == models[1])
  mn <- results %>% filter(model == models[i])
  gw[i] <- gw.test(m1$fc, mn$fc, length(test_Y), nrow(m1_data), 1, alternative = "two.sided")$p.value
  gw2[i] <- gw.test(m1$fc, mn$fc, length(test_Y), nrow(m1_data), 1, alternative = "two.sided", power = 1)$p.value
}

statistics_lm <- data.frame(Model = NA, RMSE = NA, MAE = NA)
for (i in 1:6) {
  temp <- filter(results, model == models[i])
  rmse <- sqrt(sum((temp$fc - temp$Y)^2)/nrow(temp))
  mae <- (1/nrow(temp))*sum(abs((temp$Y - temp$fc)))
  statistics_lm[i,] <- c(plotnames[i],rmse, mae)
}
statistics_lm <- cbind(statistics_lm, "GW Test MSE" = gw, "GW Test MAE" = gw2) %>%
  mutate(RMSE = as.numeric(RMSE), MAE = as.numeric(MAE))

save.image(file = paste0(
  "Scripts/RW Forecast H/Short/Objects/",set,"-norm-linear-h",h,"-",type[1],".RData"))

stats[[p]] <- statistics_lm
beep(2)
  }
  names(stats) <- c("h3", "h6", "h12")
  
  stats_lm[[s]] <- stats
  
  #save(list(stats,vars), file = paste0("Scripts/RW Forecast H/Short/Results/",set,"-norm-linear-",type[1],".RData"))
}
beep(11)
names(stats_lm) <- c("YA","YM","YN","YS","YW")

rm(list=setdiff(ls(), c("stats_lm", "type")))
save.image(file = paste0("Scripts/RW Forecast H/Short/Results/norm-linear-",type[1],".RData"))
