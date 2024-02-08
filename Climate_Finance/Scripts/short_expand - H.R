source("Scripts/functions.R")

set1 <- read.csv2("Data/Short/1macro_financial.csv")
set2 <- read.csv2("Data/Short/2eco_fin_uncert.csv")
set3 <- read.csv2("Data/Short/3non_eco_fin_uncert.csv")
set4 <- read.csv2("Data/Short/4climate_change.csv")
set5 <- read.csv2("Data/Short/5climate_change_vol.csv")
y <- read.csv2("Data/Short/housing_returns.csv")
date <- y %>% select(Period)
y <- y %>% select(-Period)

##### Y Only #####

Hs <- c(3,6,12)

for (i in 1:3) {
  y <- read.csv2("Data/Short/housing_returns.csv")
  date <- y %>% select(Period)
  y <- y %>% select(-Period)
  h <- Hs[i]
  
  y <- h_sets(y,h)
  
  
  y_lag <- add_lags(data = y, lags = 11)
  set1_lag <- add_lags(data = set1[-(1:(h-1)),], lags = 11)
  set2_lag <- add_lags(data = set2[-(1:(h-1)),], lags = 11)
  set3_lag <- add_lags(data = set3[-(1:(h-1)),], lags = 11)
  set4_lag <- add_lags(data = set4[-(1:(h-1)),], lags = 11)
  set5_lag <- add_lags(data = set5[-(1:(h-1)),], lags = 11)
  date <- date[-(1:(h-1)),] %>% tail(-11)
  
  m1_data <- y_lag
  m2_data <- set1_lag
  m3_data <- cbind(m2_data, set2_lag)
  m4_data <- cbind(m3_data, set3_lag)
  m5_data <- cbind(m4_data, set4_lag)
  m6_data <- cbind(m5_data, set5_lag)
  
  short_expand <- cbind(date, y_lag, set1_lag, set2_lag, set3_lag, set4_lag, set5_lag)
  
  write.csv2(m2_data, file = paste0("Output/Data/H/Dependent/Short/h",h,"/m2_vars.csv"), row.names = F)
  write.csv2(m3_data, file = paste0("Output/Data/H/Dependent/Short/h",h,"/m3_vars.csv"), row.names = F)
  write.csv2(m4_data, file = paste0("Output/Data/H/Dependent/Short/h",h,"/m4_vars.csv"), row.names = F)
  write.csv2(m5_data, file = paste0("Output/Data/H/Dependent/Short/h",h,"/m5_vars.csv"), row.names = F)
  write.csv2(m6_data, file = paste0("Output/Data/H/Dependent/Short/h",h,"/m6_vars.csv"), row.names = F)
  write.csv2(short_expand,file = paste0("Output/Data/H/Dependent/Short/h",h,"/short_expanded.csv"),
             row.names = F)
  
  idx <- names(m1_data) %>% str_detect("YA")
  YA_lag <- m1_data[idx] %>% select(-1)
  YA <- m1_data[idx] %>% select(1)
  
  idx <- names(m1_data) %>% str_detect("YN")
  YN_lag <- m1_data[idx] %>% select(-1)
  YN <- m1_data[idx] %>% select(1)
  
  idx <- names(m1_data) %>% str_detect("YM")
  YM_lag <- m1_data[idx] %>% select(-1)
  YM <- m1_data[idx] %>% select(1)
  
  idx <- names(m1_data) %>% str_detect("YS")
  YS_lag <- m1_data[idx] %>% select(-1)
  YS <- m1_data[idx] %>% select(1)
  
  idx <- names(m1_data) %>% str_detect("YW")
  YW_lag <- m1_data[idx] %>% select(-1)
  YW <- m1_data[idx] %>% select(1)
  
  write.csv2(YA, file = paste0("Output/Data/H/Dependent/Short/h",h,"/YA.csv"), row.names = F)
  write.csv2(YA_lag, file = paste0("Output/Data/H/Dependent/Short/h",h,"/YA_lag.csv"), row.names = F)
  write.csv2(YN, file = paste0("Output/Data/H/Dependent/Short/h",h,"/YN.csv"), row.names = F)
  write.csv2(YN_lag, file = paste0("Output/Data/H/Dependent/Short/h",h,"/YN_lag.csv"), row.names = F)
  write.csv2(YM, file = paste0("Output/Data/H/Dependent/Short/h",h,"/YM.csv"), row.names = F)
  write.csv2(YM_lag, file = paste0("Output/Data/H/Dependent/Short/h",h,"/YM_lag.csv"), row.names = F)
  write.csv2(YS, file = paste0("Output/Data/H/Dependent/Short/h",h,"/YS.csv"), row.names = F)
  write.csv2(YS_lag, file = paste0("Output/Data/H/Dependent/Short/h",h,"/YS_lag.csv"), row.names = F)
  write.csv2(YW, file = paste0("Output/Data/H/Dependent/Short/h",h,"/YW.csv"), row.names = F)
  write.csv2(YW_lag, file = paste0("Output/Data/H/Dependent/Short/h",h,"/YW_lag.csv"), row.names = F)
}


##### All #####

Hs <- c(3,6,12)

for (i in 1:3) {
  set1 <- read.csv2("Data/Short/1macro_financial.csv")
  set2 <- read.csv2("Data/Short/2eco_fin_uncert.csv")
  set3 <- read.csv2("Data/Short/3non_eco_fin_uncert.csv")
  set4 <- read.csv2("Data/Short/4climate_change.csv")
  set5 <- read.csv2("Data/Short/5climate_change_vol.csv")
  y <- read.csv2("Data/Short/housing_returns.csv")
  date <- y %>% select(Period)
  y <- y %>% select(-Period)
  h <- Hs[i]
  
  y <- h_sets(y,h)
  set1 <- h_sets(set1,h)
  set2 <- h_sets(set2,h)
  set3 <- h_sets(set3,h)
  set4 <- h_sets(set4,h)
  set5 <- h_sets(set5,h)
  
  y_lag <- add_lags(data = y, lags = 11)
  set1_lag <- add_lags(data = set1, lags = 11)
  set2_lag <- add_lags(data = set2, lags = 11)
  set3_lag <- add_lags(data = set3, lags = 11)
  set4_lag <- add_lags(data = set4, lags = 11)
  set5_lag <- add_lags(data = set5, lags = 11)
  date <- date[-(1:(h-1)),] %>% tail(-11)
  
  m1_data <- y_lag
  m2_data <- set1_lag
  m3_data <- cbind(m2_data, set2_lag)
  m4_data <- cbind(m3_data, set3_lag)
  m5_data <- cbind(m4_data, set4_lag)
  m6_data <- cbind(m5_data, set5_lag)
  
  short_expand <- cbind(date, y_lag, set1_lag, set2_lag, set3_lag, set4_lag, set5_lag)
  
  write.csv2(m2_data, file = paste0("Output/Data/H/All/Short/h",h,"/m2_vars.csv"), row.names = F)
  write.csv2(m3_data, file = paste0("Output/Data/H/All/Short/h",h,"/m3_vars.csv"), row.names = F)
  write.csv2(m4_data, file = paste0("Output/Data/H/All/Short/h",h,"/m4_vars.csv"), row.names = F)
  write.csv2(m5_data, file = paste0("Output/Data/H/All/Short/h",h,"/m5_vars.csv"), row.names = F)
  write.csv2(m6_data, file = paste0("Output/Data/H/All/Short/h",h,"/m6_vars.csv"), row.names = F)
  write.csv2(short_expand,file = paste0("Output/Data/H/All/Short/h",h,"/short_expanded.csv"),
             row.names = F)
  
  idx <- names(m1_data) %>% str_detect("YA")
  YA_lag <- m1_data[idx] %>% select(-1)
  YA <- m1_data[idx] %>% select(1)
  
  idx <- names(m1_data) %>% str_detect("YN")
  YN_lag <- m1_data[idx] %>% select(-1)
  YN <- m1_data[idx] %>% select(1)
  
  idx <- names(m1_data) %>% str_detect("YM")
  YM_lag <- m1_data[idx] %>% select(-1)
  YM <- m1_data[idx] %>% select(1)
  
  idx <- names(m1_data) %>% str_detect("YS")
  YS_lag <- m1_data[idx] %>% select(-1)
  YS <- m1_data[idx] %>% select(1)
  
  idx <- names(m1_data) %>% str_detect("YW")
  YW_lag <- m1_data[idx] %>% select(-1)
  YW <- m1_data[idx] %>% select(1)
  
  write.csv2(YA, file = paste0("Output/Data/H/All/Short/h",h,"/YA.csv"), row.names = F)
  write.csv2(YA_lag, file = paste0("Output/Data/H/All/Short/h",h,"/YA_lag.csv"), row.names = F)
  write.csv2(YN, file = paste0("Output/Data/H/All/Short/h",h,"/YN.csv"), row.names = F)
  write.csv2(YN_lag, file = paste0("Output/Data/H/All/Short/h",h,"/YN_lag.csv"), row.names = F)
  write.csv2(YM, file = paste0("Output/Data/H/All/Short/h",h,"/YM.csv"), row.names = F)
  write.csv2(YM_lag, file = paste0("Output/Data/H/All/Short/h",h,"/YM_lag.csv"), row.names = F)
  write.csv2(YS, file = paste0("Output/Data/H/All/Short/h",h,"/YS.csv"), row.names = F)
  write.csv2(YS_lag, file = paste0("Output/Data/H/All/Short/h",h,"/YS_lag.csv"), row.names = F)
  write.csv2(YW, file = paste0("Output/Data/H/All/Short/h",h,"/YW.csv"), row.names = F)
  write.csv2(YW_lag, file = paste0("Output/Data/H/All/Short/h",h,"/YW_lag.csv"), row.names = F)
}