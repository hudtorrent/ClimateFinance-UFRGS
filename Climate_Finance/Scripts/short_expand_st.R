source("Scripts/functions.R")

set1 <- read.csv2("Data/Short/1macro_financial.csv")
set2 <- read.csv2("Data/Short/2eco_fin_uncert.csv")
set3 <- read.csv2("Data/Short/3non_eco_fin_uncert.csv")
set4 <- read.csv2("Data/Short/4climate_change.csv")
set5 <- read.csv2("Data/Short/5climate_change_vol.csv")
y <- read.csv2("Data/Short/housing_returns.csv")
date <- y %>% select(Period)
y <- y %>% select(-Period)

st_series(set1)
st_series(set2)
set2 <- set2 %>% mutate(MEU3 = diff_series$MEU3, MEU12 = diff_series$MEU12)
st_series(set3)
set3 <- set3 %>% mutate(NMEU3 = diff_series$NMEU3, NMEU12 = diff_series$NMEU12, NFEU3 = diff_series$NFEU3)
st_series(set4)
st_series(set5)
#st_series(y)

set1 <- tail(set1, -1)
set2 <- tail(set2, -1)
set3 <- tail(set3, -1)
set4 <- tail(set4, -1)
set5 <- tail(set5, -1)
y <- tail(y, -1)
date <- tail(date, - 12)


y_lag <- add_lags(data = y, lags = 11)
set1_lag <- add_lags(data = set1, lags = 11)
set2_lag <- add_lags(data = set2, lags = 11)
set3_lag <- add_lags(data = set3, lags = 11)
set4_lag <- add_lags(data = set4, lags = 11)
set5_lag <- add_lags(data = set5, lags = 11)

m1_data <- y_lag
m2_data <- set1_lag
m3_data <- cbind(m2_data, set2_lag)
m4_data <- cbind(m3_data, set3_lag)
m5_data <- cbind(m4_data, set4_lag)
m6_data <- cbind(m5_data, set5_lag)

#short_expand <- cbind(date, y_lag, set1_lag, set2_lag, set3_lag, set4_lag, set5_lag)

write.csv2(m2_data, file = "Output/Data/Stationary/Short/m2_vars.csv", row.names = F)
write.csv2(m3_data, file = "Output/Data/Stationary/Short/m3_vars.csv", row.names = F)
write.csv2(m4_data, file = "Output/Data/Stationary/Short/m4_vars.csv", row.names = F)
write.csv2(m5_data, file = "Output/Data/Stationary/Short/m5_vars.csv", row.names = F)
write.csv2(m6_data, file = "Output/Data/Stationary/Short/m6_vars.csv", row.names = F)
#write.csv2(short_expand,file = "Output/Data/Stationary/Short/short_expanded.csv", row.names = F)

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

write.csv2(YA, file = "Output/Data/Stationary/Short/YA.csv", row.names = F)
write.csv2(YA_lag, file = "Output/Data/Stationary/Short/YA_lag.csv", row.names = F)
write.csv2(YN, file = "Output/Data/Stationary/Short/YN.csv", row.names = F)
write.csv2(YN_lag, file = "Output/Data/Stationary/Short/YN_lag.csv", row.names = F)
write.csv2(YM, file = "Output/Data/Stationary/Short/YM.csv", row.names = F)
write.csv2(YM_lag, file = "Output/Data/Stationary/Short/YM_lag.csv", row.names = F)
write.csv2(YS, file = "Output/Data/Stationary/Short/YS.csv", row.names = F)
write.csv2(YS_lag, file = "Output/Data/Stationary/Short/YS_lag.csv", row.names = F)
write.csv2(YW, file = "Output/Data/Stationary/Short/YW.csv", row.names = F)
write.csv2(YW_lag, file = "Output/Data/Stationary/Short/YW_lag.csv", row.names = F)