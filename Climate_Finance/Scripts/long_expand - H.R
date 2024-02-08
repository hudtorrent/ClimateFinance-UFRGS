source("Scripts/functions.R")

set1 <- read.csv2("Data/Long/1macro_financial.csv")
set2 <- read.csv2("Data/Long/2eco_fin_uncert.csv")
set3 <- read.csv2("Data/Long/3non_eco_fin_uncert.csv")
set4 <- read.csv2("Data/Long/4climate_change.csv")
set5 <- read.csv2("Data/Long/5climate_change_vol.csv")
y <- read.csv2("Data/Long/housing_returns.csv")
date <- y %>% select(Date)
y <- y %>% select(-Date)

##### Y Only #####

Hs <- c(3,6,12)

for (i in 1:3) {
  y <- read.csv2("Data/Long/housing_returns.csv")
  date <- y %>% select(Date)
  y <- y %>% select(-Date)
  h <- Hs[i]
  
  y <- data.frame(Y = h_sets(y, h))


y_lag <- add_lags(data = y, lags = 11)
set1_lag <- add_lags(data = set1[-(1:(h-1)),], lags = 11)
set2_lag <- add_lags(data = set2[-(1:(h-1)),], lags = 11)
set3_lag <- add_lags(data = set3[-(1:(h-1)),], lags = 11)
set4_lag <- add_lags(data = set4[-(1:(h-1)),], lags = 11)
set5_lag <- add_lags(data = set5[-(1:(h-1)),], lags = 11)
date <- date[-(1:(h-1)),] %>% tail(-11)

m1_data <- y_lag
m2_data <- cbind(m1_data, set1_lag)
m3_data <- cbind(m2_data, set2_lag)
m4_data <- cbind(m3_data, set3_lag)
m5_data <- cbind(m4_data, set4_lag)
m6_data <- cbind(m5_data, set5_lag)

long_expand <- cbind(date, y_lag, set1_lag, set2_lag, set3_lag, set4_lag, set5_lag)

write.csv2(m1_data, file = paste0("Output/Data/H/Dependent/Long/h",h,"/m1_data.csv"), row.names = F)
write.csv2(m2_data, file = paste0("Output/Data/H/Dependent/Long/h",h,"/m2_data.csv"), row.names = F)
write.csv2(m3_data, file = paste0("Output/Data/H/Dependent/Long/h",h,"/m3_data.csv"), row.names = F)
write.csv2(m4_data, file = paste0("Output/Data/H/Dependent/Long/h",h,"/m4_data.csv"), row.names = F)
write.csv2(m5_data, file = paste0("Output/Data/H/Dependent/Long/h",h,"/m5_data.csv"), row.names = F)
write.csv2(m6_data, file = paste0("Output/Data/H/Dependent/Long/h",h,"/m6_data.csv"), row.names = F)
write.csv2(long_expand,file = paste0("Output/Data/H/Dependent/Long/h",h,"/long_expanded.csv"),
           row.names = F)

}
##### All #####

Hs <- c(3,6,12)

for (i in 1:3) {
  set1 <- read.csv2("Data/Long/1macro_financial.csv")
  set2 <- read.csv2("Data/Long/2eco_fin_uncert.csv")
  set3 <- read.csv2("Data/Long/3non_eco_fin_uncert.csv")
  set4 <- read.csv2("Data/Long/4climate_change.csv")
  set5 <- read.csv2("Data/Long/5climate_change_vol.csv")
  y <- read.csv2("Data/Long/housing_returns.csv")
  date <- y %>% select(Date)
  y <- y %>% select(-Date)
  h <- Hs[i]
  
  y <- data.frame(Y = h_sets(y, h))
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
  m2_data <- cbind(m1_data, set1_lag)
  m3_data <- cbind(m2_data, set2_lag)
  m4_data <- cbind(m3_data, set3_lag)
  m5_data <- cbind(m4_data, set4_lag)
  m6_data <- cbind(m5_data, set5_lag)
  
  long_expand <- cbind(date, y_lag, set1_lag, set2_lag, set3_lag, set4_lag, set5_lag)
  
  write.csv2(m1_data, file = paste0("Output/Data/H/All/Long/h",h,"/m1_data.csv"), row.names = F)
  write.csv2(m2_data, file = paste0("Output/Data/H/All/Long/h",h,"/m2_data.csv"), row.names = F)
  write.csv2(m3_data, file = paste0("Output/Data/H/All/Long/h",h,"/m3_data.csv"), row.names = F)
  write.csv2(m4_data, file = paste0("Output/Data/H/All/Long/h",h,"/m4_data.csv"), row.names = F)
  write.csv2(m5_data, file = paste0("Output/Data/H/All/Long/h",h,"/m5_data.csv"), row.names = F)
  write.csv2(m6_data, file = paste0("Output/Data/H/All/Long/h",h,"/m6_data.csv"), row.names = F)
  write.csv2(long_expand,file = paste0("Output/Data/H/All/Long/h",h,"/long_expanded.csv"),
             row.names = F)
}