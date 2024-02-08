source("Scripts/functions.R")

#### Boosting ####
load("Scripts/RW Forecast/Short/Objects/h3-YA-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/h6-YA-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/h12-YA-boosting05-env.RData")

#h = 3
#h = 6
h = 12

YA_m1_data <- cbind(read.csv2("Output/Data/Short/YA.csv"), read.csv2("Output/Data/Short/YA_lag.csv"))
YA_m2_data <- cbind(YA_m1_data, read.csv2("Output/Data/Short/m2_vars.csv"))
YA_m3_data <- cbind(YA_m1_data, read.csv2("Output/Data/Short/m3_vars.csv"))
YA_m4_data <- cbind(YA_m1_data, read.csv2("Output/Data/Short/m4_vars.csv"))
YA_m5_data <- cbind(YA_m1_data, read.csv2("Output/Data/Short/m5_vars.csv"))
YA_m6_data <- cbind(YA_m1_data, read.csv2("Output/Data/Short/m6_vars.csv"))
YA_m6_data[,373:492] <- log(YA_m6_data[,373:492])

YA_m1_fc <- rwf_mboost2(YA_m1_data,0.5, norm = T, h = h)
YA_m1_vars <- vars_freq(variables)
YA_m1_vars_avg <- variables_avg

YA_m2_fc <- rwf_mboost2(YA_m2_data,0.5, norm = T, h = h)
YA_m2_vars <- vars_freq(variables)
YA_m2_vars_avg <- variables_avg

YA_m3_fc <- rwf_mboost2(YA_m3_data,0.5, norm = T, h = h)
YA_m3_vars <- vars_freq(variables)
YA_m3_vars_avg <- variables_avg

YA_m4_fc <- rwf_mboost2(YA_m4_data,0.5, norm = T, h = h)
YA_m4_vars <- vars_freq(variables)
YA_m4_vars_avg <- variables_avg

YA_m5_fc <- rwf_mboost2(YA_m5_data,0.5, norm = T, h = h)
YA_m5_vars <- vars_freq(variables)
YA_m5_vars_avg <- variables_avg

YA_m6_fc <- rwf_mboost2(YA_m6_data,0.5, norm = T, h = h)
YA_m6_vars <- vars_freq(variables)
YA_m6_vars_avg <- variables_avg

data <- read.csv2("Output/Data/Short/short_expanded.csv") %>% select(Period)
date <- ym(str_replace(data$Period, "M", ""))
date <- date[-idx]
date <- date[-(1:(h-1))]

YA_results <- tibble(date, Y = test_Y, YA_m1_fc, YA_m2_fc, YA_m3_fc, YA_m4_fc, YA_m5_fc, YA_m6_fc)
YA_results <- YA_results %>% gather(YA_m1_fc, YA_m2_fc, YA_m3_fc, YA_m4_fc, YA_m5_fc, YA_m6_fc,
                              key = "model", value = "fc")

models <- c("YA_m1_fc", "YA_m2_fc", "YA_m3_fc", "YA_m4_fc", "YA_m5_fc", "YA_m6_fc")
plotnames <- c("YA - Model 1 - mboost", "YA - Model 2 - mboost",
               "YA - Model 3 - mboost", "YA - Model 4 - mboost",
               "YA - Model 5 - mboost", "YA - Model 6 - mboost")

gw <- c(NA)
gw2 <- c(NA)
for (i in 2:6) {
  m1 <- YA_results %>% filter(model == models[1])
  mn <- YA_results %>% filter(model == models[i])
  gw[i] <- gw.test(m1$fc, mn$fc, length(test_Y), 545-(h-1), 1, alternative = "two.sided")$p.value
  gw2[i] <- gw.test(m1$fc, mn$fc, length(test_Y), 545-(h-1), 1, alternative = "two.sided", power = 1)$p.value
}

YA_statistics_mboost <- data.frame(Model = NA, RMSE = NA, MAPE = NA)
MediaVariaveis = c(YA_m1_vars_avg, YA_m2_vars_avg, YA_m3_vars_avg,
                   YA_m4_vars_avg, YA_m5_vars_avg, YA_m6_vars_avg)
for (i in 1:6) {
  temp <- filter(YA_results, model == models[i])
  rmse <- sqrt(sum((temp$fc - temp$Y)^2)/nrow(temp))
  mape <- (100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y))
  #mape <-(100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y)[abs((temp$Y - temp$fc)/temp$Y) < 10000000])
  YA_statistics_mboost[i,] <- c(plotnames[i],rmse, mape)
}
YA_statistics_mboost <- cbind(YA_statistics_mboost, MediaVariaveis,
                              "GW Test MSE" = gw, "GW Test MAPE" = gw2)

YA_top_vars <- data.frame(
  top_n(YA_m2_vars, 15)[1:15,1],
  top_n(YA_m2_vars, 15)[1:15,3],
  top_n(YA_m3_vars, 15)[1:15,1],
  top_n(YA_m3_vars, 15)[1:15,3],
  top_n(YA_m4_vars, 15)[1:15,1],
  top_n(YA_m4_vars, 15)[1:15,3],
  top_n(YA_m5_vars, 15)[1:15,1],
  top_n(YA_m5_vars, 15)[1:15,3],
  top_n(YA_m6_vars, 15)[1:15,1],
  top_n(YA_m6_vars, 15)[1:15,3]
)
colnames(YA_top_vars) <- c("Model 2", "Relative 2", "Model 3", "Relative 3", "Model 4", "Relative 4",
                           "Model 5","Relative 5","Model 6","Relative 6")
beep(11)
for (i in 1:6) { 
  names <- paste0("Output/Plots/RW Forecast/Short H/",plotnames[i]," - h",h,".jpeg")
  jpeg(filename = names, width = 1920, height = 1080)
  
  print(ggplot(YA_results[YA_results$model == models[i],])+
          geom_line(aes(x = date, y = fc, color = paste0("Forecast (h=", h,")")), size = 1)+
          geom_line(aes(x = date, y = Y, color = "Y"), size = 1)+
          scale_color_manual("", values = c("Forecast (h=12)"  = "red", "Y" = "black"))+
          scale_x_date(date_breaks = "1 year", labels = date_format("%Y"),expand = c(0.05,0.005))+
          ggtitle(plotnames[i])+
          labs(x= NULL, y = NULL)+
          theme(legend.position = c(0.07,0.95),
                legend.background = element_rect(fill = NA),
                legend.key = element_rect(size = 30),
                legend.text = element_text(size = 30),
                plot.title = element_text(hjust = 0.5, size = 40),
                axis.text = element_text(size = 20)))
  dev.off()
}

#save.image(file = "Scripts/RW Forecast/Short/Objects/h12-YA-boosting05-env.RData")

#### Linear ####
load("Scripts/RW Forecast/Short/Objects/h3-YA-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/h6-YA-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/h12-YA-linear05-env.RData")

#h = 3
h = 6
#h = 12

YA_m1_data <- cbind(read.csv2("Output/Data/Short/YA.csv"), read.csv2("Output/Data/Short/YA_lag.csv"))
YA_m2_data <- cbind(YA_m1_data, read.csv2("Output/Data/Short/m2_vars.csv"))
YA_m3_data <- cbind(YA_m1_data, read.csv2("Output/Data/Short/m3_vars.csv"))
YA_m4_data <- cbind(YA_m1_data, read.csv2("Output/Data/Short/m4_vars.csv"))
YA_m5_data <- cbind(YA_m1_data, read.csv2("Output/Data/Short/m5_vars.csv"))
YA_m6_data <- cbind(YA_m1_data, read.csv2("Output/Data/Short/m6_vars.csv"))
YA_m6_data[,373:492] <- log(YA_m6_data[,373:492])

YA_m1_fc <- rwf_lm2(YA_m1_data,0.5, norm = T, h = h)
YA_m2_fc <- rwf_lm2(YA_m2_data,0.5, norm = T, h = h)
YA_m3_fc <- rwf_lm2(YA_m3_data,0.5, norm = T, h = h)
YA_m4_fc <- rwf_lm2(YA_m4_data,0.5, norm = T, h = h)
YA_m5_fc <- rwf_lm2(YA_m5_data,0.5, norm = T, h = h)
YA_m6_fc <- rwf_lm2(YA_m6_data,0.5, norm = T, h = h)

data <- read.csv2("Output/Data/Short/short_expanded.csv") %>% select(Period)
date <- ym(str_replace(data$Period, "M", ""))
date <- date[-idx]
date <- date[-(1:(h-1))]

YA_results <- tibble(date, Y = test_Y, YA_m1_fc, YA_m2_fc, YA_m3_fc, YA_m4_fc, YA_m5_fc, YA_m6_fc)
YA_results <- YA_results %>% gather(YA_m1_fc, YA_m2_fc, YA_m3_fc, YA_m4_fc, YA_m5_fc, YA_m6_fc,
                                    key = "model", value = "fc")

models <- c("YA_m1_fc", "YA_m2_fc", "YA_m3_fc", "YA_m4_fc", "YA_m5_fc", "YA_m6_fc")
plotnames <- c("YA - Model 1 - Linear Model", "YA - Model 2 - Linear Model",
               "YA - Model 3 - Linear Model", "YA - Model 4 - Linear Model",
               "YA - Model 5 - Linear Model", "YA - Model 6 - Linear Model")
gw <- c(NA)
gw2 <- c(NA)
for (i in 2:6) {
  m1 <- YA_results %>% filter(model == models[1])
  mn <- YA_results %>% filter(model == models[i])
  gw[i] <- gw.test(m1$fc, mn$fc, length(test_Y), 545-(h-1), 1, alternative = "two.sided")$p.value
  gw2[i] <- gw.test(m1$fc, mn$fc, length(test_Y), 545-(h-1), 1, alternative = "two.sided", power = 1)$p.value
}

YA_statistics_lm <- data.frame(Model = NA, RMSE = NA, MAPE = NA)
for (i in 1:6) {
  temp <- filter(YA_results, model == models[i])
  rmse <- sqrt(sum((temp$fc - temp$Y)^2)/nrow(temp))
  mape <- (100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y))
  #mape <-(100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y)[abs((temp$Y - temp$fc)/temp$Y) < 10000000]
  YA_statistics_lm[i,] <- c(plotnames[i],rmse, mape)
}
YA_statistics_lm <- cbind(YA_statistics_lm, "GW Test MSE" = gw, "GW Test MAPE" = gw2)
beep(11)
for (i in 1:6) { 
  names <- paste0("Output/Plots/RW Forecast/Short H/",plotnames[i]," - h",h,".jpeg")
  jpeg(filename = names, width = 1920, height = 1080)
  
  print(ggplot(YA_results[YA_results$model == models[i],])+
          geom_line(aes(x = date, y = fc, color = paste0("Forecast (h=", h,")")), size = 1)+
          geom_line(aes(x = date, y = Y, color = "Y"), size = 1)+
          scale_color_manual("", values = c("Forecast (h=6)" = "red", "Y" = "black"))+
          scale_x_date(date_breaks = "1 year", labels = date_format("%Y"),expand = c(0.05,0.005))+
          ggtitle(plotnames[i])+
          labs(x= NULL, y = NULL)+
          theme(legend.position = c(0.07,0.95),
                legend.background = element_rect(fill = NA),
                legend.key = element_rect(size = 30),
                legend.text = element_text(size = 30),
                plot.title = element_text(hjust = 0.5, size = 40),
                axis.text = element_text(size = 20)))
  dev.off()
}

#save.image(file = "Scripts/RW Forecast/Short/Objects/h6-YA-linear05-env.RData")
