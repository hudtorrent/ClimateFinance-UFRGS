source("Scripts/functions.R")

#### Boosting ####
load("Scripts/RW Forecast/Short/Objects/h3-YW-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/h6-YW-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/h12-YW-boosting05-env.RData")

#h = 3
#h = 6
h = 12

YW_m1_data <- cbind(read.csv2("Output/Data/Short/YW.csv"), read.csv2("Output/Data/Short/YW_lag.csv"))
YW_m2_data <- cbind(YW_m1_data, read.csv2("Output/Data/Short/m2_vars.csv"))
YW_m3_data <- cbind(YW_m1_data, read.csv2("Output/Data/Short/m3_vars.csv"))
YW_m4_data <- cbind(YW_m1_data, read.csv2("Output/Data/Short/m4_vars.csv"))
YW_m5_data <- cbind(YW_m1_data, read.csv2("Output/Data/Short/m5_vars.csv"))
YW_m6_data <- cbind(YW_m1_data, read.csv2("Output/Data/Short/m6_vars.csv"))
YW_m6_data[,373:492] <- log(YW_m6_data[,373:492])

YW_m1_fc <- rwf_mboost2(YW_m1_data,0.5, norm = T, h = h)
YW_m1_vars <- vars_freq(variables)
YW_m1_vars_avg <- variables_avg

YW_m2_fc <- rwf_mboost2(YW_m2_data,0.5, norm = T, h = h)
YW_m2_vars <- vars_freq(variables)
YW_m2_vars_avg <- variables_avg

YW_m3_fc <- rwf_mboost2(YW_m3_data,0.5, norm = T, h = h)
YW_m3_vars <- vars_freq(variables)
YW_m3_vars_avg <- variables_avg

YW_m4_fc <- rwf_mboost2(YW_m4_data,0.5, norm = T, h = h)
YW_m4_vars <- vars_freq(variables)
YW_m4_vars_avg <- variables_avg

YW_m5_fc <- rwf_mboost2(YW_m5_data,0.5, norm = T, h = h)
YW_m5_vars <- vars_freq(variables)
YW_m5_vars_avg <- variables_avg

YW_m6_fc <- rwf_mboost2(YW_m6_data,0.5, norm = T, h = h)
YW_m6_vars <- vars_freq(variables)
YW_m6_vars_avg <- variables_avg

data <- read.csv2("Output/Data/Short/short_expanded.csv") %>% select(Period)
date <- ym(str_replace(data$Period, "M", ""))
date <- date[-idx]
date <- date[-(1:(h-1))]

YW_results <- tibble(date, Y = test_Y, YW_m1_fc, YW_m2_fc, YW_m3_fc, YW_m4_fc, YW_m5_fc, YW_m6_fc)
YW_results <- YW_results %>% gather(YW_m1_fc, YW_m2_fc, YW_m3_fc, YW_m4_fc, YW_m5_fc, YW_m6_fc,
                                    key = "model", value = "fc")

models <- c("YW_m1_fc", "YW_m2_fc", "YW_m3_fc", "YW_m4_fc", "YW_m5_fc", "YW_m6_fc")
plotnames <- c("YW - Model 1 - mboost", "YW - Model 2 - mboost",
               "YW - Model 3 - mboost", "YW - Model 4 - mboost",
               "YW - Model 5 - mboost", "YW - Model 6 - mboost")

gw <- c(NA)
gw2 <- c(NA)
for (i in 2:6) {
  m1 <- YW_results %>% filter(model == models[1])
  mn <- YW_results %>% filter(model == models[i])
  gw[i] <- gw.test(m1$fc, mn$fc, length(test_Y), 545-(h-1), 1, alternative = "two.sided")$p.value
  gw2[i] <- gw.test(m1$fc, mn$fc, length(test_Y), 545-(h-1), 1, alternative = "two.sided", power = 1)$p.value
}

YW_statistics_mboost <- data.frame(Model = NA, RMSE = NA, MAPE = NA)
MediaVariaveis = c(YW_m1_vars_avg, YW_m2_vars_avg, YW_m3_vars_avg,
                   YW_m4_vars_avg, YW_m5_vars_avg, YW_m6_vars_avg)
for (i in 1:6) {
  temp <- filter(YW_results, model == models[i])
  rmse <- sqrt(sum((temp$fc - temp$Y)^2)/nrow(temp))
  mape <- (100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y))
  #mape <-(100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y)[abs((temp$Y - temp$fc)/temp$Y) < 10000000])
  YW_statistics_mboost[i,] <- c(plotnames[i],rmse, mape)
}
YW_statistics_mboost <- cbind(YW_statistics_mboost, MediaVariaveis,
                              "GW Test MSE" = gw, "GW Test MAPE" = gw2)

YW_top_vars <- data.frame(
  top_n(YW_m2_vars, 15)[1:15,1],
  top_n(YW_m2_vars, 15)[1:15,3],
  top_n(YW_m3_vars, 15)[1:15,1],
  top_n(YW_m3_vars, 15)[1:15,3],
  top_n(YW_m4_vars, 15)[1:15,1],
  top_n(YW_m4_vars, 15)[1:15,3],
  top_n(YW_m5_vars, 15)[1:15,1],
  top_n(YW_m5_vars, 15)[1:15,3],
  top_n(YW_m6_vars, 15)[1:15,1],
  top_n(YW_m6_vars, 15)[1:15,3]
)
colnames(YW_top_vars) <- c("Model 2", "Relative 2", "Model 3", "Relative 3", "Model 4", "Relative 4",
                           "Model 5","Relative 5","Model 6","Relative 6")
beep(11)
for (i in 1:6) { 
  names <- paste0("Output/Plots/RW Forecast/Short H/",plotnames[i]," - h",h,".jpeg")
  jpeg(filename = names, width = 1920, height = 1080)
  
  print(ggplot(YW_results[YW_results$model == models[i],])+
          geom_line(aes(x = date, y = fc, color = paste0("Forecast (h=", h,")")), size = 1)+
          geom_line(aes(x = date, y = Y, color = "Y"), size = 1)+
          scale_color_manual("", values = c("Forecast (h=12)" = "red", "Y" = "black"))+
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

#save.image(file = "Scripts/RW Forecast/Short/Objects/h12-YW-boosting05-env.RData")

#### Linear ####
load("Scripts/RW Forecast/Short/Objects/h3-YW-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/h6-YW-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/h12-YW-linear05-env.RData")

#h = 3
#h = 6
h = 12

YW_m1_data <- cbind(read.csv2("Output/Data/Short/YW.csv"), read.csv2("Output/Data/Short/YW_lag.csv"))
YW_m2_data <- cbind(YW_m1_data, read.csv2("Output/Data/Short/m2_vars.csv"))
YW_m3_data <- cbind(YW_m1_data, read.csv2("Output/Data/Short/m3_vars.csv"))
YW_m4_data <- cbind(YW_m1_data, read.csv2("Output/Data/Short/m4_vars.csv"))
YW_m5_data <- cbind(YW_m1_data, read.csv2("Output/Data/Short/m5_vars.csv"))
YW_m6_data <- cbind(YW_m1_data, read.csv2("Output/Data/Short/m6_vars.csv"))
YW_m6_data[,373:492] <- log(YW_m6_data[,373:492])

YW_m1_fc <- rwf_lm2(YW_m1_data,0.5, norm = T, h = h)
YW_m2_fc <- rwf_lm2(YW_m2_data,0.5, norm = T, h = h)
YW_m3_fc <- rwf_lm2(YW_m3_data,0.5, norm = T, h = h)
YW_m4_fc <- rwf_lm2(YW_m4_data,0.5, norm = T, h = h)
YW_m5_fc <- rwf_lm2(YW_m5_data,0.5, norm = T, h = h)
YW_m6_fc <- rwf_lm2(YW_m6_data,0.5, norm = T, h = h)

data <- read.csv2("Output/Data/Short/short_expanded.csv") %>% select(Period)
date <- ym(str_replace(data$Period, "M", ""))
date <- date[-idx]
date <- date[-(1:(h-1))]

YW_results <- tibble(date, Y = test_Y, YW_m1_fc, YW_m2_fc, YW_m3_fc, YW_m4_fc, YW_m5_fc, YW_m6_fc)
YW_results <- YW_results %>% gather(YW_m1_fc, YW_m2_fc, YW_m3_fc, YW_m4_fc, YW_m5_fc, YW_m6_fc,
                                    key = "model", value = "fc")

models <- c("YW_m1_fc", "YW_m2_fc", "YW_m3_fc", "YW_m4_fc", "YW_m5_fc", "YW_m6_fc")
plotnames <- c("YW - Model 1 - Linear Model", "YW - Model 2 - Linear Model",
               "YW - Model 3 - Linear Model", "YW - Model 4 - Linear Model",
               "YW - Model 5 - Linear Model", "YW - Model 6 - Linear Model")

gw <- c(NA)
gw2 <- c(NA)
for (i in 2:6) {
  m1 <- YW_results %>% filter(model == models[1])
  mn <- YW_results %>% filter(model == models[i])
  gw[i] <- gw.test(m1$fc, mn$fc, length(test_Y), 545-(h-1), 1, alternative = "two.sided")$p.value
  gw2[i] <- gw.test(m1$fc, mn$fc, length(test_Y), 545-(h-1), 1, alternative = "two.sided", power = 1)$p.value
}

YW_statistics_lm <- data.frame(Model = NA, RMSE = NA, MAPE = NA)
for (i in 1:6) {
  temp <- filter(YW_results, model == models[i])
  rmse <- sqrt(sum((temp$fc - temp$Y)^2)/nrow(temp))
  mape <- (100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y))
  #mape <-(100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y)[abs((temp$Y - temp$fc)/temp$Y) < 10000000]
  YW_statistics_lm[i,] <- c(plotnames[i],rmse, mape)
}
YW_statistics_lm <- cbind(YW_statistics_lm, "GW Test MSE" = gw, "GW Test MAPE" = gw2)
beep(11)
for (i in 1:6) { 
  names <- paste0("Output/Plots/RW Forecast/Short H/",plotnames[i]," - h",h,".jpeg")
  jpeg(filename = names, width = 1920, height = 1080)
  
  print(ggplot(YW_results[YW_results$model == models[i],])+
          geom_line(aes(x = date, y = fc, color = paste0("Forecast (h=", h,")")), size = 1)+
          geom_line(aes(x = date, y = Y, color = "Y"), size = 1)+
          scale_color_manual("", values = c("Forecast (h=12)" = "red", "Y" = "black"))+
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

#save.image(file = "Scripts/RW Forecast/Short/Objects/h12-YW-linear05-env.RData")
