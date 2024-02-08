source("Scripts/functions.R")

#### Boosting ####
load("Scripts/RW Forecast/Short/Objects/h3-YS-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/h6-YS-boosting05-env.RData")
load("Scripts/RW Forecast/Short/Objects/h12-YS-boosting05-env.RData")

#h = 3
#h = 6
h = 12

YS_m1_data <- cbind(read.csv2("Output/Data/Short/YS.csv"), read.csv2("Output/Data/Short/YS_lag.csv"))
YS_m2_data <- cbind(YS_m1_data, read.csv2("Output/Data/Short/m2_vars.csv"))
YS_m3_data <- cbind(YS_m1_data, read.csv2("Output/Data/Short/m3_vars.csv"))
YS_m4_data <- cbind(YS_m1_data, read.csv2("Output/Data/Short/m4_vars.csv"))
YS_m5_data <- cbind(YS_m1_data, read.csv2("Output/Data/Short/m5_vars.csv"))
YS_m6_data <- cbind(YS_m1_data, read.csv2("Output/Data/Short/m6_vars.csv"))
YS_m6_data[,373:492] <- log(YS_m6_data[,373:492])

YS_m1_fc <- rwf_mboost2(YS_m1_data,0.5, norm = T, h = h)
YS_m1_vars <- vars_freq(variables)
YS_m1_vars_avg <- variables_avg

YS_m2_fc <- rwf_mboost2(YS_m2_data,0.5, norm = T, h = h)
YS_m2_vars <- vars_freq(variables)
YS_m2_vars_avg <- variables_avg

YS_m3_fc <- rwf_mboost2(YS_m3_data,0.5, norm = T, h = h)
YS_m3_vars <- vars_freq(variables)
YS_m3_vars_avg <- variables_avg

YS_m4_fc <- rwf_mboost2(YS_m4_data,0.5, norm = T, h = h)
YS_m4_vars <- vars_freq(variables)
YS_m4_vars_avg <- variables_avg

YS_m5_fc <- rwf_mboost2(YS_m5_data,0.5, norm = T, h = h)
YS_m5_vars <- vars_freq(variables)
YS_m5_vars_avg <- variables_avg

YS_m6_fc <- rwf_mboost2(YS_m6_data,0.5, norm = T, h = h)
YS_m6_vars <- vars_freq(variables)
YS_m6_vars_avg <- variables_avg

data <- read.csv2("Output/Data/Short/short_expanded.csv") %>% select(Period)
date <- ym(str_replace(data$Period, "M", ""))
date <- date[-idx]
date <- date[-(1:(h-1))]

YS_results <- tibble(date, Y = test_Y, YS_m1_fc, YS_m2_fc, YS_m3_fc, YS_m4_fc, YS_m5_fc, YS_m6_fc)
YS_results <- YS_results %>% gather(YS_m1_fc, YS_m2_fc, YS_m3_fc, YS_m4_fc, YS_m5_fc, YS_m6_fc,
                                    key = "model", value = "fc")

models <- c("YS_m1_fc", "YS_m2_fc", "YS_m3_fc", "YS_m4_fc", "YS_m5_fc", "YS_m6_fc")
plotnames <- c("YS - Model 1 - mboost", "YS - Model 2 - mboost",
               "YS - Model 3 - mboost", "YS - Model 4 - mboost",
               "YS - Model 5 - mboost", "YS - Model 6 - mboost")

gw <- c(NA)
gw2 <- c(NA)
for (i in 2:6) {
  m1 <- YS_results %>% filter(model == models[1])
  mn <- YS_results %>% filter(model == models[i])
  gw[i] <- gw.test(m1$fc, mn$fc, length(test_Y), 545-(h-1), 1, alternative = "two.sided")$p.value
  gw2[i] <- gw.test(m1$fc, mn$fc, length(test_Y), 545-(h-1), 1, alternative = "two.sided", power = 1)$p.value
}

YS_statistics_mboost <- data.frame(Model = NA, RMSE = NA, MAPE = NA)
MediaVariaveis = c(YS_m1_vars_avg, YS_m2_vars_avg, YS_m3_vars_avg,
                   YS_m4_vars_avg, YS_m5_vars_avg, YS_m6_vars_avg)
for (i in 1:6) {
  temp <- filter(YS_results, model == models[i])
  rmse <- sqrt(sum((temp$fc - temp$Y)^2)/nrow(temp))
  mape <- (100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y))
  #mape <-(100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y)[abs((temp$Y - temp$fc)/temp$Y) < 10000000])
  YS_statistics_mboost[i,] <- c(plotnames[i],rmse, mape)
}
YS_statistics_mboost <- cbind(YS_statistics_mboost, MediaVariaveis,
                              "GW Test MSE" = gw, "GW Test MAPE" = gw2)

YS_top_vars <- data.frame(
  top_n(YS_m2_vars, 15)[1:15,1],
  top_n(YS_m2_vars, 15)[1:15,3],
  top_n(YS_m3_vars, 15)[1:15,1],
  top_n(YS_m3_vars, 15)[1:15,3],
  top_n(YS_m4_vars, 15)[1:15,1],
  top_n(YS_m4_vars, 15)[1:15,3],
  top_n(YS_m5_vars, 15)[1:15,1],
  top_n(YS_m5_vars, 15)[1:15,3],
  top_n(YS_m6_vars, 15)[1:15,1],
  top_n(YS_m6_vars, 15)[1:15,3]
)
colnames(YS_top_vars) <- c("Model 2", "Relative 2", "Model 3", "Relative 3", "Model 4", "Relative 4",
                           "Model 5","Relative 5","Model 6","Relative 6")
beep(11)
for (i in 1:6) { 
  names <- paste0("Output/Plots/RW Forecast/Short H/",plotnames[i]," - h",h,".jpeg")
  jpeg(filename = names, width = 1920, height = 1080)
  
  print(ggplot(YS_results[YS_results$model == models[i],])+
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

#save.image(file = "Scripts/RW Forecast/Short/Objects/h12-YS-boosting05-env.RData")

#### Linear ####
load("Scripts/RW Forecast/Short/Objects/h3-YS-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/h6-YS-linear05-env.RData")
load("Scripts/RW Forecast/Short/Objects/h12-YS-linear05-env.RData")

#h = 3
#h = 6
h = 12

YS_m1_data <- cbind(read.csv2("Output/Data/Short/YS.csv"), read.csv2("Output/Data/Short/YS_lag.csv"))
YS_m2_data <- cbind(YS_m1_data, read.csv2("Output/Data/Short/m2_vars.csv"))
YS_m3_data <- cbind(YS_m1_data, read.csv2("Output/Data/Short/m3_vars.csv"))
YS_m4_data <- cbind(YS_m1_data, read.csv2("Output/Data/Short/m4_vars.csv"))
YS_m5_data <- cbind(YS_m1_data, read.csv2("Output/Data/Short/m5_vars.csv"))
YS_m6_data <- cbind(YS_m1_data, read.csv2("Output/Data/Short/m6_vars.csv"))
YS_m6_data[,373:492] <- log(YS_m6_data[,373:492])

YS_m1_fc <- rwf_lm2(YS_m1_data,0.5, norm = T, h = h)
YS_m2_fc <- rwf_lm2(YS_m2_data,0.5, norm = T, h = h)
YS_m3_fc <- rwf_lm2(YS_m3_data,0.5, norm = T, h = h)
YS_m4_fc <- rwf_lm2(YS_m4_data,0.5, norm = T, h = h)
YS_m5_fc <- rwf_lm2(YS_m5_data,0.5, norm = T, h = h)
YS_m6_fc <- rwf_lm2(YS_m6_data,0.5, norm = T, h = h)

data <- read.csv2("Output/Data/Short/short_expanded.csv") %>% select(Period)
date <- ym(str_replace(data$Period, "M", ""))
date <- date[-idx]
date <- date[-(1:(h-1))]

YS_results <- tibble(date, Y = test_Y, YS_m1_fc, YS_m2_fc, YS_m3_fc, YS_m4_fc, YS_m5_fc, YS_m6_fc)
YS_results <- YS_results %>% gather(YS_m1_fc, YS_m2_fc, YS_m3_fc, YS_m4_fc, YS_m5_fc, YS_m6_fc,
                                    key = "model", value = "fc")

models <- c("YS_m1_fc", "YS_m2_fc", "YS_m3_fc", "YS_m4_fc", "YS_m5_fc", "YS_m6_fc")
plotnames <- c("YS - Model 1 - Linear Model", "YS - Model 2 - Linear Model",
               "YS - Model 3 - Linear Model", "YS - Model 4 - Linear Model",
               "YS - Model 5 - Linear Model", "YS - Model 6 - Linear Model")

gw <- c(NA)
gw2 <- c(NA)
for (i in 2:6) {
  m1 <- YS_results %>% filter(model == models[1])
  mn <- YS_results %>% filter(model == models[i])
  gw[i] <- gw.test(m1$fc, mn$fc, length(test_Y), 545-(h-1), 1, alternative = "two.sided")$p.value
  gw2[i] <- gw.test(m1$fc, mn$fc, length(test_Y), 545-(h-1), 1, alternative = "two.sided", power = 1)$p.value
}

YS_statistics_lm <- data.frame(Model = NA, RMSE = NA, MAPE = NA)
for (i in 1:6) {
  temp <- filter(YS_results, model == models[i])
  rmse <- sqrt(sum((temp$fc - temp$Y)^2)/nrow(temp))
  mape <- (100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y))
  #mape <-(100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y)[abs((temp$Y - temp$fc)/temp$Y) < 10000000]
  YS_statistics_lm[i,] <- c(plotnames[i],rmse, mape)
}
YS_statistics_lm <- cbind(YS_statistics_lm, "GW Test MSE" = gw, "GW Test MAPE" = gw2)
beep(11)
for (i in 1:6) { 
  names <- paste0("Output/Plots/RW Forecast/Short H/",plotnames[i]," - h",h,".jpeg")
  jpeg(filename = names, width = 1920, height = 1080)
  
  print(ggplot(YS_results[YS_results$model == models[i],])+
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

#save.image(file = "Scripts/RW Forecast/Short/Objects/h12-YS-linear05-env.RData")
