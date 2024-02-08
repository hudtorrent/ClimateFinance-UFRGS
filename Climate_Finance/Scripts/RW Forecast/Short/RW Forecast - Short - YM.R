source("Scripts/functions.R")

#### Boosting ####
load("Scripts/RW Forecast/Short/Objects/YM-boosting05-env.RData")
#load("Scripts/RW Forecast/Short/Objects/YM-boosting-env.RData")

YM_m1_data <- cbind(read.csv2("Output/Data/Short/YM.csv"), read.csv2("Output/Data/Short/YM_lag.csv"))
YM_m2_data <- cbind(YM_m1_data, read.csv2("Output/Data/Short/m2_vars.csv"))
YM_m3_data <- cbind(YM_m1_data, read.csv2("Output/Data/Short/m3_vars.csv"))
YM_m4_data <- cbind(YM_m1_data, read.csv2("Output/Data/Short/m4_vars.csv"))
YM_m5_data <- cbind(YM_m1_data, read.csv2("Output/Data/Short/m5_vars.csv"))
YM_m6_data <- cbind(YM_m1_data, read.csv2("Output/Data/Short/m6_vars.csv"))

YM_m1_fc <- rwf_mboost(YM_m1_data,0.5)
YM_m1_vars <- vars_freq(variables)
YM_m1_vars_avg <- variables_avg

YM_m2_fc <- rwf_mboost(YM_m2_data,0.5)
YM_m2_vars <- vars_freq(variables)
YM_m2_vars_avg <- variables_avg

YM_m3_fc <- rwf_mboost(YM_m3_data,0.5)
YM_m3_vars <- vars_freq(variables)
YM_m3_vars_avg <- variables_avg

YM_m4_fc <- rwf_mboost(YM_m4_data,0.5)
YM_m4_vars <- vars_freq(variables)
YM_m4_vars_avg <- variables_avg

YM_m5_fc <- rwf_mboost(YM_m5_data,0.5)
YM_m5_vars <- vars_freq(variables)
YM_m5_vars_avg <- variables_avg

YM_m6_fc <- rwf_mboost(YM_m6_data,0.5)
YM_m6_vars <- vars_freq(variables)
YM_m6_vars_avg <- variables_avg

data <- read.csv2("Output/Data/Short/short_expanded.csv") %>% select(Period)
date <- ym(str_replace(data$Period, "M", ""))
date <- date[-idx]

YM_results <- tibble(date, Y = test_Y, YM_m1_fc, YM_m2_fc, YM_m3_fc, YM_m4_fc, YM_m5_fc, YM_m6_fc)
YM_results <- YM_results %>% gather(YM_m1_fc, YM_m2_fc, YM_m3_fc, YM_m4_fc, YM_m5_fc, YM_m6_fc,
                                    key = "model", value = "fc")

models <- c("YM_m1_fc", "YM_m2_fc", "YM_m3_fc", "YM_m4_fc", "YM_m5_fc", "YM_m6_fc")
plotnames <- c("YM - Model 1 - mboost", "YM - Model 2 - mboost",
               "YM - Model 3 - mboost", "YM - Model 4 - mboost",
               "YM - Model 5 - mboost", "YM - Model 6 - mboost")

gw <- c(NA)
gw2 <- c(NA)
for (i in 2:6) {
  m1 <- YM_results %>% filter(model == models[1])
  mn <- YM_results %>% filter(model == models[i])
  gw[i] <- gw.test(m1$fc, mn$fc, 272, 545, 1, alternative = "two.sided")$p.value
  gw2[i] <- gw.test(m1$fc, mn$fc, 360, 721, 1, alternative = "two.sided", power = 1)$p.value
}

YM_statistics_mboost <- data.frame(Model = NA, RMSE = NA, MAPE = NA)
MediaVariaveis = c(YM_m1_vars_avg, YM_m2_vars_avg, YM_m3_vars_avg,
                   YM_m4_vars_avg, YM_m5_vars_avg, YM_m6_vars_avg)
for (i in 1:6) {
  temp <- filter(YM_results, model == models[i])
  rmse <- sqrt(sum((temp$fc - temp$Y)^2)/nrow(temp))
  mape <- (100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y))
  YM_statistics_mboost[i,] <- c(plotnames[i],rmse, mape)
}
YM_statistics_mboost <- cbind(YM_statistics_mboost, MediaVariaveis,
                              "GW Test MSE" = gw, "GW Test MAPE" = gw2)

YM_top_vars <- data.frame(
  top_n(YM_m2_vars, 15)[1:15,1],
  top_n(YM_m2_vars, 15)[1:15,3],
  top_n(YM_m3_vars, 15)[1:15,1],
  top_n(YM_m3_vars, 15)[1:15,3],
  top_n(YM_m4_vars, 15)[1:15,1],
  top_n(YM_m4_vars, 15)[1:15,3],
  top_n(YM_m5_vars, 15)[1:15,1],
  top_n(YM_m5_vars, 15)[1:15,3],
  top_n(YM_m6_vars, 15)[1:15,1],
  top_n(YM_m6_vars, 15)[1:15,3]
)
colnames(YM_top_vars) <- c("Model 2", "Relative 2", "Model 3", "Relative 3", "Model 4", "Relative 4",
                           "Model 5","Relative 5","Model 6","Relative 6")

for (i in 1:6) { 
  names <- paste0("Output/Plots/RW Forecast/Short/",plotnames[i],".jpeg")
  jpeg(filename = names, width = 1920, height = 1080)
  
  print(ggplot(YM_results[YM_results$model == models[i],])+
          geom_line(aes(x = date, y = fc, color = "Forecast (h=1)"), size = 1)+
          geom_line(aes(x = date, y = Y, color = "Y"), size = 1)+
          scale_color_manual("", values = c("Forecast (h=1)" = "red", "Y" = "black"))+
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

#save.image(file = "Scripts/RW Forecast/Short/Objects/YM-boosting05-env.RData")

#### Linear ####
load("Scripts/RW Forecast/Short/Objects/YM-linear05-env.RData")
#load("Scripts/RW Forecast/Short/Objects/YM-linear-env.RData")

YM_m1_data <- cbind(read.csv2("Output/Data/Short/YM.csv"), read.csv2("Output/Data/Short/YM_lag.csv"))
YM_m2_data <- cbind(YM_m1_data, read.csv2("Output/Data/Short/m2_vars.csv"))
YM_m3_data <- cbind(YM_m1_data, read.csv2("Output/Data/Short/m3_vars.csv"))
YM_m4_data <- cbind(YM_m1_data, read.csv2("Output/Data/Short/m4_vars.csv"))
YM_m5_data <- cbind(YM_m1_data, read.csv2("Output/Data/Short/m5_vars.csv"))
YM_m6_data <- cbind(YM_m1_data, read.csv2("Output/Data/Short/m6_vars.csv"))

YM_m1_fc <- rwf_lm(YM_m1_data,0.5)
YM_m2_fc <- rwf_lm(YM_m2_data,0.5)
YM_m3_fc <- rwf_lm(YM_m3_data,0.5)
YM_m4_fc <- rwf_lm(YM_m4_data,0.5)
YM_m5_fc <- rwf_lm(YM_m5_data,0.5)
YM_m6_fc <- rwf_lm(YM_m6_data,0.5)

data <- read.csv2("Output/Data/Short/short_expanded.csv") %>% select(Period)
date <- ym(str_replace(data$Period, "M", ""))
date <- date[-idx]

YM_results <- tibble(date, Y = test_Y, YM_m1_fc, YM_m2_fc, YM_m3_fc, YM_m4_fc, YM_m5_fc, YM_m6_fc)
YM_results <- YM_results %>% gather(YM_m1_fc, YM_m2_fc, YM_m3_fc, YM_m4_fc, YM_m5_fc, YM_m6_fc,
                                    key = "model", value = "fc")

models <- c("YM_m1_fc", "YM_m2_fc", "YM_m3_fc", "YM_m4_fc", "YM_m5_fc", "YM_m6_fc")
plotnames <- c("YM - Model 1 - Linear Model", "YM - Model 2 - Linear Model",
               "YM - Model 3 - Linear Model", "YM - Model 4 - Linear Model",
               "YM - Model 5 - Linear Model", "YM - Model 6 - Linear Model")

gw <- c(NA)
gw2 <- c(NA)
for (i in 2:6) {
  m1 <- YM_results %>% filter(model == models[1])
  mn <- YM_results %>% filter(model == models[i])
  gw[i] <- gw.test(m1$fc, mn$fc, 272, 545, 1, alternative = "two.sided")$p.value
  gw2[i] <- gw.test(m1$fc, mn$fc, 360, 721, 1, alternative = "two.sided", power = 1)$p.value
}

YM_statistics_lm <- data.frame(Model = NA, RMSE = NA, MAPE = NA)
for (i in 1:6) {
  temp <- filter(YM_results, model == models[i])
  rmse <- sqrt(sum((temp$fc - temp$Y)^2)/nrow(temp))
  mape <- (100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y))
  YM_statistics_lm[i,] <- c(plotnames[i],rmse, mape)
}
YM_statistics_lm <- cbind(YM_statistics_lm, "GW Test MSE" = gw, "GW Test MAPE" = gw2)

for (i in 1:6) { 
  names <- paste0("Output/Plots/RW Forecast/Short/",plotnames[i],".jpeg")
  jpeg(filename = names, width = 1920, height = 1080)
  
  print(ggplot(YM_results[YM_results$model == models[i],])+
          geom_line(aes(x = date, y = fc, color = "Forecast (h=1)"), size = 1)+
          geom_line(aes(x = date, y = Y, color = "Y"), size = 1)+
          scale_color_manual("", values = c("Forecast (h=1)" = "red", "Y" = "black"))+
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

#save.image(file = "Scripts/RW Forecast/Short/Objects/YM-linear05-env.RData")