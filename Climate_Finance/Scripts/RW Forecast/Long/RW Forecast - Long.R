source("Scripts/functions.R")

#### Boosting ####
load("Scripts/RW Forecast/Long/Objects/boosting05-env.RData")
#load("Scripts/RW Forecast/Long/Objects/boosting-env.RData")

m1_data <- read.csv2("Output/Data/Long/m1_data.csv")
m2_data <- read.csv2("Output/Data/Long/m2_data.csv")
m3_data <- read.csv2("Output/Data/Long/m3_data.csv")
m4_data <- read.csv2("Output/Data/Long/m4_data.csv")
m5_data <- read.csv2("Output/Data/Long/m5_data.csv")
m6_data <- read.csv2("Output/Data/Long/m6_data.csv")

m1_fc <- rwf_mboost(m1_data,0.5)
m1_vars <- vars_freq(variables)
m1_vars_avg <- variables_avg

m2_fc <- rwf_mboost(m2_data,0.5)
m2_vars <- vars_freq(variables)
m2_vars_avg <- variables_avg

m3_fc <- rwf_mboost(m3_data,0.5)
m3_vars <- vars_freq(variables)
m3_vars_avg <- variables_avg

m4_fc <- rwf_mboost(m4_data,0.5)
m4_vars <- vars_freq(variables)
m4_vars_avg <- variables_avg

m5_fc <- rwf_mboost(m5_data,0.5)
m5_vars <- vars_freq(variables)
m5_vars_avg <- variables_avg

m6_fc <- rwf_mboost(m6_data,0.5)
m6_vars <- vars_freq(variables)
m6_vars_avg <- variables_avg

data <- read.csv2("Output/Data/Long/long_expanded.csv") %>% select(Date)
date <- data[-idx,]

results <- tibble(date = my(date), Y = test_Y, m1_fc, m2_fc, m3_fc, m4_fc, m5_fc, m6_fc)
results <- results %>% gather(m1_fc, m2_fc, m3_fc, m4_fc, m5_fc, m6_fc, key = "model", value = "fc")

models <- c("m1_fc", "m2_fc", "m3_fc", "m4_fc", "m5_fc", "m6_fc")
plotnames <- c("Model 1 - mboost", "Model 2 - mboost",
               "Model 3 - mboost", "Model 4 - mboost",
               "Model 5 - mboost", "Model 6 - mboost")

gw <- c(NA)
gw2 <- c(NA)
for (i in 2:6) {
  m1 <- results %>% filter(model == "m1_fc")
  mn <- results %>% filter(model == models[i])
  gw[i] <- gw.test(m1$fc, mn$fc, 360, 721, 1, alternative = "two.sided")$p.value
  gw2[i] <- gw.test(m1$fc, mn$fc, 360, 721, 1, alternative = "two.sided", power = 1)$p.value
}

statistics_mboost <- data.frame(Model = NA, RMSE = NA, MAPE = NA)
MediaVariaveis = c(m1_vars_avg, m2_vars_avg, m3_vars_avg, m4_vars_avg, m5_vars_avg, m6_vars_avg)
for (i in 1:6) {
  temp <- filter(results, model == models[i])
  rmse <- sqrt(sum((temp$fc - temp$Y)^2)/nrow(temp))
  mape <- (100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y))
  statistics_mboost[i,] <- c(plotnames[i],rmse, mape)
}
statistics_mboost <- cbind(statistics_mboost, MediaVariaveis, "GW Test MSE" = gw, "GW Test MAPE" = gw2)

top_vars <- data.frame(
  top_n(m2_vars, 15)[1],
  top_n(m2_vars, 15)[3],
  top_n(m3_vars, 15)[1],
  top_n(m3_vars, 15)[3],
  top_n(m4_vars, 15)[1],
  top_n(m4_vars, 15)[3],
  top_n(m5_vars, 15)[1],
  top_n(m5_vars, 15)[3],
  top_n(m6_vars, 15)[1],
  top_n(m6_vars, 15)[3])
colnames(top_vars) <- c("Model 2", "Relative 2", "Model 3", "Relative 3", "Model 4", "Relative 4",
                        "Model 5","Relative 5","Model 6","Relative 6")

for (i in 1:6) { 
  names <- paste0("Output/Plots/RW Forecast/Long/",plotnames[i],".jpeg")
  jpeg(filename = names, width = 1920, height = 1080)
  
  print(ggplot(results[results$model == models[i],])+
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

#save.image(file = "Scripts/RW Forecast/Long/Objects/boosting05-env.RData")

#### Linear ####
load("Scripts/RW Forecast/Long/Objects/linear05-env.RData")
#load("Scripts/RW Forecast/Long/Objects/linear-env.RData")

m1_data <- read.csv2("Output/Data/Long/m1_data.csv")
m2_data <- read.csv2("Output/Data/Long/m2_data.csv")
m3_data <- read.csv2("Output/Data/Long/m3_data.csv")
m4_data <- read.csv2("Output/Data/Long/m4_data.csv")
m5_data <- read.csv2("Output/Data/Long/m5_data.csv")
m6_data <- read.csv2("Output/Data/Long/m6_data.csv")

m1_fc <- rwf_lm(m1_data,0.5)
m2_fc <- rwf_lm(m2_data,0.5)
m3_fc <- rwf_lm(m3_data,0.5)
m4_fc <- rwf_lm(m4_data,0.5)
m5_fc <- rwf_lm(m5_data,0.5)
m6_fc <- rwf_lm(m6_data,0.5)

data <- read.csv2("Output/Data/Long/long_expanded.csv") %>% select(Date)
date <- data[-idx,]

results <- tibble(date = my(date), Y = test_Y, m1_fc, m2_fc, m3_fc, m4_fc, m5_fc, m6_fc)
results <- results %>% gather(m1_fc, m2_fc, m3_fc, m4_fc, m5_fc, m6_fc, key = "model", value = "fc")

models <- c("m1_fc", "m2_fc", "m3_fc", "m4_fc", "m5_fc", "m6_fc")
plotnames <- c("Model 1 - Linear Model", "Model 2 - Linear Model",
               "Model 3 - Linear Model", "Model 4 - Linear Model",
               "Model 5 - Linear Model", "Model 6 - Linear Model")

gw <- c(NA)
gw2 <- c(NA)
for (i in 2:6) {
  m1 <- results %>% filter(model == "m1_fc")
  mn <- results %>% filter(model == models[i])
  gw[i] <- gw.test(m1$fc, mn$fc, 360, 721, 1, alternative = "two.sided")$p.value
  gw2[i] <- gw.test(m1$fc, mn$fc, 360, 721, 1, alternative = "two.sided", power = 1)$p.value
}

statistics_lm <- data.frame(Model = NA, RMSE = NA, MAPE = NA)
for (i in 1:6) {
  temp <- filter(results, model == models[i])
  rmse <- sqrt(sum((temp$fc - temp$Y)^2)/nrow(temp))
  mape <- (100/nrow(temp))*sum(abs((temp$Y - temp$fc)/temp$Y))
  statistics_lm[i,] <- c(plotnames[i],rmse, mape)
}
statistics_lm <- cbind(statistics_lm, "GW Test MSE" = gw, "GW Test MAPE" = gw2)

for (i in 1:6) { 
  names <- paste0("Output/Plots/RW Forecast/Long/",plotnames[i],".jpeg")
  jpeg(filename = names,
       width = 1920,
       height = 1080)
  
  print(ggplot(results[results$model == models[i],])+
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

#save.image(file = "Scripts/RW Forecast/Long/Objects/linear05-env.RData")