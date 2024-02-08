library(mboost)
library(tidyverse)
##### Model 1 #####
m1_data <- read.csv2("Output/Data/Long/m1_data.csv")
Y <- m1_data %>% select(Y) %>% as.matrix()
regs <- m1_data %>% select(-Y)

model <- glmboost(Y ~.,
                  data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m1 <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")

##### Model 2 #####
m2_data <- read.csv2("Output/Data/Long/m2_data.csv")
Y <- m2_data %>% select(Y) %>% as.matrix()
regs <- m2_data %>% select(-Y)

model <- glmboost(Y ~.,
                  data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m2 <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")

##### Model 3 #####
m3_data <- read.csv2("Output/Data/Long/m3_data.csv")
Y <- m3_data %>% select(Y) %>% as.matrix()
regs <- m3_data %>% select(-Y)

model <- glmboost(Y ~.,
                  data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m3 <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")

##### Model 4 #####
m4_data <- read.csv2("Output/Data/Long/m4_data.csv")
Y <- m4_data %>% select(Y) %>% as.matrix()
regs <- m4_data %>% select(-Y)

model <- glmboost(Y ~.,
                  data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m4 <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")

##### Model 5 #####
m5_data <- read.csv2("Output/Data/Long/m5_data.csv")
Y <- m5_data %>% select(Y) %>% as.matrix()
regs <- m5_data %>% select(-Y)

model <- glmboost(Y ~.,
                  data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m5 <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")

##### Model 6 #####
m6_data <- read.csv2("Output/Data/Long/m6_data.csv")
Y <- m6_data %>% select(Y) %>% as.matrix()
regs <- m6_data %>% select(-Y)

model <- glmboost(Y ~.,
                  data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m6 <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")

##### Comparison #####

jpeg(filename = "Output/Plots/Basic Models/glmboost_long.jpeg",
     width = 1920,
     height = 1080)

grid.arrange(plot(varimp(m1), blorder = "importance", main = "Model 1"),
             plot(varimp(m2), blorder = "importance", main = "Model 2"),
             plot(varimp(m3), blorder = "importance", main = "Model 3"),
             plot(varimp(m4), blorder = "importance", main = "Model 4"),
             plot(varimp(m5), blorder = "importance", main = "Model 5"),
             plot(varimp(m6), blorder = "importance", main = "Model 6"),
             nrow = 2, ncol = 3, heights = c(8, 8), widths = c(12, 12, 12))
dev.off()