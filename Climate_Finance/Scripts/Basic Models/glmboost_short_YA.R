library(mboost)
library(tidyverse)
library(stringr)
library(gridExtra)

##### Variables #####

YA <- read.csv2("Output/Data/Short/YA.csv") %>% as.matrix()
YA_lag <- read.csv2("Output/Data/Short/YA_lag.csv")
m2_vars <- read.csv2("Output/Data/Short/m2_vars.csv")
m3_vars <- read.csv2("Output/Data/Short/m3_vars.csv")
m4_vars <- read.csv2("Output/Data/Short/m4_vars.csv")
m5_vars <- read.csv2("Output/Data/Short/m5_vars.csv")
m6_vars <- read.csv2("Output/Data/Short/m6_vars.csv")

##### Model 1 #####

model <- glmboost(YA ~., data = YA_lag,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m1_YA <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")


##### Model 2 #####

regs <- cbind(YA_lag, m2_vars)

model <- glmboost(YA ~., data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m2_YA <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")


##### Model 3 #####

regs <- cbind(YA_lag, m3_vars)

model <- glmboost(YA ~., data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m3_YA <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")


##### Model 4 #####

regs <- cbind(YA_lag, m4_vars)

model <- glmboost(YA ~., data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m4_YA <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")


##### Model 5 #####

regs <- cbind(YA_lag, m5_vars)

model <- glmboost(YA ~., data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m5_YA <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")


##### Model 6 #####

regs <- cbind(YA_lag, m6_vars)

model <- glmboost(YA ~., data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m6_YA <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")

##### Compare ####

jpeg(filename = "Output/Plots/Basic Models/glmboost_short_YA.jpeg",
     width = 1920,
     height = 1080)

grid.arrange(plot(varimp(m1_YA), blorder = "importance", main = "YA - Model 1"),
             plot(varimp(m2_YA), blorder = "importance", main = "YA - Model 2"),
             plot(varimp(m3_YA), blorder = "importance", main = "YA - Model 3"),
             plot(varimp(m4_YA), blorder = "importance", main = "YA - Model 4"),
             plot(varimp(m5_YA), blorder = "importance", main = "YA - Model 5"),
             plot(varimp(m6_YA), blorder = "importance", main = "YA - Model 6"),
             nrow = 2, ncol = 3, heights = c(8, 8), widths = c(12, 12, 12))
dev.off()