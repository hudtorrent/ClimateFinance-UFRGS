library(mboost)
library(tidyverse)
library(stringr)
library(gridExtra)

##### Variables #####

YM <- read.csv2("Output/Data/Short/YM.csv") %>% as.matrix()
YM_lag <- read.csv2("Output/Data/Short/YM_lag.csv")
m2_vars <- read.csv2("Output/Data/Short/m2_vars.csv")
m3_vars <- read.csv2("Output/Data/Short/m3_vars.csv")
m4_vars <- read.csv2("Output/Data/Short/m4_vars.csv")
m5_vars <- read.csv2("Output/Data/Short/m5_vars.csv")
m6_vars <- read.csv2("Output/Data/Short/m6_vars.csv")

##### Model 1 #####

model <- glmboost(YM ~., data = YM_lag,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m1_YM <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")


##### Model 2 #####

regs <- cbind(YM_lag, m2_vars)

model <- glmboost(YM ~., data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m2_YM <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")


##### Model 3 #####

regs <- cbind(YM_lag, m3_vars)

model <- glmboost(YM ~., data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m3_YM <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")


##### Model 4 #####

regs <- cbind(YM_lag, m4_vars)

model <- glmboost(YM ~., data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m4_YM <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")


##### Model 5 #####

regs <- cbind(YM_lag, m5_vars)

model <- glmboost(YM ~., data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m5_YM <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")


##### Model 6 #####

regs <- cbind(YM_lag, m6_vars)

model <- glmboost(YM ~., data = regs,
                  center = TRUE,
                  family = Gaussian(),
                  control = boost_control(mstop = 1000, nu = 0.1))

AIC = AIC(model, method = "corrected" , df = "actset")
(m = mstop(AIC))
plot(AIC)
m6_YM <- model[m]

coef(model,which = "")
plot(model,off2int = T)

varimp(model)
plot(varimp(model), blorder = "importance")

##### Compare ####

jpeg(filename = "Output/Plots/Basic Models/glmboost_short_YM.jpeg",
     width = 1920,
     height = 1080)

grid.arrange(plot(varimp(m1_YM), blorder = "importance", main = "YM - Model 1"),
             plot(varimp(m2_YM), blorder = "importance", main = "YM - Model 2"),
             plot(varimp(m3_YM), blorder = "importance", main = "YM - Model 3"),
             plot(varimp(m4_YM), blorder = "importance", main = "YM - Model 4"),
             plot(varimp(m5_YM), blorder = "importance", main = "YM - Model 5"),
             plot(varimp(m6_YM), blorder = "importance", main = "YM - Model 6"),
             nrow = 2, ncol = 3, heights = c(8, 8), widths = c(12, 12, 12))
dev.off()