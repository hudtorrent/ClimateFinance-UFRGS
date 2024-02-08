######  Packs #####

library(tidyverse)
library(forecast)
library(mboost)
library(lubridate)
library(scales)
library(urca)
library(beepr)
library(MCS)

##### Stationarize #####

st_series <- function(data){
  not_st <- c()
  not_st1 <- c()
  diff_series <- data.frame(rep(0, nrow(data)))
  
  for (i in 1:ncol(data)) {
    test <- ur.df(y = data[,i], type = "trend", lags = 15, selectlags = "BIC")
    if(test@teststat[1] > test@cval[1,2]){
      cat(names(data)[i]," - Not Stationary \n")
      not_st[i] <- names(data)[i]
      }else{cat(names(data)[i], " - Stationary \n")}
  }
  not_st <- na.omit(not_st)[1:sum(!is.na(not_st))]
  
  if(length(not_st) > 0){
    cat("\n\nFirst Difference \n")
    data2 <- select(data, not_st)
    for (j in 1:ncol(data2)) {
      test <- ur.df(y = diff(data2[,j]), type = "trend", lags = 15, selectlags = "BIC")
      if(test@teststat[1] > test@cval[1,2]){
        cat(names(data2)[j]," D1 - Not Stationary \n")
        not_st1[j] <- names(data2)[j]
      }else{cat(names(data2)[j], " D1 - Stationary \n")}
      diff_series[,j] <- c(NA, diff(data2[,j]))
    }
    not_st1 <- na.omit(not_st1)[1:sum(!is.na(not_st1))]
  }
  not_st <<- not_st
  not_st1 <<- not_st1
  colnames(diff_series) <- not_st
  diff_series <<- diff_series
}

##### Add Lags #####
add_lags <- function(data, lags){
  library(tidyverse)
  
  data <- as_tibble(data)
  n <- 1:lags
  output <- data.frame(idx = 1:nrow(tail(data, -lags)))
  
  for (j in 1:ncol(data)) {
    
    x = data[,j]
    
    temp <- map(.x = n, .f = lag, x = x)
    
    df <- data.frame(x)
    
    sufix <- paste("_L", n, sep ="")
    var_names <- c(names(df), paste0(names(df), sufix))
    
    for (i in 1:lags) {
      df[,i+1] <- temp[[i]]
    }
    
    df2 <- tail(df, -lags)
    colnames(df2) <- var_names
    output <- cbind(output, df2)
  }
  return(select(output, -idx))
}

##### Short Sets #####
short_sets <- function(data){
  nome1 <- paste0("Output/Data/Short/", names(data)[1], "_m1_data.csv")
  nome2 <- paste0("Output/Data/Short/", names(data)[1], "_m2_data.csv")
  nome3 <- paste0("Output/Data/Short/", names(data)[1], "_m3_data.csv")
  nome4 <- paste0("Output/Data/Short/", names(data)[1], "_m4_data.csv")
  nome5 <- paste0("Output/Data/Short/", names(data)[1], "_m5_data.csv")
  nome6 <- paste0("Output/Data/Short/", names(data)[1], "_m6_data.csv")
  
  temp1 <- data
  temp2 <- cbind(data, m2_data)
  temp3 <- cbind(data, m3_data)
  temp4 <- cbind(data, m4_data)
  temp5 <- cbind(data, m5_data)
  temp6 <- cbind(data, m6_data)
  
  write.csv2(temp1, file = nome1, row.names = F)
  write.csv2(temp2, file = nome2, row.names = F)
  write.csv2(temp3, file = nome3, row.names = F)
  write.csv2(temp4, file = nome4, row.names = F)
  write.csv2(temp5, file = nome5, row.names = F)
  write.csv2(temp6, file = nome6, row.names = F)
}

##### Variables Frequency #####

vars_freq <- function(data){
  df <- c()
  for (i in 1:length(test_Y)) {
    temp <- names(data[[i]])
    temp <- temp[temp != "FC" & temp != "(Intercept)"]
    df <- c(df, temp)
  }
  return(arrange(count(tibble(vars = df), vars),-n) %>% mutate(relative = n/length(test_Y)))
}

##### RW Forecast Boosting #####

rwf_mboost <- function(data, n, md = 2){
  
  library(tidyverse)
  library(forecast)
  library(mboost)
  library(lubridate)
  library(scales)
  
  vars <- names(data[-1])
  ar_vars <- vars[!str_detect(vars, "_L")]
  
  idx <<- 1:ceiling(nrow(data)*n)
  
  test_Y <<- data[-idx, 1]
  test_regs <- data[-idx, -1]
  train_Y <- data[idx, 1]
  train_regs <- data[idx, -1]
  
  fc <- c()
  iter <- c()
  selected_vars <- list()
  
  temp_Y <- train_Y
  temp_regs <- train_regs
  
  for (i in 1:length(test_Y)) {
    
    model <- glmboost(temp_Y ~.,
                      data = temp_regs,
                      center = TRUE,
                      family = Gaussian(),
                      control = boost_control(mstop = 1000, nu = 0.1))
    AIC = AIC(model, method = "corrected" , df = "actset")
    m = mstop(AIC)
    model <- model[m]
    iter[i] <- m
    cat("Forecast:",i,"\n","Optimal M:",m, "\n")
    
    ar_set <- select(temp_regs, ar_vars)
    ar_fc <- ar_set[1,]*0
    
    if (ncol(ar_set)>0) {
      for (j in 1:ncol(ar_set)) {
        ar_fc[1,j] <- predict(auto.arima(ar_set[,j], max.p = 1,max.q = 0, max.d = md, allowdrift = F),
                              n.ahead = 1)$pred[1]
      }
    }
    
    fc_set <- cbind(select(test_regs[i,], -ar_vars), ar_fc)
    fc[i] <- predict(model, fc_set)
    selected_vars[[i]] <- c(FC = i, coef(model))
    
    temp_Y <- c(train_Y[-(1:i)], test_Y[1:i])
    temp_regs <- train_regs[-(1:i),] %>% rbind(test_regs[1:i,])
  }
  variables <<- selected_vars
  variables_avg <<- mean(lengths(variables)-2)
  return(fc)
}

##### RW Forecast Linear #####

rwf_lm <- function(data, n, md = 2){
  
  library(tidyverse)
  library(forecast)
  library(mboost)
  library(lubridate)
  library(scales)
  
  vars <- names(data[-1])
  ar_vars <- vars[!str_detect(vars, "_L")]
  
  idx <<- 1:ceiling(nrow(data)*n)
  
  test_Y <<- data[-idx, 1]
  test_regs <- data[-idx, -1]
  train_Y <- data[idx, 1]
  train_regs <- data[idx, -1]
  
  fc <- c()
  iter <- c()
  selected_vars <- list()
  
  temp_Y <- train_Y
  temp_regs <- train_regs
  
  for (i in 1:length(test_Y)) {
    
    model <- lm(temp_Y ~., data = temp_regs)
    cat("Forecast:",i,"\n")
    
    ar_set <- select(temp_regs, ar_vars)
    ar_fc <- ar_set[1,]*NA
    
    if (ncol(ar_set)>0) {
      for (j in 1:ncol(ar_set)) {
        ar_fc[1,j] <- predict(auto.arima(ar_set[,j], max.p = 1,max.q = 0, max.d = md, allowdrift = F),
                              n.ahead = 1)$pred[1]
      }
    }
    
    fc_set <- cbind(select(test_regs[i,], -ar_vars), ar_fc)
    fc[i] <- predict(model, fc_set)
    
    temp_Y <- c(train_Y[-(1:i)], test_Y[1:i])
    temp_regs <- train_regs[-(1:i),] %>% rbind(test_regs[1:i,])
  }
  return(fc)
}

##### RW Forecast Boosting 2 #####

rwf_mboost2 <- function(data, n, norm = F, h = 1){
  
  library(tidyverse)
  library(mboost)
  library(lubridate)
  library(scales)
  options(scipen = 999)
  
  #vars <- names(data[-1])
  #response <- names(data[1])
  #h1_vars <- c(response, vars[str_detect(vars, "_L")])
  #h1_data <- data %>% select(h1_vars)
  
  h1_data <- data
  
  if(h > 1){
    temp_data <- h1_data
    for (k in 0:(nrow(h1_data)-h)) {
      temp_data[h+k,1] <- sum(h1_data[(1+k):(h+k),1])
    }
    temp_data <- temp_data[-(1:(h-1)),]
    h1_data <- temp_data
  }
  cat("\n", "Forecasts for h =",h,"\n","\n")
  
  idx <<- 1:ceiling(nrow(h1_data)*n)
  
  test_Y <<- h1_data[-idx, 1]
  test_regs <- h1_data[-idx, -1]
  train_Y <- h1_data[idx, 1]
  train_regs <- h1_data[idx, -1]
  
  fc <- c()
  iter <- c()
  selected_vars <- list()
  
  temp_Y <- train_Y
  temp_regs <- train_regs
  
  for (i in 1:length(test_Y)) {
    
    fc_regs <- test_regs[i,]
    ct = T
    
    if(norm){
      means <- c()
      sds <- c()
      for (j in 1:ncol(temp_regs)) {
        means[j] <- mean(temp_regs[,j])
        sds[j] <- sd(temp_regs[,j])
        temp_regs[,j] <- (temp_regs[,j] - means[j])/sds[j]
      }
      fc_regs <- (fc_regs - means)/sds
      ct = F
    }
    
    model <- glmboost(temp_Y ~.,
                      data = temp_regs,
                      center = ct,
                      family = Gaussian(),
                      control = boost_control(mstop = 1000, nu = 0.1))
    AIC = AIC(model, method = "corrected" , df = "actset")
    m = mstop(AIC)
    model <- model[m]
    iter[i] <- m
    cat("Forecast:",i,"\n","Optimal M:",m, "\n")
    
    fc[i] <- predict(model, fc_regs) 
    selected_vars[[i]] <- c(FC = i, coef(model))
    
    temp_Y <- c(train_Y[-(1:i)], test_Y[1:i])
    temp_regs <- train_regs[-(1:i),] %>% rbind(test_regs[1:i,])
  }
  variables <<- selected_vars
  variables_avg <<- mean(lengths(variables) - ifelse(ct, 2, 1))
  return(fc)
}

##### RW Forecast Linear 2 #####

rwf_lm2 <- function(data, n, norm = F, h = 1){
  
  library(tidyverse)
  library(mboost)
  library(lubridate)
  library(scales)
  options(scipen = 99)
  
  #vars <- names(data[-1])
  #response <- names(data[1])
  #h1_vars <- c(response, vars[str_detect(vars, "_L")])
  #h1_data <- data %>% select(h1_vars)
  
  h1_data <- data
  
  if(h > 1){
    temp_data <- h1_data
    for (k in 0:(nrow(h1_data)-h)) {
      temp_data[h+k,1] <- sum(h1_data[(1+k):(h+k),1])
    }
    temp_data <- temp_data[-(1:(h-1)),]
    h1_data <- temp_data
  }
  cat("\n","Forecasts for h =",h,"\n","\n")
  
  idx <<- 1:ceiling(nrow(h1_data)*n)
  
  test_Y <<- h1_data[-idx, 1]
  test_regs <- h1_data[-idx, -1]
  train_Y <- h1_data[idx, 1]
  train_regs <- h1_data[idx, -1]
  
  fc <- c()
  iter <- c()
  selected_vars <- list()
  
  temp_Y <- train_Y
  temp_regs <- train_regs
  
  for (i in 1:length(test_Y)) {
    
    fc_regs <- test_regs[i,]
    
    if(norm){
      means <- c()
      sds <- c()
      for (j in 1:ncol(temp_regs)) {
        means[j] <- mean(temp_regs[,j])
        sds[j] <- sd(temp_regs[,j])
        temp_regs[,j] <- (temp_regs[,j] - means[j])/sds[j]
      }
      fc_regs <- (fc_regs - means)/sds
    }
    
    model <- lm(temp_Y ~.,data = temp_regs)
    cat("Forecast:",i,"\n")
    
    fc[i] <- predict(model, fc_regs)
    
    temp_Y <- c(train_Y[-(1:i)], test_Y[1:i])
    temp_regs <- train_regs[-(1:i),] %>% rbind(test_regs[1:i,])
  }
  return(fc)
}

##### GW Test #####

gw.test <- function(
  x,
  y,
  p,
  T,
  tau,
  method = c("HAC", "NeweyWest", "Andrews", "LumleyHeagerty"),
  alternative = c("two.sided", "less", "greater"),
  power = 2
){
  
  require(sandwich)
  
  if (is.matrix(x) && ncol(x) > 2) 
    stop("multivariate time series not allowed")
  if (is.matrix(y) && ncol(y) > 2) 
    stop("multivariate time series not allowed")
  if (is.matrix(p) && ncol(p) > 2) 
    stop("multivariate time series not allowed")
  
  # x: predicciones modelo 1
  # y: predicciones modelo 2
  # p: observaciones
  # T: sample total size
  # tau: horizonte de prediccion
  # method: if tau=1, method=NA. if tau>1, methods
  # alternative: "two.sided","less","greater"
  
  if(NCOL(x) > 1) stop("x is not a vector or univariate time series")     
  if(tau < 1) stop("Predictive Horizon must to be a positive integer")     
  if(length(x) != length(y)) stop("size of x and y differ")
  
  alternative <- match.arg(alternative)     
  DNAME <- deparse(substitute(x)) 
  
  l1=abs(x-p)^power
  l2=abs(y-p)^power
  dif=l1-l2
  q=length(dif)
  m=T-q	
  n=T-tau-m+1	
  delta=mean(dif)
  mod <- lm(dif~0+rep(1,q))
  
  if(tau==1){
    re=summary(mod)
    STATISTIC = re$coefficients[1,3]
    if (alternative == "two.sided") PVAL <- 2 * pnorm(-abs(STATISTIC))
    else if (alternative == "less") PVAL <- pnorm(STATISTIC)
    else if (alternative == "greater") PVAL <- pnorm(STATISTIC, lower.tail = FALSE)     
    names(STATISTIC) <- "Normal Standad"
    METHOD <- "Standard Statistic Simple Regression Estimator" 
    # Teste ##
    # r2 <- re$r.sq
    # est <- q * r2
    # p.valor <- pchisq(est, df = tau, lower.tail = FALSE)
    ###########
    
    
  }
  
  if(tau>1){
    
    if(method=="HAC"){ 
      METHOD <- "HAC Covariance matrix Estimation"
      ds=sqrt(vcovHAC(mod)[1,1])
    }
    if(method=="NeweyWest"){ 
      METHOD <- "Newey-West HAC Covariance matrix Estimation"
      ds=sqrt(NeweyWest(mod,tau)[1,1])
    }
    if(method=="LumleyHeagerty"){ 
      METHOD <- "Lumley HAC Covariance matrix Estimation"
      ds=sqrt(weave(mod)[1,1])
    }
    if(method=="Andrews"){ 
      METHOD <- "kernel-based HAC Covariance matrix Estimator"
      ds=sqrt(kernHAC(mod)[1,1])
    }
    #STATISTIC = sqrt(n)*delta/ds
    STATISTIC = delta/ds
    if (alternative == "two.sided") PVAL <- 2 * pnorm(-abs(STATISTIC))     
    else if (alternative == "less") PVAL <- pnorm(STATISTIC)
    else if (alternative == "greater") PVAL <- pnorm(STATISTIC, lower.tail = FALSE)     
    names(STATISTIC) <- "Normal Standar"
  }
  structure(
    list(
      statistic = STATISTIC,
      alternative = alternative,
      p.value = PVAL,
      method = METHOD,
      data.name = DNAME
      # teste.p.valor = p.valor
    )
  )
}


##### H Sets #####

h_sets <- function(data, h){
temp_data <- data
for (k in 0:(nrow(data)-h)) {
  if(ncol(data)>1){
    temp_data[h+k,] <- colSums(data[(1+k):(h+k),])
  }else{
    temp_data[h+k,] <- sum(data[(1+k):(h+k),])
  }
}
temp_data <- temp_data[-(1:(h-1)),]
data <- temp_data
return(data)
}