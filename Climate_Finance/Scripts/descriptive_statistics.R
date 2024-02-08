#### Long ####
source("Scripts/functions.R")
rm(list = ls()) 
set1 <- read.csv2("Data/Long/1macro_financial.csv")
set2 <- read.csv2("Data/Long/2eco_fin_uncert.csv")
set3 <- read.csv2("Data/Long/3non_eco_fin_uncert.csv")
set4 <- read.csv2("Data/Long/4climate_change.csv")
set5 <- read.csv2("Data/Long/5climate_change_vol.csv")
y <- read.csv2("Data/Long/housing_returns.csv")

df <- cbind(y = y[,-1], set1, set2, set3, set4, set5)

iterations = 41
variables = 5

output <- matrix(ncol=variables, nrow=iterations)

for(i in 1:iterations){
  var <- df[,i]
  output[i,] <- c(names(df[i]),mean(var), sd(var), min(var), max(var))
  
}

output1 <- data.frame(output)
colnames(output1) <- c("var", "mean", "sd","min","max")
rm(list=setdiff(ls(), "output1"))

#### Short ####
y <- read.csv2("Data/Short/housing_returns.csv")

df <- y[,-1]

iterations = 5
variables = 5

output <- matrix(ncol=variables, nrow=iterations)

for(i in 1:iterations){
  var <- df[,i]
  output[i,] <- c(names(df[i]),mean(var), sd(var), min(var), max(var))
  
}

output2 <- data.frame(output)
colnames(output2) <- c("var", "mean", "sd","min","max")

output <- rbind(output2,output1)

write_csv2(output,"descriptive_statistics.csv")
