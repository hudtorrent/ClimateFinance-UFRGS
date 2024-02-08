library(tidyverse)
library(xlsx)
##### Long #####
load("Scripts/RW Forecast H/Long/Results/norm-All.RData")

h <- c("h3","h6","h12")
for (i in 1:3) {

temp <- cbind(stats_boosting[[h[i]]], stats_lm[[h[i]]])

write.xlsx(as.data.frame(temp), file="C:/Users/bruno/Desktop/stats_long_All.xlsx",
           sheetName = h[i], row.names = F, append=TRUE, showNA = F)

temp2 <- vars_boosting[[h[i]]]

write.xlsx(as.data.frame(temp2), file="C:/Users/bruno/Desktop/vars_long_All.xlsx",
           sheetName = h[i], row.names = F, append=TRUE, showNA = F)
}
##### Short #####
load("Scripts/RW Forecast H/Short/Results/norm-All.RData")

h <- c("h3","h6","h12")

for (i in 1:3) {

temp <- cbind(
  stats_boosting[["YA"]][[h[i]]], stats_lm[["YA"]][[h[i]]],
  stats_boosting[["YM"]][[h[i]]], stats_lm[["YM"]][[h[i]]],
  stats_boosting[["YN"]][[h[i]]], stats_lm[["YN"]][[h[i]]],
  stats_boosting[["YS"]][[h[i]]], stats_lm[["YS"]][[h[i]]],
  stats_boosting[["YW"]][[h[i]]], stats_lm[["YW"]][[h[i]]])

write.xlsx(as.data.frame(temp), file="C:/Users/bruno/Desktop/stats_short_All.xlsx",
           sheetName = h[i], row.names = F, append=TRUE, showNA = F)

temp2 <- cbind(
  vars_boosting[["YA"]][[h[i]]],
  vars_boosting[["YM"]][[h[i]]],
  vars_boosting[["YN"]][[h[i]]],
  vars_boosting[["YS"]][[h[i]]],
  vars_boosting[["YW"]][[h[i]]])

write.xlsx(as.data.frame(temp2), file="C:/Users/bruno/Desktop/vars_short_All.xlsx",
           sheetName = h[i], row.names = F, append=TRUE, showNA = F)
}
