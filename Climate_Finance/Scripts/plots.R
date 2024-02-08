source("Scripts/functions.R")
rm(list = ls())
#### Dependent ####

y_long <- read.csv2("Data/Long/housing_returns.csv")
y_long$Date <- my(y_long$Date)
y_short <- read.csv2("Data/Short/housing_returns.csv")
y_short$Period <- ym(str_replace(y_short$Period, "M", ""))

dependent <- y_long %>% left_join(y_short, by=c("Date" = "Period"))
dependent <- dependent %>% gather(key = "series", value = "value", Y,YA,YN,YM,YS,YW)
ggplot(dependent)+
  geom_line(aes(x = Date, y = value), color = "cornflowerblue")+
  facet_wrap(.~series, scales = "free", ncol = 2)+
  labs(x = NULL, y = NULL)+
  theme(plot.title = element_text(size = 12, family = "serif"),
        plot.caption = element_text())
ggsave("dependent.pdf", plot = last_plot(), width = 11.5, height = 7, units = "in")

#### Set 1 ####
rm(list = ls())
df <- read.csv2("Data/Long/1macro_financial.csv")
date <- read.csv2("Data/Long/housing_returns.csv")[,1]
df <- cbind(date, df)
df$date <- my(df$date)
rm(date)
df <- df %>% gather(key = "series", value = "value",setdiff(names(df),"date"))
ggplot(df)+
  geom_line(aes(x = date, y = value), color = "cornflowerblue")+
  facet_wrap(.~series, scales = "free", ncol = 2)+
  labs(x = NULL, y = NULL)+
  theme(plot.title = element_text(size = 12, family = "serif"),
        plot.caption = element_text())
ggsave("set1.pdf", plot = last_plot(), width = 11.5, height = 7, units = "in")

#### Set 2 ####
rm(list = ls())
df <- read.csv2("Data/Long/2eco_fin_uncert.csv")
date <- read.csv2("Data/Long/housing_returns.csv")[,1]
df <- cbind(date, df)
df$date <- my(df$date)
rm(date)
df <- df %>% gather(key = "series", value = "value",setdiff(names(df),"date"))
lvs <- unique(df$series)
df$series <- factor(df$series, levels = lvs)
ggplot(df)+
  geom_line(aes(x = date, y = value), color = "cornflowerblue")+
  facet_wrap(.~series, scales = "free", ncol = 2)+
  labs(x = NULL, y = NULL)+
  theme(plot.title = element_text(size = 12, family = "serif"),
        plot.caption = element_text())
ggsave("set2.pdf", plot = last_plot(), width = 11.5, height = 7, units = "in")       
#### Set 3 ####
rm(list = ls())
df <- read.csv2("Data/Long/3non_eco_fin_uncert.csv")
date <- read.csv2("Data/Long/housing_returns.csv")[,1]
df <- cbind(date, df)
df$date <- my(df$date)
rm(date)
df <- df %>% gather(key = "series", value = "value",setdiff(names(df),"date"))
lvs <- unique(df$series)
df$series <- factor(df$series, levels = lvs)
ggplot(df)+
  geom_line(aes(x = date, y = value), color = "cornflowerblue")+
  facet_wrap(.~series, scales = "free", ncol = 2)+
  labs(x = NULL, y = NULL)+
  theme(plot.title = element_text(size = 12, family = "serif"),
        plot.caption = element_text())
ggsave("set3.pdf", plot = last_plot(), width = 11.5, height = 7, units = "in")
#### Set 4 ####
rm(list = ls())
df <- read.csv2("Data/Long/4climate_change.csv")
date <- read.csv2("Data/Long/housing_returns.csv")[,1]
df <- cbind(date, df)
df$date <- my(df$date)
rm(date)
df <- df %>% gather(key = "series", value = "value",setdiff(names(df),"date"))
lvs <- unique(df$series)
df$series <- factor(df$series, levels = lvs)
ggplot(df)+
  geom_line(aes(x = date, y = value), color = "cornflowerblue")+
  facet_wrap(.~series, scales = "free", ncol = 2)+
  labs(x = NULL, y = NULL)+
  theme(plot.title = element_text(size = 12, family = "serif"),
        plot.caption = element_text())
ggsave("set4.pdf", plot = last_plot(), width = 11.5, height = 7, units = "in")
#### Set 5 ####
rm(list = ls())
df <- read.csv2("Data/Long/5climate_change_vol.csv")
date <- read.csv2("Data/Long/housing_returns.csv")[,1]
df <- cbind(date, df)
df$date <- my(df$date)
rm(date)
df <- df %>% gather(key = "series", value = "value",setdiff(names(df),"date"))
lvs <- unique(df$series)
df$series <- factor(df$series, levels = lvs)
ggplot(df)+
  geom_line(aes(x = date, y = value), color = "cornflowerblue")+
  facet_wrap(.~series, scales = "free", ncol = 2)+
  labs(x = NULL, y = NULL)+
  theme(plot.title = element_text(size = 12, family = "serif"),
        plot.caption = element_text())
ggsave("set5.pdf", plot = last_plot(), width = 11.5, height = 7, units = "in")