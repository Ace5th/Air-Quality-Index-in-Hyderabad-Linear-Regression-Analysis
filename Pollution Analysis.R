install.packages("ggplot2")
install.packages("performance")
install.packages("effects")
install.packages("sjPlot")
library(ggplot2)
library(performance)
library(effects)
library(sjPlot)
library(see)
library(patchwork)

getwd()
setwd()
aqi <- read.csv(file = "AQI Data Set.csv", stringsAsFactors = T)

nrow(aqi)
ncol(aqi)
str(aqi)
colSums(is.na(aqi))
summary(aqi)

aqi <- subset(aqi, select = -c(1,2))

aqi[is.na(aqi["PM10.in.æg.m3"])] <- sum(aqi$PM10.in.æg.m3)/nrow(aqi)
aqi[is.na(aqi["SO2.in.æg.m3"])] <- sum(aqi$SO2.in.æg.m3)/nrow(aqi)
aqi[is.na(aqi["NOx..in.æg.m3"])] <- sum(aqi$NOx..in.æg.m3)/nrow(aqi)
aqi[is.na(aqi["AQI"])] <- sum(aqi$AQI)/nrow(aqi)

sum(aqi$NOx..in.æg.m3)
sum(aqi['NOx..in.æg.m3',])

for(i in 1:ncol(aqi)){
  aqi[is.na(aqi[,i]), i] <- mean(aqi[,i], na.rm = TRUE)
}

#aqi_lm <- lm(AQI ~ PM10.in.æg.m3 + SO2.in.æg.m3 + NOx..in.æg.m3 + PM2.5..in.æg.m3
#             + Ammonia...NH3..in.æg.m3 + O3...in.æg.m3 + CO..in.mg.m3
#             + Benzene..in.æg.m3, data = aqi)

aqi_lm <- lm(AQI ~ ., data = aqi)

summary(aqi_lm)
check_model(aqi_lm)
