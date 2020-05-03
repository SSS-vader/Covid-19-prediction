# Coronavirus prediction
# USING CUMULATIVE CASES

# Trends data is for "Coronavirus testing" in India
# 8 Mar to 29 April
Trends = read.csv("Google trends.csv")
str(Trends)

Cases_master = read.csv("case_time_series.csv")

# daily cases, 1 day lag for cases and daily trends
# Train: Cases and Trends for 8 march to 23 April and Lag is 7 March to 22 April
# Test: Cases and Trends for 24 April to 29 April and Lag for 23 April to 28 May
Cumulative = Cases_master$Total.Confirmed[4:56]
tre = Trends$covid.helpline.number
Cases = data.frame(Cumulative, tre)

library(zoo)
Lag1 = lag(zoo(Cumulative), -1, na.pad = TRUE)
Cases$Lag1 = coredata(Lag1)
Cases$Lag1[1] = Cases_master$Cumulative.Confirmed[3]

Train = Cases[1:48,]
Test = Cases[49:nrow(Cases),]

Model = lm(Cumulative ~ tre + Lag1, data = Train)
summary(Model)

pred = predict(Model, newdata=Test)
SSE = sum((Test$Cumulative - pred)^2)
SST = sum((Test$Cumulative - mean(Train$Cumulative))^2)
RMSE = sqrt(SSE/nrow(Test))
RMSE
R_sq = 1 - (SSE/SST)
R_sq
