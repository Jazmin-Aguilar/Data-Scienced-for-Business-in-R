#install packages
install.packages ("tidyverse")
install.packages("TTR")

#load libraries
library(tidyverse)
library(TTR)

#set working directory (adjust this for your own computer)
setwd("C:/Users/jazmi/OneDrive/1. DTSC-560-80 Data Science for Business/DTSC-560-80 Data Science for Business/Module 4")

#read dataset into R
airfaredf <- read.csv("airfare.csv")
View(airfaredf)

#1.
ggplot(data = airfaredf, mapping = aes(x = Year, y = Airfare)) +
  geom_line (group=1) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))
labs(title = "Airfare in Year", 
     x = "Year", y = "Airfare")

#2.use a simple moving average with n= 3 (3 years) to calculate
#a forecast for the average airfare in 2019

#create a separate vector for the actual weekly sales
airfare_actual<-airfaredf$Airfare

#use the simple moving average method to forecast the 13th week of milk sales

sma13<-SMA (airfare_actual, n=3)
sma13
#The last value in the vector is the forecast for sales for the 13th week
#353.7600



#3.Based on the predicted values up to 2018, calculate accuracy measures (MAE,
#MSE, RMSE, and MAPE) for the simple moving average method.
#Quiz question #3: What is the MAE for the simple moving average method?


#Create functions for the accuracy measures with vector of actual values 
#and vector of predicted values as inputs


#because its average of three means, the first three need to be NA for the predicted vector
#to calculate the accuracy measures
#Adjust the vector of predicted values to align with the sales_actual vector
sales_ma_pred<-c(NA, sma13[-length(sma13)]) 
sales_ma_pred

mae<-function(actual,pred){
  mae <- mean(abs(actual-pred), na.rm=TRUE)
  return (mae)
}

mse<-function(actual,pred){
  mse <- mean((actual-pred)^2, na.rm=TRUE)
  return (mse)
}

rmse<-function(actual,pred){
  rmse <- sqrt(mean((actual-pred)^2, na.rm=TRUE))
  return (rmse)
}  

mape<-function(actual,pred){
  mape <- mean(abs((actual - pred)/actual), na.rm=TRUE)*100
  return (mape)
}
#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(airfare_actual, sales_ma_pred)
mse(airfare_actual, sales_ma_pred)
rmse(airfare_actual, sales_ma_pred)
mape(airfare_actual, sales_ma_pred)
#19.30083

#Use a simple exponential smoothing with a smoothing constant of 0.2 to calculate
#a forecast for the average airfare in 2019.
#Quiz question #4: What is the 2019 forecast for average airfare using simple exponential
#smoothing with a smoothing constant of 0.2?

#use the exponential smoothing method with alpha = 0.2 to forecast the 
#2019 year of airefare
exp13 <- EMA (airfare_actual, n=1, ratio = .2)
exp13


#The last value in the vector is the airefare estimate for year 2019
# 376.6357

#5. Based on the predicted values up to 2018, calculate accuracy measures (MAE,
#MSE, RMSE, and MAPE) for the simple exponential smoothing method.
#Quiz question #5: What is the MAE for the simple exponential smoothing method with a
#smoothing constant of 0.2?

#first value needs to be NA for the EMA predicted to calc accuracy measures
#Adjust the vector of predicted values to align with the sales_actuals vector
exp_pred <- c(NA, exp13[-length(exp13)])

#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(airfare_actual, exp_pred)
mse(airfare_actual, exp_pred)
rmse(airfare_actual, exp_pred)
mape(airfare_actual, exp_pred)

## MAE: 17.71794



#6. Use a simple exponential smoothing with a smoothing constant of 0.8 to calculate
#a forecast for the average airfare in 2019.
#Quiz question #6: What is the 2019 forecast for average airfare using simple exponential
#smoothing with a smoothing constant of 0.8?

exp19 <- EMA (airfare_actual, n=1, ratio = .8)
exp19
#forecast of airfare is 348.4198



#7. Based on the predicted values up to 2018, calculate accuracy measures (MAE,
#MSE, RMSE, and MAPE) for the simple exponential smoothing method.
#Quiz question #7: What is the MAE for the simple exponential smoothing method with a
#smoothing constant of 0.8?
#Quiz question #8: Reviewing the accuracy measure values for each of these forecasting
#attempts, which would you trust most to provide the most accurate forecast of 2019
#average airfare?

exp_pred19 <- c(NA, exp19[-length(exp19)])
mae(airfare_actual, exp_pred19)
mse(airfare_actual, exp_pred19)
rmse(airfare_actual, exp_pred19)
mape(airfare_actual, exp_pred19)


#QQ7: MAE: 15.25307

#QQ8: the smoothing constant of 0.8





#Part 2:

#read dataset into R
wmusicdf <- read.csv("warner_music.csv")
View(wmusicdf)

#8. Create a time series plot of the Warner Music Group data.
#Quiz question #9: What patterns do you notice in the time series plot? (MC)


#create series plot showing yearly net revenue in billions
ggplot(data = wmusicdf, mapping = aes(x = Quarter, y = Revenue)) +
  geom_line (group = 1) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))
  labs(title = "Quarterly Revenue 2015 to 2021", x = "Year", y = "Revenue")

#goes up but has spikes in 1st quarter

#9. Use linear regression to model ONLY the trend in the time series.
#Quiz question #10: After conducting a linear regression to model the trend ONLY, what is
#the slope coefficient for the trend variable?   

#Add a column of consecutive numbers corresponding with each year
wmusicdf$Time <- 1:nrow(wmusicdf) 

lreg = lm(Revenue ~ Time, data= wmusicdf)
summary(lreg)
#slope : 23.190


#Calculate accuracy measures for predicted values based on the regression in (2).
#Quiz question #11: What is the RMSE based on the results of the linear regression you
#just conducted?
#Create a vector of predicted values generated from the 
#regression above (sbreg)
sb_pred = predict(lreg)


#Run the accuracy measure functions with vector of actual values and vector
#of predicted values as inputs
mae(wmusicdf$Revenue, sb_pred)
mse(wmusicdf$Revenue, sb_pred)
rmse(wmusicdf$Revenue, sb_pred)
mape(wmusicdf$Revenue, sb_pred)
##RMSE: 72.07186


#Use linear regression to model both the trend and the seasonality in the time
#series. Choose as a reference variable the quarter tending to show the highest
#revenues each year.
#Quiz question #12: After conducting a linear regression to model both trend and
#seasonality, how should the regression coefficient for Quarter 2 be interpreted? (MC


#Create dummy variables corresponding to each quarter 
wmusicdf$Q1 <- ifelse(grepl("Q1",wmusicdf$Quarter), 1, 0)
wmusicdf$Q2 <- ifelse(grepl("Q2",wmusicdf$Quarter), 1, 0)
wmusicdf$Q3 <- ifelse(grepl("Q3",wmusicdf$Quarter), 1, 0)
wmusicdf$Q4 <- ifelse(grepl("Q4",wmusicdf$Quarter), 1, 0)


#Use multiple regression with quarter variables to generate a regression 
#equation for forecasting
mreg<-lm(Revenue ~ Time + Q2 + Q3 + Q4, data = wmusicdf)
summary(mreg)

#-133.969



#Calculate accuracy measures for predicted values based on the regression in (4).
#Quiz question #13: What is the RMSE based on the results of the second linear
#regression you just conducted?
#Quiz question #14: What conclusion can you draw from comparing the accuracy
#measure values from the first regression to those from the second regression? (MC)

#Create a vector of predicted values generated from the multiple 
#regression above
wm_pred2 = predict(mreg)


#calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(wmusicdf$Revenue, wm_pred2)
mse(wmusicdf$Revenue, wm_pred2)
rmse(wmusicdf$Revenue, wm_pred2)
mape(wmusicdf$Revenue, wm_pred2)
#rmse: 48.40696
#mreg is better lower errors


#Forecast Q1, Q2, Q3, and Q4 revenues for Warner Music Group for 2022 using the
#results of the second regression analysis modeling both trend and seasonality.
#Quiz question #15: Based on the second regression analysis modeling both trend and
#seasonality, what is the forecasted revenue for Warner Music Group for Quarter 4 of
#2022?

#Create an object with the time periods to use for the prediction
new <- data.frame(Time = c(26, 27, 28, 29), Q2 = c(0,1,0,0), Q3 = c(0,0,1,0), 
                  Q4 = c(0,0,0,1)) 
predict(mreg, newdata = new)
#4 -> 1405.559




#Part 3:


amazondf <- read.csv("amazon_web_services.csv")
View(amazondf)


#1. create a time series plot of the amazon web 
#services revenue data

#create series plot showing yearly net revenue in billions
ggplot(data = amazondf, mapping = aes(x = Quarter, y = Revenue)) +
  geom_line (group = 1) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))
labs(title = "Quarterly Revenue 2014 to 2021", x = "Year/Quarter", y = "Revenue")

#2.Use linear regression first to model the trend in the time series.
#Quiz question #16: After conducting a linear regression to model
#the trend in the time
#series, what is the slope coefficient for the trend variable?



amazondf$Time <- 1:nrow(amazondf)

alreg = lm(Revenue ~ Time, data = amazondf)
summary(alreg)
# slope coefficient: 497.9

#3 Calculate accuracy measures for predicted values based on the regression in (2).
#Quiz question #17: What is the MAPE based on the results of the linear regression you
#just conducted?

am_pred = predict(alreg)

#calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(amazondf$Revenue, am_pred)
mse(amazondf$Revenue, am_pred)
rmse(amazondf$Revenue, am_pred)
mape(amazondf$Revenue, am_pred)

#31.39103

#Based on the pattern in the time series data, choose a regression approach to use
#to model the trend, and conduct that regression analysis.
#Quiz question #18: Based on the pattern in this time series data, what type of regression
#discussed in this module would be most appropriate to calculate forecasts based on this
#time series?



#Create a new variable that squares the Time variable
amazondf$Time2 <- amazondf$Time^2


aregquad <- lm(Revenue ~ Time + Time2, data=amazondf)
summary(aregquad)

aregpred <- predict(aregquad)


mae(amazondf$Revenue, aregpred)
mse(amazondf$Revenue, aregpred)
rmse(amazondf$Revenue, aregpred)
mape

#mape: 5.205433


#6 Forecast Q1, Q2, Q3, and Q4 revenues for Amazon Web Services for 2022 using
#the results of the second regression analysis
#Quiz question #20: Based on results from the second regression analysis, what is the
#forecasted revenue for Amazon Web Services for Quarter 3 of 2022?


#Create an object with the time periods to use for the prediction
new <- data.frame(Time = c(33, 34, 35, 36), Time2 = c(1089, 1156, 1225, 1296))
predict(aregquad, newdata = new)

#quarter 3: 19788.18
