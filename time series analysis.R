
getwd()
df<-read.csv("dc.csv")

library(astsa) #astsa: Applied Statistical Time Series Analysis
install.packages("forecast")
install.packages("tseries")
install.packages("xts")
install.packages("ggplot2")
library(forecast)
library(tseries)
library(xts)
library(ggplot2)


df$X<-as.Date(df$X)#converting charcters in date format
df_train<-df[81:1000,1:9] #traing data 920 0bservation out of 1000
df_t<-df[71:80,1:9] #next 10 data for test/observed

ggplot(data = df, aes(x = X)) +
  geom_line(aes(y = open_SAR, colour = "open_SAR")) +
  geom_line(aes(y = open_USD, colour = "open_USD")) +
  geom_line(aes(y = high_SAR, colour = "high_SAR")) +
  geom_line(aes(y = high_USD, colour = "high_USD")) +
  geom_line(aes(y = low_SAR, colour = "low_SAR")) +
  geom_line(aes(y = low_USD, colour = "low_USD")) +
  geom_line(aes(y = close_SAR, colour = "close_SAR")) +
  geom_line(aes(y = close_USD, colour = "close_USD")) +

  scale_colour_manual("", 
                      breaks = c("open_SAR", "open_USD", "high_SAR","high_USD","low_SAR", "low_USD","close_SAR","close_USD"),
                      values = c("open_SAR"="red", "open_USD"="blue", "high_SAR"="pink","high_USD"="orange","low_SAR"="olivedrab", "low_USD"="coral","close_SAR"="purple","close_USD"="cyan")) +
  xlab("Time") +
  scale_y_continuous("Value", limits = c(0,158000)) + 
  labs(title="Digital currency")



ggplot()+
  geom_line(data=df_train,mapping = aes(x=X,y=close_SAR),col='purple')+
  xlab("Time")
# selection of the attribute we want to forecast and apply models on
#conversion into time series
data<-xts(df$close_SAR,df$X)
head(data)
autoplot(data)
data
decompdata<-decompose(data)
#analysis on our time series data
length(data)  #1000 observations
start(data) # starts from "2018-05-07"
end(data)  #ends on "2021-01-30"
frequency(data)  #frequency of data is 1 thus no seasonality
#The "frequency" is the number of observations before the seasonal pattern repeats

#conversion of training set and testing/observed values set as timeseries
train_sar<-xts(df_train$close_SAR,df_train$X)
test_sar<-xts(df_t$close_SAR,df_t$X)
length(train_sar)
head(train_sar)
tail(train_sar)
head(test_sar)
tail(test_sar)
#analysis on train_sar ,since the value i9s increasing over the time there can be positive trend
autoplot(train_sar)
#add one trend line
boxplot(train_sar) #presence of outliers

##########################################
###### Making time series stationary####
#from plot we conclude since it is growing trend so it is not stationary
autoplot(diff(train_sar))
#here we can check the stationary by adf test
adf.test(train_sar)
y22<-diff(log(train_sar))
y1<-na.omit(y22)
head(y1)

#shows p value =0.695 so it is not stationary
train_sard1<-diff(train_sar)
adf.test(train_sarl)
autoplot(train_sard1)
adf.test(na.omit(train_sard1))
#p value is less than 0.01 hence the difference of  training data set is stationary
# there is still fluctuating variance , so we can use log in data then difference
train_sarl<-log(train_sar)
autoplot(diff(train_sarl))
adf.test(na.omit(diff(train_sarl)))
#p value is less than 0.01 hence the difference of log of training data set is stationary
# since applying difference over log of actual ts gives better time series
#we will examine its ACF and PACF
acf2(diff(train_sarl))

#from ACF we can say MA of order 2 and from PACF AR of order 2 if we use original data d=1
#ARIMA(2,1,2) as our base model , we will tune parameters to see if other model yeild better rtesults

#fitting with different ARIMA model parameters
fit0<-arima(train_sar,order=c(2,1,2))
fit0
checkresiduals(fit0)
#gives AIC=15537.48
#residuals is-significant at lag 20 else it follows normal distribution
fit1<-arima(train_sar,order=c(1,1,2))
fit1
checkresiduals(fit1)
#gives AIC=15536.1
#residuals is-significant at lag 20  else it follows normal distribution
fit2<-arima(train_sar,order=c(2,1,1))
fit2
checkresiduals(fit2)  
#AIC=15535.47
#residuals is-significant at lag 20 and lag 8 else it follows normal distribution
fit3<-arima(train_sar,order=c(0,2,2))
fit3
checkresiduals(fit3) 
#AIC=aic = 15525.9
#residuals is-significant at lag 20 else it follows normal distribution
fit4<-arima(train_sar,order=c(0,1,2))
fit4
checkresiduals(fit4)
#AIC=15534.07
fit5<-arima(train_sarl,order=c(0,1,2))
checkresiduals(fit5)
#residuals is-significant at lag 20 else it follows normal distribution
###############################################################
#from above we can say fit4 and fit3 is better option based on AIC value
#predicting rest of values with fit4 for next 30 prediction
forecast(fit4,h=10)$pred
predict(fit4,n.ahead = 10)$pred

autoplot(forecast(fit4,h=10))
accuracy(predict(fit4,n.ahead = 10)$pred,test_sar)
#MAPE of fit4 is 13.246

#for fit3
forecast(fit3,h=10)

predict(fit3,n.ahead = 10)$pred
autoplot(forecast(fit3,h=10)) 
accuracy(predict(fit3,n.ahead = 10)$pred,test_sar)
test_sar
#MAPE of fit3  is  10.4718
#hence we can say fit3 is better fitted than fit 4 , ARIMA(0,1,2) is better model to predict among rest mentioned model

########## using Sarima model to predict and check residuals####
sarima(train_sar,p=0,d=2,q=2)
sarima.for(train_sar,10,p=0,d=2,q=2)

#since we took log of actual values for fitment we will revert it to original
t1=2.718^sarima.for(train_sarl,10,p=0,d=2,q=2)$pred
autoplot(t1)
accuracy(t1,test_sar)
sarima(train_sarl,p=0,d=2,q=2)
#######
sarima(train_sarl,p=0,d=1,q=2)
sarima.for(train_sarl,10,p=0,d=1,q=2)$pred
#since we took log of actual values for fittment we will revert it to original
t2=2.718^sarima.for(train_sarl,10,p=0,d=1,q=2)$pred
ts.plot(t2)
accuracy(t2,test_sar)
######
