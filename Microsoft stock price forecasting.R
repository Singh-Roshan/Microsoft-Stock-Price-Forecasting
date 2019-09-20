library(ggplot2)
library(quantmod)
library(ggplot2)
library(fpp)
library(fpp2)

start_date <- as.Date("2012-01-01")
end_date <- as.Date("2019-01-01")
start_date
end_date

lapply(start_date, class)
lapply(end_date, class)

#Data crawling from Yahoo finance
getSymbols("MSFT", src = "yahoo", from = start_date, to = end_date)
summary(MSFT)
head(MSFT)
View(MSFT)
names(MSFT)
data <- ts(MSFT,start=c(2012,1),end=c(2019,01), frequency = 12)
data=data[,3]
View(data)

#Calculating Train and Test data
train_data = window(data,start=c(2012,1), end=c(2016,12))
test_data = window(data,start=c(2017,1), end=c(2018,12))
train_data
test_data

#DataPlot
autoplot(data) + ggtitle("Microsoft stock price") + ylab("$ million") + xlab("Year")
#Seasonal DataPlot
ggseasonplot(data, year.labels=TRUE, year.labels.left=TRUE) + ylab("$
million") + ggtitle("Seasonal plot: Microsoft stock price")
#Seasonal subseries plot
ggsubseriesplot(data) + ylab("$ million") + ggtitle("Seasonal subseries plot:Microsoft stock price")

#Test for Stationary
Box.test(data, lag = 20, type = 'Ljung-Box')
#Adf test
adf.test(data)
#Autocorelation Function
Acf(data, lwd=3,main="Microsoft stock price") #By seeing the plot, we can make out, it is not the stationary hence, we are using differencing
Acf(diff(data), lwd=3,main="Microsoft stock price")

#Partial ACF
Pacf(data, lwd=3,main="Without diff Microsoft stock price")
Pacf(diff(data), lwd=3,main="Microsoft stock price")

adf.test(diff(data)) #here pvalue is lesss than 0.05, hence series is stationary

#Mean, Naive, Seasonal Naive
fit.mean=meanf(train_data,h=12)
fit.naive=naive(train_data,h=12)
fit.snaive=snaive(train_data,h=12)
autoplot(train_data) +
  autolayer(meanf(train_data, h=12),
            series="Mean", PI=FALSE) +
  autolayer(naive(train_data, h=12),
            series="Naive", PI=FALSE) +
  autolayer(snaive(train_data, h=12),
            series="Seasonal naive", PI=FALSE) +
  ggtitle("Forecasts Microsoft stock price") +
  xlab("Year") + ylab("$ millions") +
  guides(colour=guide_legend(title="Forecast"))

#Linear Trend
linear_reg <- tslm(train_data ~ trend)
fit.tslm1=forecast(linear_reg, h=12)
summary(fit.tslm1)
plot(fit.tslm1, ylab="Microsoft stock price",
     xlab="t")

#Season + Trend
linear_season <- tslm(train_data ~ trend + season)
fit.tslm2=forecast(linear_season, h=12)
summary(fit.tslm2)
plot(fit.tslm2, ylab="Microsoft stock price",
     xlab="t")


#STL decomposition
stl_decomp <- stl(train_data, t.window=12, s.window="periodic")
plot(stl_decomp)

fit.stl <- forecast(stl_decomp,h=12)
summary(fit.stl)
plot(fit.stl)

#Seasonally adjusted data
plot(train_data, col="grey",
     main="Microsoft stock price",
     xlab="", ylab="New orders index")
lines(seasadj(stl_decomp),col="red",ylab="Seasonally adjusted")

#Moving Average
par(mfrow=c(2,2))

plot(train_data, main="Microsoft stock price",
     ylab="$ million", xlab="Year")
lines(ma(train_data,3),col="yellow")
legend("topleft",lty=1,col="yellow",cex=0.6,
       legend=c("3-MA"))
plot(train_data, main="Microsoft stock price",
     ylab="$ million", xlab="Year")
lines(ma(train_data,5),col="blue")
legend("topleft",lty=1,col="blue",cex=0.6,
       legend=c("5-MA"))
plot(train_data, main="Microsoft stock price",
     ylab="$ million", xlab="Year")
lines(ma(train_data,7),col="red")
legend("topleft",lty=1,col="red",cex=0.6,
       legend=c("7-MA"))
plot(train_data, main="Microsoft stock price",
     ylab="$ million", xlab="Year")
lines(ma(train_data,9),col="green")
legend("topleft",lty=1,col="green",cex=0.6,
       legend=c("9-MA"))


#SES
fit.ses <- ses(train_data, h = 12)
fit.ses <- forecast(fit.ses)
summary(fit.ses)
plot(fit.ses)

fit1 <-ses(train_data, alpha=0.2, initial="simple", h=3)
fit2 <-ses(train_data, alpha=0.6, initial="simple", h=3)
fit3 <-ses(train_data, h=3)
plot(fit1,main="Microsoft stock price", ylab="$
     (millions)", xlab="Year", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")


lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"),
       c("data", expression(lambda == 0.2), expression(lambda == 0.6),
         expression(lambda == 0.89)),pch=1)

#Holt's Linear trend
#The SES model usually doesn’t work well when the data shows a long term trend. This method uses two smoothing techniques instead of just the alpha one
fit.hlinear <- holt(train_data, h=3)
fit.hlinear <- forecast(fit.hlinear)
summary(fit.hlinear)
plot(fit.hlinear, main = "Holt's Linear Trend")
lines(train_data)


#Holt's Winter Additive and Multiplicative
fit1_add <- hw(train_data,seasonal="additive")
fit1_add <- forecast(fit1_add)
fit2_multi <- hw(train_data,seasonal="multiplicative")
fit2_multi <- forecast(fit2_multi)


autoplot(train_data) +
  autolayer(fit1_add, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2_multi, series="HW multiplicative forecasts", PI=FALSE) +
  xlab("Year") +
  ylab("$ (millions)") +
  ggtitle("Microsoft stock price") +
  guides(colour=guide_legend(title="Forecast"))



#Auto ARIMA
#One of the most used Time Series analysis models. We will use the auto.arima() function to find the values automatically
y.arima <- auto.arima(train_data)
summary(y.arima)
fit.arima <- forecast(y.arima, h=12)
summary(fit.arima)
plot(fit.arima)


#Comparison between models
#To see the whole picture of our analysis, now we will display all the accuracies of each model we have created so far
a.mean=accuracy(fit.mean,test_data)
a.naive=accuracy(fit.naive,test_data)
a.snaive=accuracy(fit.snaive,test_data)
a.linear=accuracy(fit.tslm1,test_data)
a.linear_season=accuracy(fit.tslm2,test_data)
a.ets=accuracy(fit.ets_forecast,test_data)
a.ses=accuracy(fit.ses, test_data)
a.stl=accuracy(fit.stl, test_data)
a.holt=accuracy(fit.hlinear, test_data)
a.multi=accuracy(fit1_add, test_data)
a.add=accuracy(fit2_multi, test_data)
a.arima=accuracy(fit.arima, test_data)


a.table<-rbind(a.mean, a.naive, a.snaive, a.linear, a.linear_season, a.ets, a.ses, a.stl, a.holt, a.add, a.multi, a.arima)
a.table
row.names(a.table)<-c('Mean training','Mean test', 'Naive training', 'Naive test', 'Seasonal. Naive training', 'Seasonal. Naive test' ,'Linear training', 'Linear test','season-trend training', 'season-trend test', 'ets training', 'ets test',"ses training", "ses test",'STL training', 'STL test',"Holt's Linear training", "Holt's Linear test", 'Add training', 'Add test','Multi training', 'Multi test','ARIMA training', 'ARIMA test')


#Final Tabular format
a.table<-as.data.frame(a.table)
a.table

