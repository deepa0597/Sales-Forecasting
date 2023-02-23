library(readxl)
library(forecast)
library(dplyr)

sales.data <- read.csv('Furniture_Data.csv')
View(sales.data)

df_sales <- data.frame(sales.data)


View(df_sales)

central.sales <- filter(df_sales, df_sales$Region =='Central')

View(furniture_sales.central)
central_profits.ts <- ts(central.sales$Sum.of.Profit, start = c(2014,1), end = c(2017,12), frequency = 12)  
central_sales.ts <- ts(central.sales$Sum.of.Sales, start = c(2014,1), end = c(2017,12), frequency = 12)  


valid_Length <- 6
train_Length <- length(central_sales.ts) - valid_Length


#Profits
dept_profitsTrain <- window(central_profits.ts,start= c(2014,1) , end = c(2014,train_Length))
dept_profitsValid <- window(central_profits.ts, start=c(2014,train_Length+1), end =c(2014,train_Length+valid_Length))

#Sales
dept_salesTrain <- window(central_sales.ts,start= c(2014,1) , end = c(2014,train_Length))
dept_salesValid <- window(central_sales.ts, start=c(2014,train_Length+1), end =c(2014,train_Length+valid_Length))



#Arima Model
#Sales
#Profit
Sales.arima.model <- Arima(dept_salesTrain, order=c(0,0,1), seasonal = list(order = c(0,1,1),period =12))
Profit.arima.model <- Arima(dept_profitsTrain, order=c(0,0,1), seasonal = list(order = c(0,1,1),period =12))


fit1<-Arima(dept_salesTrain, order=c(0,0,0), seasonal=c(0,0,0))
Acf(fit1$residuals,lag.max=24, main='d=0 D=0 acf')
Pacf(fit1$residuals,lag.max=24, main='d=0 D=0 pacf')

Sales.arima.model <- Arima(central_sales.ts, order=c(0,0,1), seasonal = list(order = c(0,1,1),period =12))
Profit.arima.model <- Arima(central_profits.ts, order=c(0,0,1), seasonal = list(order = c(0,1,1),period =12))



central.salesforecast <- forecast(Sales.arima.model, h=12)
central.salesforecast$mean

central.profitforecast <-forecast(Profit.arima.model, h=12)
central.profitforecast$mean

#Set y-axis limits based on the data and forecasted values
ylow <- min(c(min(central_sales.ts), min(central_profits.ts), min(central.salesforecast$lower), min(central.profitforecast$lower)))
yupper <- max(c(max(central_sales.ts), max(central_profits.ts), max(central.salesforecast$upper), max(central.profitforecast$upper)))

ts.plot(central_sales.ts, xlim = c(2014,2019), col = 'blue', type='l',ylab='Dollar Value', xlab = 'Year', ylim = c(ylow, yupper),main ='Central: Sales and Profit for Furniture', lwd=2.25)
lines(central_profits.ts, lwd= 1.5, col = ' green',lty=1)
points(central.salesforecast$mean , type = "l", col = 'blue',lty=2,lwd=2)
points(central.profitforecast$mean , type = "l", col = '#50C878',lty=2,lwd=1)

#adjust label as per the plot
legend(2018.15, ylow+550, legend = c('Sales ($)', 'Profit ($)', 'Forecast: Sales', 'Forecast: Profits'), col = c('blue', '#50C878', 'blue', '#50C878'), lty = c(1, 1, 2, 2), cex = 0.75)


#Same for Region: West ##############################################################

west.sales <- filter(df_sales, df_sales$Region =='West')

west_profits.ts <- ts(west.sales$Sum.of.Profit, start = c(2014,1), end = c(2017,12), frequency = 12)  
west_sales.ts <- ts(west.sales$Sum.of.Sales, start = c(2014,1), end = c(2017,12), frequency = 12)  


valid_Length <- 6
train_Length <- length(west_sales.ts) - valid_Length


#Profits
w.dept_profitsTrain <- window(west_profits.ts,start= c(2014,1) , end = c(2014,train_Length))
w.dept_profitsValid <- window(west_profits.ts, start=c(2014,train_Length+1), end =c(2014,train_Length+valid_Length))

#Sales
w.dept_salesTrain <- window(west_sales.ts,start= c(2014,1) , end = c(2014,train_Length))
w.dept_salesValid <- window(west_sales.ts, start=c(2014,train_Length+1), end =c(2014,train_Length+valid_Length))



#Arima Model Check
Acf(west_sales.ts,lag.max = 24)

#Sales
#Profit
WSales.arima.model <- Arima(w.dept_salesTrain, order=c(0,0,1), seasonal = list(order = c(0,1,1),period =12))
WProfit.arima.model <- Arima(w.dept_profitsTrain, order=c(0,0,1), seasonal = list(order = c(0,1,1),period =12))




WSales.arima.model <- Arima(west_sales.ts, order=c(1,1,1), seasonal = list(order = c(1,1,1),period =12))
WProfit.arima.model <- Arima(west_profits.ts, order=c(0,0,1), seasonal = list(order = c(0,1,1),period =12))

summary(WSales.arima.model)
summary(WProfit.arima.model)

west.salesforecast <- forecast(WSales.arima.model, h=12)
west.salesforecast

west.profitforecast <-forecast(WProfit.arima.model, h=12)
west.profitforecast

#Set y-axis limits based on the data and forecasted values
ylow <- min(c(min(west_sales.ts), min(west_profits.ts), min(west.salesforecast$lower), min(west.profitforecast$lower)))
yupper <- max(c(max(west_sales.ts), max(west_profits.ts), max(west.salesforecast$upper), max(west.profitforecast$upper)))

ts.plot(west_sales.ts, xlim = c(2014,2019), col = 'blue', type='l',ylab='Dollar Value', xlab = 'Year', ylim = c(ylow, yupper),main ='West: Sales and Profit for Furniture', lwd=2.25)
lines(west_profits.ts, lwd= 1.5, col = ' green',lty=1)
points(west.salesforecast$mean , type = "l", col = 'blue',lty=2,lwd=2)
points(west.profitforecast$mean , type = "l", col = '#50C878',lty=2,lwd=1)

#adjust label as per the plot
legend(2018.15, ylow+350, legend = c('Sales ($)', 'Profit ($)', 'Forecast: Sales', 'Forecast: Profits'), col = c('blue', '#50C878', 'blue', '#50C878'), lty = c(1, 1, 2, 2), cex = 0.75)
