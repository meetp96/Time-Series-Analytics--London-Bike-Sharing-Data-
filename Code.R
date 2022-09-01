###############################################################
# Load libraries
###############################################################
install.packages(c("dataMaid","DataExplorer", "janitor"))
library(dataMaid) #quick report for data set
library(DataExplorer) #quick report for data set
library(ggplot2)   # For qplot
library(lubridate) # for mdy date conversion
library(ggfortify) # for ts autoplot
library(zoo)      # for ts zoo object 
library(forecast)   # univariate time series forecasts and etc. 
library(TSA)        # For the eacf function
library(fUnitRoots) #p-values for ADF test
source("Backtest.R") #from class R code
library(lmtest) #for linear regression models
library(tseries) #for ADF and KPSS test   
library(xts)

################Sample Script to combine count data date wise#############
###############Meet Patel###################
df1 <- read.csv('1.csv')
#df1

df1$Start.Date <- as.Date(df1$Start.Date,format = "%d/%m/%Y")
df1$Start.Date
tab1 <- table(cut(df1$Start.Date, 'day'))

dff1 <- data.frame(Date=format(as.Date(names(tab1)), '%m/%d/%Y'),
                   Frequency=as.vector(tab1))
dff1
write.csv(dff1,"P1111111.csv", row.names = FALSE)

############## Using Dplyr to aggregate data ##############
##########  Haoyu Lu ##############
bike = read.csv('BikeSharing_2012_2017.csv')
#The date is in Month/Date/Year
head(bike)
head(bike)
tail(bike)

bike$timestamp <- as.Date(bike$timestamp, format = '%m/%d/%y')
head(bike)

set.seed(1)

library(lubridate)
library(dplyr)
df = bike

df$week <- floor_date(df$timestamp, "week")
#find mean by week
Weekly = df %>%
  group_by(week) %>%
  summarize(cnt = mean(cnt), t1 = mean(t1), t2 = mean(t2), hum = mean(hum), ws = mean(wind_speed))
getwd()
write.csv(Weekly, "/Users/angela/Desktop/Graduate/2021-2022 fall/CSC 425 Time Series/Project/new/Weekly.csv")


df$month <- floor_date(df$timestamp, "month")
#find mean by week
Monthly = df %>%
  group_by(month) %>%
  summarize(cnt = mean(cnt), t1 = mean(t1), t2 = mean(t2), hum = mean(hum), ws = mean(wind_speed))
write.csv(Monthly, "/Users/angela/Desktop/Graduate/2021-2022 fall/CSC 425 Time Series/Project/new/Monthly.csv")

df$year <- floor_date(df$timestamp, "year")
#find mean by week
Yealy = df %>%
  group_by(year) %>%
  summarize(cnt = mean(cnt), t1 = mean(t1), t2 = mean(t2), hum = mean(hum), ws = mean(wind_speed))
write.csv(Yealy, "/Users/angela/Desktop/Graduate/2021-2022 fall/CSC 425 Time Series/Project/new/Yealy.csv")


###############################################################
# Load the Bike Sharing Demand data set
###############################################################
#The daily data set has date is in Month/Date/Year
#bike = read.csv('BikeSharing_2012_2017.csv')
bike = read.csv('Weekly.csv')
#bike = read.csv('Yearly.csv')
#bike = read.csv('Monthly.csv)
head(bike)
head(bike)
tail(bike)


#data starts from 1/4/12 to 1/3/17

###############################################################
#Date Formatting
###############################################################

#bike$timestamp <- as.Date(bike$timestamp, format = '%m/%d/%y')
#head(bike)
#The date is in now in Year/Month/Date

###############################################################
#Quick Exploratory Data Analysis Report
###############################################################

makeDataReport(bike, replace = TRUE)
summary(bike)
str(bike)
qplot(data=bike, timestamp, cnt, geom="line",main="Count")
qplot(data=bike, timestamp, t1, geom="line",main="Temperature")
qplot(data=bike, timestamp, t2, geom="line",main="Feels Like Temperature")
qplot(data=bike, timestamp, hum, geom="line",main="Humidity")
qplot(data=bike, timestamp, ws, geom="line",main="Wind Speed")

boxplot(bike$cnt,ylab = "Count",main="Count")
boxplot(bike$t1,ylab = "T1",main="Temperature")
boxplot(bike$t2,ylab = "T2",main="Feels Like Temperature")
boxplot(bike$hum,ylab = "Humidity",main="Humidity")
boxplot(bike$ws,ylab = "Wind Speed",main="Wind Speed")


###############################################################
#Converting to Time Series Object
###############################################################

#t1TS = ts(london$t1, start=c(2015,1), frequency=364*24) #364*24
#Select based on dataset.
#if daily dataset
bikeCount = ts(bike$cnt,start = c(2012,1), frequency = 365)
biket1 = ts(bike$t1,start = c(2012,1), frequency = 365)
biket2 = ts(bike$t2,start = c(2012,1), frequency = 365)
bikehumidity = ts(bike$hum,start = c(2012,1), frequency = 365)
bikewindspeed = ts(bike$ws,start = c(2012,1), frequency = 365)

#if weekly dataset
bikeCount = ts(bike$cnt,start = c(2012,1), frequency =  52)
biket1 = ts(bike$t1,start = c(2012,1), frequency = 52)
biket2 = ts(bike$t2,start = c(2012,1), frequency = 52)
bikehumidity = ts(bike$hum,start = c(2012,1), frequency = 52)
bikewindspeed = ts(bike$ws,start = c(2012,1), frequency = 52)

#if monthly dataset
bikeCount = ts(bike$cnt,start = c(2012,1), frequency = 12)
biket1 = ts(bike$t1,start = c(2012,1), frequency = 12)
biket2 = ts(bike$t2,start = c(2012,1), frequency = 12)
bikehumidity = ts(bike$hum,start = c(2012,1), frequency = 12)
bikewindspeed = ts(bike$ws,start = c(2012,1), frequency = 12)

#if yearly/annual dataset
bikeCount = ts(bike$cnt,start = c(2012,1), frequency = 1)
biket1 = ts(bike$t1,start = c(2012,1), frequency = 1)
biket2 = ts(bike$t2,start = c(2012,1), frequency = 1)
bikehumidity = ts(bike$hum,start = c(2012,1), frequency = 1)
bikewindspeed = ts(bike$ws,start = c(2012,1), frequency = 1)


autoplot(bikeCount)
autoplot(biket1)
autoplot(biket2)
autoplot(bikehumidity)
autoplot(bikewindspeed)

#Histogram and Q-Q plot

hist(bikeCount, xlab = "Count", prob = TRUE, main = "Histogram")
xfit <- seq(min(bikeCount), max(bikeCount), length = 40)
yfit <- dnorm(xfit, mean = mean(bikeCount), sd = sd(bikeCount))
lines(xfit, yfit, col="blue", lwd = 2)
qqnorm(bikeCount)
qqline(bikeCount, col = 2)

hist(biket1, xlab = "Temperature", prob = TRUE, main = "Histogram")
xfit <- seq(min(biket1), max(biket1), length = 40)
yfit <- dnorm(xfit, mean = mean(biket1), sd = sd(biket1))
lines(xfit, yfit, col="blue", lwd = 2)
qqnorm(biket1)
qqline(biket1, col = 2)

hist(biket2, xlab = "Feels Like Temperature", prob = TRUE, main = "Histogram")
xfit <- seq(min(biket2), max(biket2), length = 40)
yfit <- dnorm(xfit, mean = mean(biket2), sd = sd(biket2))
lines(xfit, yfit, col="blue", lwd = 2)
qqnorm(biket2)
qqline(biket2, col = 2)

hist(bikehumidity, xlab = "Humidity", prob = TRUE, main = "Histogram")
xfit <- seq(min(bikehumidity), max(bikehumidity), length = 40)
yfit <- dnorm(xfit, mean = mean(bikehumidity), sd = sd(bikehumidity))
lines(xfit, yfit, col="blue", lwd = 2)
qqnorm(bikehumidity)
qqline(bikehumidity, col = 2)

hist(bikewindspeed, xlab = "Wind Speed", prob = TRUE, main = "Histogram")
xfit <- seq(min(bikewindspeed), max(bikewindspeed), length = 40)
yfit <- dnorm(xfit, mean = mean(bikewindspeed), sd = sd(bikewindspeed))
lines(xfit, yfit, col="blue", lwd = 2)
qqnorm(bikewindspeed)
qqline(bikewindspeed, col = 2)


# Perform Jarque-Bera normality test.

normalTest(bikeCount,method=c('jb'))  
normalTest(biket1,method=c('jb'))  
normalTest(biket2,method=c('jb'))  
normalTest(bikehumidity,method=c('jb'))  
normalTest(bikewindspeed,method=c('jb'))  


###############################################################
#Testing autocorrelation and Applying ADF and KPSS test 
###############################################################

#########################COUNT VARIABLE########################
##################### Yue Hou ##################

# Step 1: Examine time plot, ACF and PACF of time series.
Acf(bikeCount, lag.max=100)
Pacf(bikeCount, lag.max=100)
eacf(bikeCount)

# unit root test
adfTest(bikeCount, type="nc")
adfTest(bikeCount, type="c") 
adfTest(bikeCount, type="ct") 
kpss.test(bikeCount, null = "Level") 
kpss.test(bikeCount, null = "Trend") 


# Step 2: Apply differencing
Acf(diff(bikeCount), lag.max=100)

# Step 3: Evaluate if stationarity assumptions are satisfied
adfTest(diff(bikeCount), type="nc")
adfTest(diff(bikeCount), type="c") 
adfTest(diff(bikeCount), type="ct") 
kpss.test(diff(bikeCount), null = "Level") 
kpss.test(diff(bikeCount), null = "Trend") 

# Step 4: Seasonal Model
fit1 = Arima(bikeCount, order=c(1, 0, 0), seasonal=list(order=c(1, 1, 0), seasonal=52))
fit1
coeftest(fit1)

fit2 = auto.arima(bikeCount, seasonal = T)
fit2
coeftest(fit2)

# Step 5: Run usual diagnostics to check model adequacy
autoplot(fit1$residuals)
Acf(fit1$residuals, lag.max=100)
Box.test(fit1$residuals, lag=10, type="Ljung")

# Step 6: Backtest
source("Backtest.R")

n = length(bikeCount)
b1 = backtest(fit1, cntTS, h=1, orig=.8*n)
b2 = backtest(fit2, cntTS, h=1, orig=.8*n) # backtest not working on auto.arima, plz fix it if you could

# Step 7: Validation
# prediction validation
length(bikeCount)
train = subset(bikeCount, end=242)
validate = subset(bikeCount, start=243)


fit = Arima(train, order=c(1, 0, 0), seasonal=list(order=c(1, 1, 0), seasonal=52))

ptrain = forecast(fit, h=20)
plot(ptrain,xlim=c(2012,2017))
lines(validate, col='red')

# Step 8: Compute forecasts
plot(forecast(fit1, xreg=2017:2019))
title(sub = "Forecast of London Bike Sharing Counts from 2017 to 2020")

########################T1 VARIABLE########################
##################### Mengfan Ying ##################
autoplot(biket1)

# Investigate autocorrelation
Acf(biket1,lag.max=130)    
pacf(biket1,lag.max=130)   
eacf(biket1)   

# stationarity?
#Applying ADF and KPSS test 
autoplot(biket1)
mean(diff(biket1))
t.test(diff(biket1))  

# Check the ADF test
adfTest(biket1, type="nc")
adfTest(biket1, type="c")   
adfTest(biket1, type="ct") 
# Check the KPSS test
kpss.test(biket1, null="Level")  
kpss.test(biket1, null="Trend") 

#Apply differencing
Acf(diff(biket1), lag.max=130)
Pacf(diff(biket1), lag.max=130)
eacf(diff(biket1))

#Evaluate if stationarity assumptions are satisfied
adfTest(diff(biket1), type="nc")
adfTest(diff(biket1), type="c") 
adfTest(diff(biket1), type="ct") 
kpss.test(diff(biket1), null = "Level") 
kpss.test(diff(biket1), null = "Trend") 


#MODEL BUILDING

#Seasonal Model
fitM = Arima(biket1, order=c(2, 1, 1), seasonal=list(order=c(0, 1, 1), seasonal=52)) 
summary(fitM)
coeftest(fitM)

fitA = auto.arima(biket1, seasonal = T)
summary(fitA)
coeftest(fitA)

#Run usual diagnostics to check model adequacy
autoplot(fitM$residuals) 
Acf(fitM$residuals, lag.max=100)
Box.test(fitM$residuals, lag=10, type="Ljung")

autoplot(fitA$residuals) 
Acf(fitA$residuals, lag.max=100)
Box.test(fitA$residuals, lag=10, type="Ljung")

#COMPUTE LJUNG-BOX TEST FOR WHITE NOISE (NO AUTOCORRELATION)

Box.test(fitA$residuals, lag=10, type="Ljung")
Box.test(fitM$residuals, lag=10, type="Ljung")

#Backtest
source("Backtest.R")

n = length(biket1)
n
bA = backtest(fitA, biket1, h=1, orig=0.8*n)
bM = backtest(fitM, biket1, h=1, orig=0.8*n)

#Compute forecasts
plot(forecast(fitM, xreg=2017:2019))
title(sub = "Forecast of London Bike Sharing temperature from 2017 to 2020")

#Prediction validation
length(biket1)
train = subset(biket1, end=242)
validate = subset(biket1, start=243)

fit = Arima(train, order=c(2, 1, 1), seasonal=list(order=c(0, 1, 1), seasonal=52))
fit

ptrain = forecast(fit, h=20)
plot(ptrain,xlim=c(2012,2017))
lines(validate, col='red')


#########################HUMIDITY VARIABLE########################
##########  Haoyu Lu ##############
plot(cbind(bikeCount,biket1,biket2,bikehumidity, bikewindspeed), xlab = "Weeks", main = 'Variables time plot ')

autoplot(bikehumidity, main = "London Humidity from Jan 2012 to Jan 2017")
plot(decompose(bikehumidity))

# Step 1: Examine time plot, ACF and PACF of time series.
Acf(bikehumidity, lag.max=100)
Pacf(bikehumidity, lag.max=100)
eacf(bikehumidity)

# unit root test
adfTest(bikehumidity, type="nc")
adfTest(bikehumidity, type="c") 
adfTest(bikehumidity, type="ct") 
kpss.test(bikehumidity, null = "Level") 
kpss.test(bikehumidity, null = "Trend") 

# Step 2: Apply differencing
Acf(diff(bikehumidity), lag.max=100)
Pacf(diff(bikehumidity), lag.max=100)
eacf(diff(bikehumidity))

# Step 3: Evaluate if stationarity assumptions are satisfied
t.test(diff(bikehumidity))

adfTest(diff(bikehumidity), type="nc")
adfTest(diff(bikehumidity), type="c") 
adfTest(diff(bikehumidity), type="ct") 
kpss.test(diff(bikehumidity), null = "Level") 
kpss.test(diff(bikehumidity), null = "Trend") 

# Step 4: Seasonal Model
fit1 = Arima(bikehumidity, order=c(1, 0, 2), seasonal=list(order=c(1, 1, 0), seasonal=52))
fit1
coeftest(fit1)

fit2 = auto.arima(bikehumidity, seasonal = T)
fit2
coeftest(fit2)

fit3 = Arima(bikehumidity, order=c(1, 0, 1), seasonal=list(order=c(0, 1, 1), seasonal=52))
fit3
coeftest(fit3)

# Step 5: Run usual diagnostics to check model adequacy
autoplot(fit1$residuals)
Acf(fit1$residuals, lag.max=100)
Box.test(fit1$residuals, lag=10, type="Ljung")

autoplot(fit2$residuals)
Acf(fit2$residuals, lag.max=100)
Box.test(fit2$residuals, lag=10, type="Ljung")

Acf(fit3$residuals, lag.max=100)
Box.test(fit3$residuals, lag=10, type="Ljung")

# Step 6: Backtest
source("Backtest.R")

n = length(bikehumidity)
b1 = backtest(fit1, bikehumidity, h=1, orig = 0.8*n)
b1 = backtest(fit2, bikehumidity, h=1, orig = 0.8*n)
b1 = backtest(fit3, bikehumidity, h=1, orig = 0.8*n)

# Step 7: Compute forecasts
length(bikehumidity)
train = subset(bikehumidity, end=242)
validate = subset(bikehumidity, start=243)
fit = Arima(train, order=c(1, 0, 0), seasonal=list(order=c(1, 1, 0), seasonal=52))
ptrain = forecast(fit, h=20)
plot(ptrain,xlim=c(2012,2017))
lines(validate, col='red')

plot(forecast(fit3, h = 365/7*2))
title(sub = "Forecast of London humidity from 2017 to 2019")

#########################WINDSPEED VARIABLE########################
########## Meet Patel ##############

# Investigate autocorrelation
Acf(bikewindspeed,lag.max=130)    
pacf(bikewindspeed,lag.max=130)   
eacf(bikewindspeed)   

# stationarity?
#Applying ADF and KPSS test 

autoplot(bikewindspeed)
mean(diff(bikewindspeed))
t.test(diff(bikewindspeed))  

# Check the ADF test
adfTest(bikewindspeed, type="nc")
adfTest(bikewindspeed, type="c")   
adfTest(bikewindspeed, type="ct") 
# Check the KPSS test
kpss.test(bikewindspeed, null="Level")  
kpss.test(bikewindspeed, null="Trend") 

#Apply differencing
Acf(diff(windts), lag.max=130)
Pacf(diff(windts), lag.max=130)
eacf(diff(windts))

#Evaluate if stationarity assumptions are satisfied
adfTest(diff(bikewindspeed), type="nc")
adfTest(diff(bikewindspeed), type="c") 
adfTest(diff(bikewindspeed), type="ct") 
kpss.test(diff(bikewindspeed), null = "Level") 
kpss.test(diff(bikewindspeed), null = "Trend") 


#MODEL BUILDING

#Seasonal Model
fit1 = Arima(bikewindspeed, order=c(1, 0, 2), seasonal=list(order=c(1, 1, 0), seasonal=52))
fit1
coeftest(fit1)

fit2 = auto.arima(bikewindspeed, seasonal = T)
fit2
coeftest(fit2)


#Run usual diagnostics to check model adequacy
autoplot(fit1$residuals) #fit1/fit2
Acf(fit1$residuals, lag.max=100)
Acf(fit2$residuals, lag.max=100)

#COMPUTE LJUNG-BOX TEST FOR WHITE NOISE (NO AUTOCORRELATION)

Box.test(fit1$residuals, lag=10, type="Ljung")
Box.test(fit2$residuals, lag=10, type="Ljung")

#Backtest
source("Backtest.R")

n = length(bikewindspeed)
n
b1 = backtest(fit1, bikewindspeed, h=150, orig=0.9*n)
b2 = backtest(fit2, bikewindspeed, h=1, orig=0.8*n) 

#Compute forecasts
plot(forecast(fit1, xreg=2017:2019))
title(sub = "Forecast of London Bike Sharing Counts from 2017 to 2020")

#Prediction validation
length(bikewindspeed)
train = subset(bikewindspeed, end=242)
validate = subset(bikewindspeed, start=243)

fit = Arima(train, order=c(1, 0, 2), seasonal=list(order=c(1, 1, 0), seasonal=52))
fit

ptrain = forecast(fit, h=20)
plot(ptrain,xlim=c(2012,2017))
lines(validate, col='red')

####################################################################################################

######### Harmonic Regression #############
##########  Haoyu Lu ##############
library(dynlm)
library(astsa)
library(vars)
humidity = bikehumidity[,1]

loss = loess(humidity ~ time(humidity))
plot(loss$fitted, type = 'l')
loess10 = loess(humidity ~ time(humidity), data = humidity, span = 0.1)
plot(loess10$fitted, type = 'l')
smoothed10 = predict(loess10)
smoothed10_ts = ts(smoothed10, start = 2012, frequency = frequency(humidity))

loess30 = loess(humidity ~ time(humidity), data = humidity, span=0.3)
plot(loess30$fitted, type = 'l')
smoothed30 = predict(loess30)
smoothed30_ts = ts(smoothed30, start = 2012, frequency = frequency(humidity))
plot(humidity)
lines(smoothed10_ts, col="red", lwd=2)
lines(smoothed30_ts, col="blue", lwd=2)

res10 = ts(loess10$residuals, start = 2012, frequency = frequency(humidity))
head(res10)
head(gasoline)
autoplot(res10)

spect = spectrum(res10, log="no", spans=c(2, 2), plot=T, xlab="Frequency (Cycles/Year)")

fit1 = auto.arima(humidity, xreg = fourier(humidity, K=2),seasonal = FALSE)
summary(fit1)
coeftest(fit1)

autoplot(fit1$residuals)
Acf(fit1$residuals, lag.max = 50)
Box.test(fit1$residuals, lag = 50, type = 'Ljung-Box')

autoplot(forecast(fit1, xreg = fourier(humidity, K=2, h=100)))

fit2 = auto.arima(humidity, xreg = fourier(humidity, K=3),seasonal = FALSE)
summary(fit2)
coeftest(fit2)

autoplot(fit2$residuals)
Acf(fit2$residuals, lag.max = 100)
Box.test(fit2$residuals, lag = 50, type = 'Ljung-Box')

autoplot(forecast(fit2, xreg = fourier(humidity, K=3, h=100)))

fit3 = auto.arima(humidity, xreg = fourier(humidity, K=6),seasonal = FALSE)
summary(fit3)
coeftest(fit3)

autoplot(fit3$residuals)
Acf(fit3$residuals, lag.max = 50)
Box.test(fit3$residuals, lag = 50, type = 'Ljung-Box')

autoplot(forecast(fit2, xreg = fourier(humidity, K=3, h=100)))


#Create lag plots of Cnt with each of the predictors 
lag2.plot(bikeCount, biket1, 8)     # t0, 0.62
lag2.plot(bikeCount, biket2, 8)     # t0, 0.62
lag2.plot(bikeCount, bikehumidity, 8)  # t0, -0.41
lag2.plot(bikeCount, bikewindspeed, 8)  # t0, -0.16

s = as.zoo(ts.intersect(bikeCount, t1=lag(biket1, 0), t2=lag(biket2, 0), humi=lag(bikehumidity, 0), ws=lag(bikewindspeed, 0)))
fitLag1 = Arima(s$bikeCount, xreg = cbind(s$t1, s$t2, s$humi, s$ws), order = c(0, 0, 0))
summary(fitLag1)
coeftest(fitLag1)

acf(fitLag1$residuals)
acf(fitLag1$residuals^2)
Box.test(fitLag1$residuals, lag = 50, type = 'Ljung-Box')

### Weekly
lag2.plot(bikeCount_week, biket1_week, 8)     # t0, 0.86
lag2.plot(bikeCount_week, biket2_week, 8)     # t0, 0.86
lag2.plot(bikeCount_week, bikehum_week, 8)  # t3, 0.78
lag2.plot(bikeCount_week, bikews_week, 8)  # t0, -0.34

s = as.zoo(ts.intersect(bikeCount_week, t1=lag(biket1_week, 0), t2=lag(biket2_week, 0), humi=lag(bikehum_week, 3), ws=lag(bikews_week, 0)))
fitLag2 = Arima(s$bikeCount_week, xreg = cbind(s$t1, s$t2, s$humi, s$ws), order = c(0, 0, 0))
summary(fitLag2)
coeftest(fitLag2)
acf(fitLag2$residuals)
acf(fitLag2$residuals^2)
Box.test(fitLag2$residuals, lag = 50, type = 'Ljung-Box')

######################### Regress cnt on t1 ########################
############################ Mengfan Ying ##########################
autoplot(bikeCount)
autoplot(biket1)

# Correlation analysis
cor(bikeCount, biket1)
s = ts(cbind(bikeCount, biket1), class="mts") 
autoplot(s, facets = T)   # Difficult to see lag  

# apply lag2.plot and ccf to check lagged regression
library(astsa)
lag2.plot(as.numeric(bikeCount), as.numeric(biket1), 8) #convert to numeric
ccf(as.numeric(bikeCount), as.numeric(biket1))

# conduct an OLS regression
fit0 = lm(bikeCount ~ biket1)
summary(fit0)
# investigate residuals
plot(fit0$residuals, type="l")
Acf(fit0$residuals, lag.max = 100)
pacf(fit0$residuals, lag.max = 100)
eacf (fit0$residuals)

#MODEL BUILDING

fitA = auto.arima(bikeCount, xreg=biket1)
summary(fitA)
coeftest(fitA)

fitM = Arima(bikeCount, xreg=biket1, order=c(1, 1, 1), seasonal=list(order=c(1, 0, 0), seasonal=52))
summary(fitM)
coeftest(fitM)

#Run usual diagnostics to check model adequacy
autoplot(fitM$residuals) 
Acf(fitM$residuals, lag.max=100)
Box.test(fitM$residuals, lag=10, type="Ljung")

autoplot(fitA$residuals) #fitM/fitA
Acf(fitA$residuals, lag.max=100)
Box.test(fitA$residuals, lag=10, type="Ljung")

#COMPUTE LJUNG-BOX TEST FOR WHITE NOISE (NO AUTOCORRELATION)

Box.test(fitA$residuals, lag=10, type="Ljung")
Box.test(fitM$residuals, lag=10, type="Ljung")

#Backtest
source("Backtest.R")

n = length(bikeCount)
n
bA = backtest(fitA, bikeCount, xre=biket1, h=1, orig=0.8*n)
bM = backtest(fitM, bikeCount, xre=biket1, h=1, orig=0.8*n)

#Compute forecasts
plot(forecast(fitM, xreg=biket1), xlim=c(2012, 2020))
title(sub = "Forecast of London Bike Sharing Counts from 2017 to 2020")

#Prediction validation
# prediction validation
# n = length(cntTS)
# n # 262
# n * .8 # 209
C_train = subset(bikeCount, end=242)    
T_train = subset(biket1, end=242)
C_test = subset(bikeCount, start=243)
T_test = subset(biket1, start=243)

fitTrain = Arima(C_train, xreg=T_train, order=c(1, 1, 1), seasonal=list(order=c(1, 0, 0), seasonal=52))

plot(forecast(fitTrain, xreg=T_test)) 
lines(as.numeric(time(C_test)), as.numeric(C_test), col="red")

############################################################################
###################Yue Hou - VAR##################################

ccf(bikeCount[,1], biket1[,1])
ccf(bikeCount[,1], bikehumidity[,1])
ccf(bikeCount[,1], bikewindspeed[,1])

# count vs. t1
s1 = VARselect(cbind(bikeCount, biket1), lag.max=8, type="const")
s1
s1$selection

fit1 = VAR(cbind(bikeCount, biket1), p=1, type="const")
fit1
serial.test(fit1, lags.pt=10, type="PT.asymptotic")
autoplot(forecast(fit1, h=15))

# count vs hum
s2 = VARselect(cbind(bikeCount, bikehumidity), lag.max=8, type="const")
s2
s2$selections

fit2 = VAR(cbind(bikeCount, bikehumidity), p=2, type="const")
fit2
serial.test(fit2, lags.pt=10, type="PT.asymptotic")
autoplot(forecast(fit2, h=15))

# count vs ws
s3 = VARselect(cbind(bikeCount, bikewindspeed), lag.max=8, type="const")
s3
s3$selection

fit3 = VAR(cbind(bikeCount, bikewindspeed), p=1, type="const")
fit3
serial.test(fit3, lags.pt=10, type="PT.asymptotic")
autoplot(forecast(fit3, h=15))

###############################################################
# VAR Validation
###############################################################

# Now, let's reserve a validation set at the end and compare this to separate regression
# We will take approximately an 90/10 split by reserving 17 entries off the end for validation 
cTrain = subset(bikeCount, end = 235)
cTest = subset(bikeCount, start = 236)
tTrain = subset(biket1, end = 235)
tTest = subset(biket1, start = 236)

fitCombined = VAR(cbind(cTrain, tTrain), p=1, type="const")
f = forecast(fitCombined, h=27, newdata=cbind(cTest, tTest))
class(f$forecast)

fore = f$forecast

plot(fore$cTrain,xlim=c(2012,2017))
lines(cTest, col = 'red')

plot(fore$tTrain,xlim=c(2012,2017))
lines(tTest, col = 'red')


