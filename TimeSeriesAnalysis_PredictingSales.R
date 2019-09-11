rm(list=ls())

###############################################################
# title: 'Time Series Analysis: Case study Predicting Sale'
###############################################################

# # Prologue
# We shall look into time series analysis using R package - forecast. 
# Objective of this case study is to explain the different methods available in 
# forecast package which can be applied while dealing with time series analysis/forecasting. 

# # What is Time Series?
# A [time series](http://en.wikipedia.org/wiki/Time_series) is a collection of observations 
# of well-defined data items obtained through repeated measurements over time. For example, 
# measuring the value of retail sales each month of the year would comprise a time series. 

# Time series analysis and modeling has many business and social applications. 
# It is extensively used to forecast company sales, product demand, stock market trends, 
# agricultural production, etc.

# # Objective:
# In this case study example we will learn about time series analysis for a 
# manufacturing operation.  Our goal is to :
# - Identify patterns in the data - stationarity/non-stationarity. 
# - Prediction from previous patterns.

# # Case Study details:
# PowerHorse, a tractor and farm equipment manufacturing company, was established a 
# few years after World War II. The company has shown a consistent growth in its revenue 
# from tractor sales since its inception. However, over the years the company has struggled 
# to keep it's inventory and production cost down because of variability in sales 
# and tractor demand. The management at PowerHorse is under enormous pressure from the 
# shareholders and board to reduce the production cost. Additionally, they are also 
# interested in understanding the impact of their marketing and farmer connect efforts 
# towards overall sales. In the same effort, they have hired you as a data science and 
# predictive analytics consultant.

# # Task
# You will start your investigation of this problem. Eventually, you will develop 
# an ARIMA model to forecast sale / demand for next year. Additionally, you will 
# also investigate the impact of marketing program on sales by using an exogenous 
# variable ARIMA model.

# # Data
# You are provided with the monthly sales figure from January 2003 to December 2014. 
# This data resides in the file "Tractor-Sales.csv"

# # About the data
# The production units you are analysing is based in South East Asia. This unit is 
# completely independent and caters to neighbouring geographies. This unit is just a 
# decade and a half old. In 2014 , they captured 11% of the market share, a 14% increase 
# from the previous year. However, being a new unit they have very little bargaining 
# power with their suppliers to implement Just-in-Time (JiT) manufacturing principles 
# that have worked really well in PowerHorse's base location. Hence, they want to be 
# on top of their production planning to maintain healthy business margins. Monthly 
# sales forecast is the first step you have suggested to this unit towards effective 
# inventory management.

# In the same effort, you asked the MIS team to share month on month (MoM) sales figures 
# (number of tractors sold) for the last 12 years. The following is the time series plot for 
# the same

# ## Load the data

data<-read.csv("Tractor-Sales.csv") 
data 

# ## Analysis of the data
# Create time series object from the data.

data <- ts(data[,2],start = c(2003,1),frequency = 12)
data

# The following is the time series plot for the same

par(mfrow=c(1,1))
plot(data, 
     xlab="Years",           # x axis lable
     ylab = "Tractor Sales",  # y axis lable
     main=" Monthly sale across years",  ## Chart title
     pch=20,  # point character
     col="red", # color of points,
     type="b"   # both points and line connecting
     ) 

par(mfrow=c(3,1))
plot(data, 
     xlab="Years",           # x axis lable
     ylab = "Tractor Sales",  # y axis lable
     main=" Monthly sale across years",  ## Chart title
     pch=20,  # point character
     col="red", # color of points,
     type="b"   # both points and line connecting
)
plot(diff(data), ylab="Differenced Tractor Sales", main="Differenced Tractor Sales over the years") 
plot(log10(data),ylab="Log (Tractor Sales)",main="Log (Tractor Sales) over the years") 

# ## Decomposition of time series

# The fundamental idea for time series analysis is to decompose the original time series 
# (sales, stock market trends, etc.) into several independent components. 
# Typically, business time series are divided into the following four components:

# * Trend -  overall direction of the series i.e. upwards, downwards etc.
# * Seasonality - monthly or quarterly patterns
# * Cycle -  long-term business cycles
# * Irregular remainder - random noise left after extraction of all the components 

# Interference of these components produces the final series.This can be achieved using 
# either additive model for components or multiplicative model for components. 
 
# Now the question is: why bother decomposing the original / actual time series into components? 

# The answer: It is much easier to forecast the individual regular patterns produced 
# through decomposition of time series than the actual series. This is similar to 
# reproduction and forecasting the individual sine waves (A, B, C, and D) 
# as shown in figure below instead of the final irregular pattern produced through the 
# product of these four sine waves.

# ![Multiplicative decomposition of time series](./images/MultiplicativeDecomposition.jpg)
# 
# ![Decomposition of time series](./images/Time-Series-Analysis-Decomposition.jpg)
# 
# In the above data, a cyclic pattern seems to be non-existent since the unit we are 
# analysing is a relatively new unit to notice business cycles. Also in theory, business 
# cycles in traditional businesses are observed over a period of 7 or more years. Hence, 
# you won't include business cycles in this time series decomposition exercise. 
# We will build our model based on the following function:

# $$
# Y_{t} = f(Trend_{t}\ , Seasonality_{t}\ , Remainder_{t}) 
# $$

# Now, we will study each of these components in some detail starting with trend.

### Trend - Time Series Decomposition
# Now, to begin with let's try to decipher trends embedded in the above tractor 
# sales time series. One of the commonly used procedures to do so is moving averages. 
# A good analogy for moving average is ironing clothes to remove wrinkles. The idea with 
# moving average is to remove all the zigzag motion (wrinkles) from the time series 
# to produce a steady trend through averaging adjacent values of a time period. 
# Hence, the formula for moving average is:

# $$ 
# Moving Average = \frac{\sum\limits_{i=-m}^m Y_{t+i}}{2m} 
# $$ 

# Now, let's try to remove wrinkles from our time series using moving average. 
# We will take moving average of different time periods i.e. 4,6,8, and 12 months 
# as shown below. Here, moving average is shown in blue and actual series in orange.

# moving average with period of 4 months
par(mfrow=c(1,1))
library(forecast)

# ma <- function(arr, n=15){
#   res = arr
#   for(i in n:length(arr)){
#     res[i] = mean(arr[(i-n+1):i])
#   }
#   res
# }

ts_4month = ma(data,4) 

plot(data, main="tractor sales: 4 month moving average", ylab="Sale", xlab="Year")
lines(ts_4month,col="red")

# moving average with period of 6 months
plot(data, ylab="Sale", xlab="Year", main="Tractor sale:Smoothing using 6 months moving average")
lines(ma(data,6),col="red")

# moving average with period of 8 months
plot(data,ylab="Sale", xlab="Year",main="Tractor sale:Smoothing using 8 month moving average")
lines(ma(data,8),col="red")

# moving average with period of 10 months
plot(data, ylab="Sale", xlab="Year", main="Tractor sale:Smoothing using 10 months moving average")
lines(ma(data,10),col="red")

# moving average with period of 12 months
plot(data,  ylab="Sale", xlab="Year", main="Tractor sale:Smoothing using 12 months moving average")
lines(ma(data,12),col="red")

# As you could see in the above plots, 12-month moving average could produced 
# a wrinkle free curve as desired. This on some level is expected since we are 
# using month-wise data for our analysis and there is expected monthly-seasonal 
# effect in our data. Now, let's decipher the seasonal component.

### Seasonality - Time Series Decomposition 

# The first thing to do is to see how number of tractors sold vary on a month on 
# month basis. We will plot a stacked annual plot to observe seasonality in our data. 
# As you could see there is a fairly consistent month on month variation with July 
# and August as the peak months for tractor sales.

# Seasonal decomposition

# library(forecast)
# par(mfrow=c(1,1))
seasonplot(data, year.labels = T)
ggseasonplot(data, year.labels = T, col = rainbow(12), type = "o")

### Irregular Remainder - Time Series Decomposition
# To decipher underlying patterns in tractor sales, you build a multiplicative 
# time series decomposition model with the following equation

# $$ 
# Y_{t} = Trend_{t} * Seasonality_{t} * Remainder_{t} 
# $$

# Instead of multiplicative model you could have chosen additive model as well. 
# However, it would have made very little difference in terms of conclusion you 
# will draw from this time series decomposition exercise. Additionally, you are 
# also aware that plain vanilla decomposition models like these are rarely used for 
# forecasting. Their primary purpose is to understand underlying patterns in 
# temporal data to use in more sophisticated analysis like ARIMA.

### Multiplicative decomposition with a single command and a plot

fit <- stl(data, s.window="period")
plot(fit)

### additional plots

# month plot
monthplot(data)

# The following are some of your key observations from this analysis:
# 
# 1) Trend: 12-months moving average looks quite similar to a straight line 
# hence you could have easily used linear regression to estimate the trend in this data.
# 
# 2) Seasonality: as discussed, seasonal plot displays a fairly consistent month-on-month 
# pattern. The monthly seasonal components are average values for a month after removal 
# of trend. Trend is removed from the time series using the following formula:
# 
# $$ Seasonality_{t} * Remainder_{t}={Y_{t}}/{Trend_{t}} $$
# 
# 3) Irregular Remainder (random): is the residual left in the series after removal 
# of trend and seasonal components. Remainder is calculated using the following formula:
# 
# $$Remainder_{t}= {Y_{t}}/{Trend_{t} * Seasonality_{t}} $$
# 
# The expectations from remainder component is that it should look like a white noise 
# i.e. displays no pattern at all. However, for our series residual display some pattern 
# with high variation on the edges of data i.e. near the beginning (2004-07) and 
# the end (2013-14) of the series.
# 
# White noise (randomness) has an important significance in time series modelling. 
# In the later parts of this manufacturing case study, you will use ARIMA models to 
# forecasts sales value. ARIMA modelling is an effort to make the remainder series 
# display white noise patterns.

## Forecast package & methods:
# Forecast package is written by Rob J Hyndman and is available from CRAN [here] 
# (http://cran.r-project.org/web/packages/forecast/index.html). The package contains 
# Methods and tools for displaying and analyzing univariate time series forecasts 
# including exponential smoothing via state space models and automatic ARIMA modelling.
# 
# Before going into more accurate Forecasting functions for Time series, let us do 
# some basic forecasts using Meanf(), naïve(), random walk with drift - rwf() methods. 
# Though these may not give us proper results but we can use the results as bench marks.
# 
# All these forecasting models returns objects which contain original series, 
# point forecasts, forecasting methods used residuals. Below functions shows three 
# methods & their plots.

#### Forecast from mean

mf = meanf(data,h=12,level=c(90,95),fan=FALSE,lambda=NULL)
plot(mf) 

### Forecast from naive method

mn = naive(data,h=12,level=c(90,95),fan=FALSE,lambda=NULL) 
plot(mn) 

### Random walk model forecast

md = rwf(data,h=12,drift=T,level=c(90,95),fan=FALSE,lambda=NULL) 
plot(md) 

## ARIMA model

### Step 1: Plot tractor sales data as time series

# To begin with you have prepared a time series plot for the data. 
# The following is the R code you have used to read the data in R and plot a time series chart.

data<-read.csv("Tractor-Sales.csv")
data<-ts(data[,2],start = c(2003,1),frequency = 12)

plot(data, xlab="Years", ylab = "Tractor Sales")

# Clearly the above chart has an upward trend for tractors sales and there 
# is also a seasonal component that we have already analysed 

### Step 2: Difference data to make data stationary on mean (remove trend)

# The next thing to do is to make the series stationary. This to remove the 
# upward trend through 1st order differencing the series using the following formula:
# $$
# {1st Differencing (d=1) } 	 Y'{t}=Y_t - Y_{t-1}
# $$
# The R code and output for plotting the differenced series is displayed below:

plot(diff(data),ylab="Differenced Tractor Sales")

# Okay so the above series is not stationary on variance i.e. 
# variation in the plot is increasing as we move towards the right of the chart. 
# We need to make the series stationary on variance to produce reliable forecasts 
# through ARIMA models.

### Step 3: log transform data to make data stationary on variance

# One of the best ways to make a series stationary on variance is through 
# transforming the original series through log transform. We will go back to our 
# original tractor sales series and log transform it to make it stationary on 
# variance. The following equation represents the process of log transformation mathematically:
# $$
# \ {Log of sales} Y_{t}^{new} = log_{10}(Y_t) 
# $$
# The following is the R code for the same with the output plot. Notice, this series 
# is not stationary on mean since we are using the original data without differencing.

plot(log10(data),ylab="Log (Tractor Sales)")

# Now the series looks stationary on variance.

### Step 4: Difference log transform data to make data stationary on both mean and variance

# Let us look at the differenced plot for log transformed series to reconfirm if the 
# series is actually stationary on both mean and variance.

# $$
# \text{1st Differencing (d=1) of log of sales }	 Y_{t}^{new'}=log_{10}(Y_t) -log_{10}(Y_{t-1}) 
# $$
# The following is the R code to plot the above mathematical equation.

plot(diff(log10(data)),ylab="Differenced Log (Tractor Sales)",
     main="Differenced Log (Tractor Sales) over the years") 

# Yes, now this series looks stationary on both mean and variance. 
# This also gives us the clue that I or integrated part of our ARIMA model 
# will be equal to 1 as 1st difference is making the series stationary.

### Step 5: Plot ACF and PACF to identify potential AR and MA model

# Now, let us create autocorrelation factor (ACF) and partial autocorrelation 
# factor (PACF) plots to identify patterns in the above data which is stationary 
# on both mean and variance. The idea is to identify presence of AR and MA components 
# in the residuals. The following is the R code to produce ACF and PACF plots.

par(mfrow = c(2,1))
acf(ts(diff(log10(data))),main="ACF Tractor Sales")
pacf(ts(diff(log10(data))),main="PACF Tractor Sales")

# Since, there are enough spikes in the plots outside the insignificant zone (dotted horizontal lines) 
# we can conclude that the residuals are not random. This implies that there is juice or information 
# available in residuals to be extracted by AR and MA models. Also, there is a seasonal component available 
# in the residuals at the lag 12 (represented by spikes at lag 12). This makes sense since we are analyzing 
# monthly data that tends to have seasonality of 12 months because of patterns in tractor sales.

### Step 6: Identification of best fit ARIMA model

# Auto arima function in forecast package in R helps us identify the best fit ARIMA model on the fly. 
# The following is the code for the same. Please install the required 'forecast' 
# package in R before executing this code.

require(forecast) 

ARIMAfit <- auto.arima(log10(data), trace=T) 
summary(ARIMAfit) 

# The best fit model is selected based on Akaike Information Criterion (AIC) , and 
# Bayesian Information Criterion (BIC) values. The idea is to choose a model with 
# minimum AIC and BIC values. The values of AIC and BIC for our best fit model developed 
# in R are displayed at the bottom of the following results:

# As expected, our model has I (or integrated) component equal to 1. This represents 
# differencing of order 1. There is additional differencing of lag 12 in the above best 
# fit model. Moreover, the best fit model has MA value of order 1. Also, there is seasonal 
# MA with lag 12 of order 1.

### Step 7: Forecast sales using the best fit ARIMA model

# The next step is to predict tractor sales for next 3 years i.e. for 2015, 2016, and 2017 
# through the above model. The following R code does this job for us.

pred <- predict(ARIMAfit, n.ahead = 36) 
pred 

# The following is the output with forecasted values of tractor sales in blue. 
# Also, the range of expected error (i.e. 2 times standard deviation) is displayed with 
# orange lines on either side of predicted blue line.

par(mfrow = c(1,1))
plot(data,type="l",xlim=c(2004,2018),ylim=c(1,1600),xlab = "Year",ylab = "Tractor Sales") 

lines(10^(pred$pred),col="blue") 

lines(10^(pred$pred+2*pred$se),col="orange") 

lines(10^(pred$pred-2*pred$se),col="orange") 

# Now, forecasts for a long period like 3 years is an ambitious task. 
# The major assumption here is that the underlining patterns in the time series 
# will continue to stay the same as predicted in the model. A short term forecasting model, 
# say a couple of business quarters or a year, is usually a good idea to forecast with 
# reasonable accuracy. A long term model like the one above needs to evaluated on a 
# regular interval of time (say 6 months). The idea is to incorporate the new information 
# available with the passage of time in the model.

### Step 8: Plot ACF and PACF for residuals of ARIMA model to ensure no more information 
# is left for extraction

# Finally, let's create an ACF and PACF plot of the residuals of our best fit ARIMA model 
# i.e. ARIMA(0,1,1)(0,1,1)[12]. The following is the R code for the same.

par(mfrow=c(1,2)) 
acf(ts(ARIMAfit$residuals),main="ACF Residual") 
pacf(ts(ARIMAfit$residuals),main="PACF Residual") 

# Since there are no spikes outside the insignificant zone for both ACF and PACF plots 
# we can conclude that residuals are random with no information or juice in them. 
# Hence our ARIMA model is working fine.
