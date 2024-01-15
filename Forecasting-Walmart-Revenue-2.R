## USE FORECAST LIBRARY.
library(forecast)

# Set working directory for locating files.
setwd("C:/Users/STSC/Desktop/Time Series/Time Series Case 3")

# Create data frame.
walmart.data <- read.csv("673_case2(1).csv")

# create time series data set using ts() function
walmart.ts <- ts(walmart.data$Revenue, 
                 start = c(2005, 1), end = c(2022, 4), freq = 4)

# Print the data set
walmart.ts

# Define the length of the training and validation set
nValid <- 16 
nTrain <- length(walmart.ts) - nValid

# Split the data into training and validation sets
train.ts <- window(walmart.ts, start = c(2005, 1), end = c(2018, 4))
valid.ts <- window(walmart.ts, start = c(2019, 1), end = c(2022, 4))

# Print the training and validation sets
train.ts
valid.ts



# QUESTION 1(A)

# Use Arima() function to fit AR(1) model for the historical data.
walmart.ar1 <- Arima(walmart.ts, order = c(1,0,0))

# Use summary() to identify parameters of AR(1) model. 
summary(walmart.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.9269
s.e. <- 0.0525
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}



# QUESTION 1(B)

# Create first difference of Walmart Revenue data using diff() function.
diff.walmart <- diff(walmart.ts, lag = 1)
diff.walmart


# Use Acf() function to identify autocorrelation for first differencing (lag1) 
# the historical data and plot autocorrelation for different lags 
# (up to maximum of 8).
Acf(diff.walmart, lag.max = 8, 
    main = "The Autocorrelation for the First Differencing (lag1)")



# QUESTION 2(A)

# Create regression model with quadratic trend and seasonality using tslm() function
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary and associated parameters.
summary(train.quad.season)

# Apply forecast() function to make forecast for validation period.
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred



# QUESTION 2(B)

# Use Acf() function to identify autocorrelation for the model residuals 
# (training set), and plot autocorrelation for different 
# lags (up to maximum of 8).
residual <- (train.quad.season.pred$residuals)
Acf(residual, lag.max = 8, 
    main = "Autocorrelation for model’s residuals for training period ")



# QUESTION 2(C)

# Use Arima() function to fit AR(1) model for the regression residuals.
res.ar1 <- Arima(train.quad.season$residuals, order = c(1,0,0))

# Use summary() to identify parameters of AR(1) model. 
summary(res.ar1)

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 8).
Acf(res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation Chart")


# QUESTION 2(D)

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Create two-level model's forecast with quadratic trend and seasonality 
# regression + AR(1) for residuals for validation period.

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# combined forecast for the validation period. 
valid.two.level.pred <- train.quad.season.pred$mean + res.ar1.pred$mean

valid.df <- round(data.frame(valid.ts, train.quad.season.pred$mean, 
                             res.ar1.pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Ridership", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df



# QUESTION 2(E)

## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY FOR ENTIRE DATASET.

# Use tslm() function to create quadratic trend and seasonality model.
quad.season <- tslm(walmart.ts ~ trend + I(trend^2) + season)

# Apply forecast() function to make predictions with quadratic trend and seasonal 
# model into the future 8 quarters.  
quad.season.pred <- forecast(quad.season, h = 8, level = 0)
quad.season.pred

# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 8 quarters.
residual.ar1 <- Arima(quad.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 8, level = 0)
residual.ar1.pred 

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrelation for the residuals of residuals 
# and plot autocorrelation for different lags (up to maximum of 8).
Acf(residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Residuals for Entire Data Set")

# Identify forecast for the future 8 quarters (Q1 - Q4 of 2023 and 2024) as sum 
# of quadratic trend and seasonal model and AR(1) model for residuals.
quad.season.ar1.pred <- quad.season.pred$mean + residual.ar1.pred$mean
quad.season.ar1.pred

# Create a data table with quadratic trend and seasonal forecast 
# for the future 8 quarters (Q1 - Q4 of 2023 and 2024),
# AR(1) model for residuals for the future 8 quarters, and combined 
# two-level forecast for the future 8 quarters. 
table.df <- round(data.frame(quad.season.pred$mean, 
                             residual.ar1.pred$mean, quad.season.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df



# QUESTION 3(A)

## FIT ARIMA(1,1,1)(1,1,1) MODEL.

# Use Arima() function to fit ARIMA(1,1,1)(1,1,1).
train.arima.seas <- Arima(train.ts, order = c(1,1,1), 
                          seasonal = c(1,1,1)) 

# Use summary() to show ARIMA model and its parameters.
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred



# QUESTION 3(B)

## FIT AUTO ARIMA MODEL.

# Use auto.arima() function to fit ARIMA model.
train.auto.arima <- auto.arima(train.ts)

# Use summary() to show auto ARIMA model and its parameters.
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred



# QUESTION 3(C)

# (3a) ARIMA(1,1,1)(1,1,1) model. 
round(accuracy(train.arima.seas.pred$mean, valid.ts), 3)

# (3b) Auto ARIMA model.
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)



# QUESTION 3(D)

# Use arima() function to fit seasonal ARIMA(1,1,1)(1,1,1) model 
# for entire data set.
arima.seas <- Arima(walmart.ts, order = c(1,1,1), 
                    seasonal = c(1,1,1)) 

# use summary() to show auto ARIMA model and its parameters for entire data set.
summary(arima.seas)

# Apply forecast() function to make predictions for ts with 
# seasonal ARIMA model for Q1- Q4 of 2023 and 2024. 
arima.seas.pred <- forecast(arima.seas, h = 8, level = 0)
arima.seas.pred


# Use auto.arima() function to fit ARIMA model for entire data set.
auto.arima <- auto.arima(walmart.ts)

# use summary() to show auto ARIMA model and its parameters for entire data set.
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for Q1- Q4 of 2023 and 2024. 
auto.arima.pred <- forecast(auto.arima, h = 8, level = 0)
auto.arima.pred



# QUESTION 3(E)

# Use accuracy() function to identify common accuracy measures for:

# (1) Regression model with quadratic trend and seasonality
round(accuracy(quad.season.pred$fitted, walmart.ts), 3)

# (2) Two-level model (with AR(1) model for residuals)
round(accuracy(quad.season.pred$fitted + residual.ar1.pred$fitted, walmart.ts), 3)

# (3) Seasonal ARIMA (1,1,1)(1,1,1) Model,
round(accuracy(arima.seas.pred$fitted, walmart.ts), 3)

# (4) Auto ARIMA Model,
round(accuracy(auto.arima.pred$fitted, walmart.ts), 3)

# (5) Seasonal naive forecast
round(accuracy((snaive(walmart.ts))$fitted, walmart.ts), 3)








