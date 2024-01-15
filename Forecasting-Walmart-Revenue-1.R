## USE FORECAST LIBRARY.

library(forecast)

# Set working directory for locating files.
setwd("C:/Users/STSC/Downloads/Time Series Case 2")

# Create data frame.
walmart.data <- read.csv("673_case2(1).csv")


## Question 1(a)

# create time series data set using ts() function
walmart.ts <- ts(walmart.data$Revenue, 
                   start = c(2005, 1), end = c(2022, 4), freq = 4)

# Print the data set
walmart.ts



## Question 1(b)

# Create a data plot with the historical data using the plot() function 
plot(walmart.ts, main = "Walmart Quarterly Revenue (2005-2022)", xlab = "Year", 
     ylab = "Revenue (in millions of dollars)")



## Question 2(a)

# Define the length of the training and validation set
nValid <- 16 
nTrain <- length(walmart.ts) - nValid

# Split the data into training and validation sets
train.ts <- window(walmart.ts, start = c(2005, 1), end = c(2018, 4))
valid.ts <- window(walmart.ts, start = c(2019, 1), 
                   end = c(2022, 4))



## Question 2(b)

## MODEL 1: Regression model with linear trend
# Create regression model with linear trend using tslm() function
train.lin <- tslm(train.ts ~ trend)

# See summary and associated parameters.
summary(train.lin)

# Apply forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred


## MODEL 2: Regression mode with quadratic trend
# Create regression model with quadratic trend using tslm() function
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary and associated parameters.
summary(train.quad)

# Apply forecast() function to make forecast for validation period.
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred


## MODEL 3: Regression mode with seasonality
# Create regression model with seasonality using tslm() function
train.season <- tslm(train.ts ~ season)

# See summary and associated parameters.
summary(train.season)

# Apply forecast() function to make forecast for validation period.
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred


## MODEL 4: Regression mode with linear trend and seasonality
# Create regression model with linear trend and seasonality using tslm() function
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make forecast for validation period.
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred


## MODEL 5: Regression mode with quadratic trend and seasonality
# Create regression model with quadratic trend and seasonality using tslm() function
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary and associated parameters.
summary(train.quad.season)

# Apply forecast() function to make forecast for validation period.
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred



## Question 2(c)

## MODEL 1: Regression model with linear trend
# To identify common accuracy measures using accuracy() function
round(accuracy(train.lin.pred$mean, valid.ts), 3)

## MODEL 2: Regression mode with quadratic trend
# To identify common accuracy measures using accuracy() function
round(accuracy(train.quad.pred$mean, valid.ts), 3)

## MODEL 3: Regression mode with seasonality
# To identify common accuracy measures using accuracy() function
round(accuracy(train.season.pred$mean, valid.ts), 3)

## MODEL 4: Regression mode with linear trend and seasonality
# To identify common accuracy measures using accuracy() function
round(accuracy(train.lin.season.pred$mean, valid.ts),3)

## MODEL 5: Regression mode with quadratic trend and seasonality
# To identify common accuracy measures using accuracy() function
round(accuracy(train.quad.season.pred$mean, valid.ts),3)



## Question 3(a)

## MODEL 1: Regression mode with linear trend
# Create regression model with linear trend using tslm() function
lin.trend <- tslm(walmart.ts ~ trend)

# See summary and associated parameters.
summary(lin.trend)

# Apply forecast() function to make forecast for entire dataset.
lin.trend.pred <- forecast(lin.trend, h = 8, level = 0)
lin.trend.pred


## MODEL 4: Regression mode with linear trend and seasonality 
# Create regression model with linear trend and seasonality using tslm() function
lin.season <- tslm(walmart.ts ~ trend + season)

# See summary and associated parameters.
summary(lin.season)

# Apply forecast() function to make forecast for entire dataset.
lin.season.pred <- forecast(lin.season, h = 8, level = 0)
lin.season.pred


## MODEL 5: Regression mode with quadratic trend and seasonality 
# Create regression model with quadratic trend and seasonality using tslm() function
quad.season <- tslm(walmart.ts ~ trend + I(trend^2) + season)

# See summary and associated parameters.
summary(quad.season)

# Apply forecast() function to make forecast for entire dataset.
quad.season.pred <- forecast(quad.season, h = 8, level = 0)
quad.season.pred



## Quetsion 3(b)

## MODEL 2: Regression mode with quadratic trend
# To identify common accuracy measures using accuracy() function
round(accuracy(lin.trend.pred$fitted, walmart.ts),3)

## MODEL 4: Regression mode with linear trend and seasonality 
# To identify common accuracy measures using accuracy() function
round(accuracy(lin.season.pred$fitted, walmart.ts),3)

## MODEL 5: Regression mode with quadratic trend and seasonality 
# To identify common accuracy measures using accuracy() function
round(accuracy(quad.season.pred$fitted, walmart.ts),3)

## Naive Model
# To identify common accuracy measures using accuracy() function
round(accuracy((naive(walmart.ts))$fitted, walmart.ts), 3)

## Seasonal Naive Model
# To identify common accuracy measures using accuracy() function
round(accuracy((snaive(walmart.ts))$fitted, walmart.ts), 3)









