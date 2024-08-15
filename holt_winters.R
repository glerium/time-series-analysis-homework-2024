setwd('~/data')
library(forecast, quietly = TRUE)

data = read.csv("货币供应量-月度.csv")
data = subset(data, select = -时间)
data = data[rev(1:nrow(data)), ]

ts_data <- ts(data$流通中现金.M0.供应量期末值.亿元., frequency = 12)
hw_model <- hw(ts_data, seasonal = "additive")
plot(hw_model, main = "Values and predictions of M0")
forecast_values <- forecast(hw_model, h = 24)
plot(hw_model$model)

ts_data <- ts(data$流通中现金.M0.供应量同比增长..., frequency = 12)
hw_model <- hw(ts_data, seasonal = "additive")
plot(hw_model, main = "Values and predictions of M0 Delta")
forecast_values <- forecast(hw_model, h = 24)
plot(hw_model$model)

ts_data <- ts(data$货币.M1.供应量期末值.亿元., frequency = 12)
hw_model <- hw(ts_data, seasonal = "additive")
plot(hw_model, main = "Values and predictions of M1")
forecast_values <- forecast(hw_model, h = 24)
plot(hw_model$model)

ts_data <- ts(data$货币.M1.供应量同比增长..., frequency = 12)
hw_model <- hw(ts_data, seasonal = "additive")
plot(hw_model, main = "Values and predictions of M1 Delta")
forecast_values <- forecast(hw_model, h = 24)
plot(hw_model$model)

ts_data <- ts(data$货币和准货币.M2.供应量期末值.亿元., frequency = 12)
hw_model <- hw(ts_data, seasonal = "additive")
plot(hw_model, main = "Values and predictions of M2")
forecast_values <- forecast(hw_model, h = 24)
plot(hw_model$model)

ts_data <- ts(data$货币和准货币.M2.供应量同比增长..., frequency = 12)
hw_model <- hw(ts_data, seasonal = "additive")
plot(hw_model, main = "Values and predictions of M2 Delta")
forecast_values <- forecast(hw_model, h = 24)
plot(hw_model$model)
