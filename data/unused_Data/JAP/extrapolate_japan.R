install.packages("forecast")
library(forecast)

 data <- read.csv("/home/david/Documents/Semester10/MTH5324/Project/JAP-bounded.csv")
spending <- data$SPENDING_PERCENT_OF_GDP
spending <- as.numeric(spending)
 spending
 arima_model <- arima(spending, order = c(1, 0, 0))
 
 forecast_arima <- forecast(arima_model, h = 1)
 
 # View the forecasted value
 print(forecast_arima$mean)
 
 plot(forecast_arima)
 