library(ggfortify)
library(forecast)

data <- read.csv("/home/david/Documents/Semester10/MTH5324/Project/USA-bounded-2012.csv")

data$quarter
war_start_index <- which(data$quarter == "2022Q1")
war_start_index

pre_war <- data[0:(war_start_index-1),]
pre_war
pre_war[] <- lapply(pre_war, as.numeric)
sapply(data, class)

war_data <- data[(war_start_index):nrow(data), ]
war_data

quarter_indx <- grep("quarter", colnames(data))
pre_war <- pre_war[,-quarter_indx]

ind_vars <- pre_war[ , c("CPI", "INT", "UNEMP", "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]
# ind_vars <- pre_war[ , c("INT", "UNEMP", "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]

arima_model <- arima(pre_war$GDP, order=c(1,1,1))
fcast <- forecast(arima_model, h=50)
plot(fcast)


model <- Arima(pre_war$GDP, order = c(1, 1, 1), xreg = as.matrix(ind_vars))
# corr_matrix <- cor(as.numeric(pre_war))
# fcast <- forecast( model, xreg = as.matrix(ind_vars), h=1000 )
# fcast <- forecast( auto.arima(pre_war), xreg = as.matrix(ind_vars), h=1000 )
ind_vars <- war_data[ , c("CPI", "INT", "UNEMP", "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]
# ind_vars <- pre_war[ , c("INT", "UNEMP", "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]
fcast <- forecast( model, xreg = as.matrix(ind_vars), h=10 )
fcast$x
fcast
plot(fcast)
autoplot(fcast, forecast.end =53)
               
# pre_war$GDP
# pred <- predict(model, n.ahead=8,  newxreg = as.matrix(ind_vars))
# pred <- forecast(model, n.ahead=4,  newxreg = as.matrix(ind_vars))
# pred
# autoplot(pred)
# pred[2]

