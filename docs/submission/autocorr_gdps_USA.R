library(ggfortify)
library(forecast)
library(tseries)

countries <- c("USA", "EURO", "JAP" , "CHINA")
country <- "USA"
country

csv_name <- paste0("/home/david/Documents/Semester10/MTH5324/Project/" , country  , "-bounded-2012.csv")
data <- read.csv(csv_name)

data

if (country == "CHINA") {
  start_index <- which(data$quarter == "2018Q1") 
  data <- data[start_index:nrow(data), 0:ncol(data)]
  data
}

data
data$INT
data$CPI
first <- data$GDP[2]
first/10^6
10^6
data$GDP/(10^6)

nrow(data)

if(country == "JAP" || country == "CHINA") {
  
    if(country == "CHINA") {
      cat(typeof(data$GDP), "\n")
      cat((data$GDP/(10^(6))), "\n")
      data$GDP <- (data$GDP/(10^6)) + runif(n=nrow(data), min=1, max=100)
    }else {
      data$GDP = data$GDP + runif(n=nrow(data), min=1, max=100)
    }
    data$INT = data$INT + runif(n=nrow(data), min=0.001, max=0.2)
    data$UNEMP = data$UNEMP + runif(n=nrow(data), min=0.001, max=0.2)
    data$CPI = data$CPI + runif(n=nrow(data), min=1, max=100)
    data$SPENDING_PERCENT_OF_GDP = data$SPENDING_PERCENT_OF_GDP + runif(n=nrow(data), min=0.001, max=0.001)
}
data

data$quarter
war_start_index <- which(data$quarter == "2022Q1")
war_start_index

pre_war <- data[0:(war_start_index-1),]
pre_war
pre_war[] <- lapply(pre_war, as.numeric)
sapply(data, class)
data
war_data <- data[(war_start_index):nrow(data), ]
war_data

quarter_indx <- grep("quarter", colnames(data))
pre_war <- pre_war[,-quarter_indx]

ind_vars <- pre_war[ , c("INT", "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]
# ind_vars <- pre_war[ , c("INT", "UNEMP", "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]

arima_model <- arima(pre_war$GDP, order=c(1,1,1))
fcast <- forecast(arima_model, h=50)
plot(fcast)


df_name <- "pre_war"
filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/", country, "-" , df_name, "-gdp_acf.png")
title <- paste0("GDP ACF wrt " , country , " for " , df_name , " data")
filename
# title <- paste0("GDP Autocorrelation for ")
png(filename)
labelsD = data$quarter

acf_dat <- acf(pre_war$GDP, main=title)
plot(acf_dat, main=title)
dev.off()

df_name <- "pre_war"


filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/", country, "-" , df_name, "-gdp_pacf.png")
title <- paste0("GDP PACF wrt " , country , " for " , df_name , " data")
filename
# title <- paste0("GDP Autocorrelation for ")
png(filename)
pacf(data$GDP, main=title)
dev.off()

df_name <- "all_data"
filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/", country, "-" , df_name, "-gdp_acf.png")
title <- paste0("GDP ACF wrt " , country , " for " , df_name , " data")
filename
# title <- paste0("GDP Autocorrelation for ")
png(filename)
acf(data$GDP, main=title)
dev.off()

df_name <- "all_data"
filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/", country, "-" , df_name, "-gdp_pacf.png")
title <- paste0("GDP PACF wrt " , country , " for " , df_name , " data")
filename
# title <- paste0("GDP Autocorrelation for ")
png(filename)
pacf(data$GDP, main=title)
dev.off()

country
adf.test(pre_war$GDP)
country



d_term <- 0
temp_series <- data$GDP
for(x in seq(1, 10)) {
  adf_result <- adf.test(temp_series)
  adf_result$p.value
  if(adf_result$p.value > 0.01) {
    cat("Not Stationary",adf_result$p.value,  "\n" )
    temp_series <- diff(temp_series)
  }else {
    cat("Stationary x = ",x , "p_value=", adf_result$p.value,  "\n" )
    d_term <- x
    break
    
  }
}

ind_vars <- pre_war[ , c( "SPENDING_PERCENT_OF_GDP", "CURRENCY", "INT" ) ]
ind_vars


##############################################
#                               p,d,q
#                           pacf, df test, acf
arima_params <- c(2,3,0)
suffix <- "var1"
params <- deparse(arima_params)
params <- gsub(" ", "", params)
params <- gsub("\\(", "", params)
params <- gsub(")", "", params)
params <- gsub("c", "", params)
params <- gsub(",", "-", params)
params
# ind_vars <- pre_war[ , c("CPI", "INT",  "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]
extraneous_vars <- c( "SPENDING_PERCENT_OF_GDP", "CURRENCY", "INT" )
ind_vars <- pre_war[ , extraneous_vars ]

# MAKE SURE THESE ORDER PARAMS MATCH THOSE ABOVE!
model <- Arima(pre_war$GDP, order=arima_params, xreg = as.matrix(ind_vars))
# corr_matrix <- cor(as.numeric(pre_war))
# fcast <- forecast( model, xreg = as.matrix(ind_vars), h=1000 )
# fcast <- forecast( auto.arima(pre_war), xreg = as.matrix(ind_vars), h=1000 )
ind_vars <- war_data[ , extraneous_vars ]
war_data
# ind_vars <- pre_war[ , c("INT", "UNEMP", "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]
fcast <- forecast( model, xreg = as.matrix(ind_vars), h=10 )
fcast$x
fcast
data$GDP
filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/ARIMA_", country, "_params:", params,"_",suffix,  ".png")
filename
png(filename)

labels
entries <- seq(from=1, to=45, by=4)
entry_str <- labels[entries]
entry_str
abbrv <- paste0("'", substring(entry_str, 3, length(entry_str)))
abbrv

# rm_indices <- seq(2, length(entries), by=2)
# rm_indices
# abbrv[rm_indices] = ""
# abbrv
# length(abbrv)
# length(entries)
# entries
# abbrv[entries]
params_str <- deparse(arima_params)
params_str <- gsub(" ", "", params_str)
params_str <- gsub("c", "", params_str)
extraneous_vars_str <- deparse(extraneous_vars)
extraneous_vars_str
plot_title <- paste0("ARIMAX forecast of GDP ", params_str), " wrt " , 
plot(fcast, xaxt="n", xlab="quarter",ylab="Dollars", main=)
axis(1, at=entries, labels=abbrv)
points(data$GDP)

all_data
dev.off()
# autoplot(fcast, forecast.end =53)


d_term <- 0
temp_series <- data$CURRENCY
for(x in seq(1, 10)) {
  adf_result <- adf.test(temp_series)
  adf_result$p.value
  if(adf_result$p.value > 0.01) {
    cat("Not Stationary",adf_result$p.value,  "\n" )
    temp_series <- diff(temp_series)
  }else {
    cat("Stationary x = ",x , "p_value=", adf_result$p.value,  "\n" )
    d_term <- x
    break
  }
}

# ind_vars <- pre_war[ , c("CPI", "INT",  "SPENDING_PERCENT_OF_GDP", "GDP" ) ]
ind_vars <- pre_war[ , c("GDP",  "SPENDING_PERCENT_OF_GDP", "INT" ) ]

##############################################
# CURRENCY
##############################################





df_name <- "pre_war"
filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/", country, "-" , df_name, "-currency_acf.png")
title <- paste0("GDP ACF wrt " , country , " for " , df_name , " data")
filename
# title <- paste0("GDP Autocorrelation for ")
png(filename)
acf(pre_war$GDP, main=title)
dev.off()

df_name <- "pre_war"
filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/", country, "-" , df_name, "-currency_pacf.png")
title <- paste0("GDP PACF wrt " , country , " for " , df_name , " data")
filename
# title <- paste0("GDP Autocorrelation for ")
png(filename)
pacf(data$GDP, main=title)
dev.off()

df_name <- "all_data"
filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/", country, "-" , df_name, "-currency_acf.png")
title <- paste0("GDP ACF wrt " , country , " for " , df_name , " data")
filename
# title <- paste0("GDP Autocorrelation for ")
png(filename)
acf(data$GDP, main=title)
dev.off()

df_name <- "all_data"
filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/", country, "-" , df_name, "-currency_pacf.png")
title <- paste0("GDP PACF wrt " , country , " for " , df_name , " data")
filename
# title <- paste0("GDP Autocorrelation for ")
png(filename)
pacf(data$GDP, main=title)
dev.off()

##############################################
#                               p,d,q
#                           pacf, df test, acf
suffix <- "var1"
params <- deparse(substitute(c(9,4,0)))
params <- gsub(" ", "", params)
params <- gsub("\\(", "", params)
params <- gsub(")", "", params)
params <- gsub("c", "", params)
params <- gsub(",", "-", params)
params
ind_vars <- pre_war[ , c("GDP",  "SPENDING_PERCENT_OF_GDP", "INT" ) ]
# MAKE SURE THESE ORDER PARAMS MATCH THOSE ABOVE!
model <- Arima(pre_war$CURRENCY, order=c(9,4,0), xreg = as.matrix(ind_vars))
# corr_matrix <- cor(as.numeric(pre_war))
# fcast <- forecast( model, xreg = as.matrix(ind_vars), h=1000 )
# fcast <- forecast( auto.arima(pre_war), xreg = as.matrix(ind_vars), h=1000 )
# ind_vars <- pre_war[ , c("INT", "UNEMP", "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]
# ind_vars <- war_data[ , c("CPI",  "INT", "GDP" ) ]
ind_vars <- war_data[ , c("GDP",  "SPENDING_PERCENT_OF_GDP", "INT" ) ]

fcast <- forecast( model, xreg = as.matrix(ind_vars), h=4 )
fcast$x
fcast
filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/ARIMA_", country, "_params:", params,"_",suffix,  "_currency.png")
filename
png(filename)
plot(fcast, xlim=c(0, nrow(data)), ylim=c(min(data$CURRENCY), max(data$CURRENCY)))
all_data
points(data$CURRENCY)
all_data
dev.off()
data
# autoplot(fcast, forecast.end =53)

