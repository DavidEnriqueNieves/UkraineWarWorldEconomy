library(ggfortify)
install.packages("gptstudio")
library(forecast)
require(usethis)
library(tseries)

# commod_names <- c("CrudeOil", "NatGas",  "Wheat",  "SoybeanOil" )
# commod_names <- c("CrudeOil")
commod_names <- c("CrudeOil", "NatGas",  "Wheat",  "SoybeanOil" )

commod_name <- "NatGas"

# analyze_commod <- function(commod_name) {
#   
#   price_selection <- "Close"
#   filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/commodities/",commod_name,".csv")
#   cat("Filename is " , filename , "\n")
#   data <- read.csv(filename)
#   print(data)
#   print(data$Close)
#   data
#   war_start_index <- which(data$Date == "2022-03-01")
#   war_start_index
#   # 
#   pre_war <- data[0:(war_start_index-1),]
#   # pre_war
#   pre_war[] <- lapply(pre_war, as.numeric)
#   sapply(data, class)
#   # 
#   war_data <- data[(war_start_index):nrow(data), ]
#   war_data
#   # arima_model <- arima(pre_war$Close, order=c(9,5,8))
#   arima_model <- arima(pre_war[price_selection], order=c(1,0,2))
#   fcast <- forecast(arima_model, h=9)
#   titleStr <- paste("\n\n wrt ", commod_name)
#   plot(fcast, title=title(titleStr))
#   # points(data[war_start_index :nrow(data) , ])
#   points(data)
#   
#   plot_title <- paste0("Autocorrelation of " ,commod_name, " with 95% lines in blue")
#   # acf(data, main=plot_title)
#   png("./plot2.png")
#   acf(data, main=plot_title)
#   dev.off()
# 
# }
# 
# analyze_commod("Wheat")


  filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/commod-daily/",commod_name,".csv")
    cat("Filename is " , filename , "\n")
  all_data <- read.csv(filename)
  # all_data <- which(all_data$Date >= "2011-01-01")
  all_data <- all_data[which(all_data$Date >= "2011-01-01"), ]
  print(all_data)
  all_data$Date
  
  war_start_index <- which(all_data$Date >= "2022-24-02")
  war_data <- subset(all_data, all_data$Date >= as.Date("2022-02-24"))
  pre_war <- subset(all_data, all_data$Date < as.Date("2022-02-24") )
  pre_war
  pre_war <- subset(pre_war, pre_war$Date > as.Date("2020-11-01") )
  all_data <- all_data[which(all_data$Date > "2020-11-01"), ]
  pre_war
  
  war_start_index
  war_start_index <- war_start_index[1]
  war_start_index
  all_data[war_start_index,]
  
find_d_term <- function(metric) {
  d_term <- 0
  print(all_data[metric])
  all_data[metric]
  temp_series <- all_data[, metric]
  for(x in seq(1, 10)) {
    adf_result <- adf.test(temp_series)
    adf_result$p.value
    if(adf_result$p.value > 0.01) {
      cat("Not Stationary",adf_result$p.value,  "\n" )
      temp_series <- diff(temp_series)
    }else {
      cat("Stationary x = ",x , "p_value=", adf_result$p.value,  "\n" )
      d_term <- x
      return(d_term)
      break
    }
  }
}

temp_series
d_term <- find_d_term("Close")
d_term
diff_close <- diff(pre_war$Close, differences=d_term)
diff_close
  
  # model <- Arima(diff_close, order=arima_params)
  fcast <- forecast( model, h=nrow(war_data) )
  
 all_diff_close <- diff(all_data$Close, differences=d_term)
  plot(fcast)
  points(all_diff_close)
  # install.packages("astsa")
  # library(astsa)
#   model <- sarima(pre_war[, metric], p=0,d=2,q=40)
#   plot(sarima.for(pre_war[, metric], p=0,d=2,q=40))
  # library(forecast)
  
  # Convert pre_war data to a time series object
  ts_data <- ts(pre_war$Close, frequency = 1)
  
  # Fit a SARIMA model to the time series
  # sarima_model <- auto.arima(ts_data, seasonal = TRUE)
  
  # Make a forecast of the next 10 periods
  forecast_data <- forecast(sarima_model, h = 365)
  d_term
  
  # Plot the forecast with the pre-war data
  jj
  jj
  plot(forecast_data)
  plot(ts_data, main = "Pre-war Close data with SARIMA forecast")
  points(all_data$Close)
  lines(forecast_data$mean, col = "red")
  # corr_matrix <- cor(as.numeric(pre_war))
  # fcast <- forecast( model, xreg = as.matrix(ind_vars), h=1000 )
  # fcast <- forecast( auto.arima(pre_war), xreg = as.matrix(ind_vars), h=1000 )
  # ind_vars <- all_data[ , extraneous_vars ]
  
  library(lubridate)
  fcast <- forecast( model, h=nrow(war_data) )
  
   plot_title <- paste0("Autocorrelation of " ,commod_name, " \n with 95% lines in blue")
  
  df_title_str <- deparse(substitute(post_war))
  df_title_str <- deparse(substitute(all_data))
  plot_title <- paste0("Autocorrelation of " ,commod_name, " for ",  df_title_str,  "\n  with 95% lines in blue")
  
  filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/arima/",commod_name,"-all_data-acf.png")
  png(filename)
  acf(all_data$Close, type="correlation", main=plot_title)
  dev.off()
  
  filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/arima/",commod_name,"-prewar-acf.png")
  png(filename)
  acf(pre_war$Close, type="correlation", main=plot_title)
  dev.off()
  
  filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/arima/",commod_name,"-all_data-pacf.png")
  png(filename)
  pacf(all_data$Close, type="correlation", main=plot_title)
  dev.off()
  
  filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/arima/",commod_name,"-prewar-pacf.png")
  png(filename)
  pacf(pre_war$Close, type="correlation", main=plot_title)
  dev.off()
  d_term
  ##############################
  #      Plotting ARIMA
  ##############################
  ##############################################
  #                               p,d,q
  #                           pacf, df test, acf
  arima_params <- c(0,2,40)
  
  params <- deparse(arima_params)
  params <- gsub(" ", "", params)
  # params <- gsub("\\(", "", params)
  # params <- gsub(")", "", params)
  params <- gsub("c", "", params)
  #params <- gsub(",", "-", params)
  params
  
  # ind_vars <- pre_war[ , c("CPI", "INT",  "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]
  
  metric <- "Close"
  # MAKE SURE THESE ORDER PARAMS MATCH THOSE ABOVE!
  pre_war
  # model <- Arima(pre_war[, metric], order=arima_params)
  # corr_matrix <- cor(as.numeric(pre_war))
  # fcast <- forecast( model, xreg = as.matrix(ind_vars), h=1000 )
  # fcast <- forecast( auto.arima(pre_war), xreg = as.matrix(ind_vars), h=1000 )
  # ind_vars <- all_data[ , extraneous_vars ]
  
  library(lubridate)
  fcast <- forecast( model, h=nrow(war_data) )
  fcast
  seq(0,200, 10)
  fcast[seq(0,200, 10)]
  filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/ARIMA_", commod_name, "_params:", params,"_",metric,  ".png")
  png(filename)
  all_data$Date
  labels <- all_data$Date
  labels
  library("zoo")
  quarters <- floor_date(as.Date(labels), "quarter")
  qtrs <- as.yearqtr(as.Date(labels),           # Convert dates to quarterly
             format = "%Y-%m-%d")
  
  labels_str <- qtrs
  
  
  params_str <- deparse(arima_params)
  params_str <- gsub("c", "", params_str)
  params_str <- gsub(" ", "", params_str)
  params_str
  plot_title <- paste0("ARIMA " , params_str  ," forecast of " , " " , commod_name , " futures in USD($) " )
  plot_title
  ylab_str <- paste0(commod_name,  " " , metric , " price : ", "(USD)")
  ylab_str
  
  entries <- seq(from=1, to=nrow(all_data), by=4)
  labels_str <- labels_str[entries]
  start_plot_index <- which(all_data$Date >= "2021-01-01")
  start_plot_index <- start_plot_index[1]
  typeof(fcast)
  fcast$mean
  fcast_idx <- which(fcast$x%%10==0)
  fcast_idx
  fcast
  length(fcast)
  seq(1,length(fcast), 10)
  plot(fcast)
   plot(fcast, xaxt="n", 
        xlab="Days (starting from 2021-01-01, war starts on 2022-24-02",
        ylab=ylab_str,
        main=plot_title,
        xlim=c(1, nrow(pre_war) + nrow(war_data) ),
        # ylim=c(min(pre_war[start_plot_index,"Close"]), )
        )
    axis(1, at=entries, labels=labels_str)
   library(forecast)
   labels_str
  my_shape1 <- c(0.2, 0.5, 0.8, 0.5, 0.2, -0.2, -0.5, -0.2)
  my_shape2 <- c(1, 0.5, 0.8, 0.5, 0.2, -0.2, -0.5, -0.2)
  all_data$X <- seq(1, nrow(all_data))
  
  subset_data <- subset(all_data, all_data$Date >= "2022-02-24")
  points(all_data$Close, col=ifelse(all_data$X%%5==0,ifelse(all_data$Date>= "2022-02-24" , "red", "black"), "#FFFFFF00" ), pch=ifelse(all_data$Date>= "2022-02-24",24 , 1) )
  lines(subset_data$X, subset_data$Close, type = "l", col = "red")
  
  dev.off()
  residuals <- residuals(model)
  
  # TODO: ARIMA 
  params_str
  qqnorm(residuals , main=paste0("Normality of residuals ", " of ARIMA", params_str , " \n wrt ",   commod_name) )
  qqline(residuals)
  
  ks.test(residuals, "pnorm")
  shapiro.test(residuals)
  
  
  
  
  
  
  arima_model <- arima(pre_war$Close, order=c(1,0,2))
  
  
  