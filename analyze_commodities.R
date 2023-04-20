library(ggfortify)
install.packages("gptstudio")
library(forecast)
Sys.setenv(OPENAI_API_KEY = "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")
require(usethis)
edit_r_environ(scope = "project")
library(tseries)

# commod_names <- c("CrudeOil", "NatGas",  "Wheat",  "SoybeanOil" )
# commod_names <- c("CrudeOil")
commod_names <- c("CrudeOil", "NatGas",  "Wheat",  "SoybeanOil" )

commod_name <- "Wheat"

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

  commod_name = "SoybeanOil"

  filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/commodities/",commod_name,".csv")
  cat("Filename is " , filename , "\n")
  all_data <- read.csv(filename)
  print(all_data)
  print(all_data$Close)
  all_data
  war_start_index <- which(all_data$Date == "2022-01-01")
  war_start_index
  # 
  pre_war <- all_data[0:(war_start_index-1),]
  # pre_war
  pre_war[] <- lapply(pre_war, as.numeric)
  sapply(all_data, class)
  # 
  war_data <- all_data[(war_start_index):nrow(all_data), ]
    war_data
  # arima_model <- arima(pre_war$Close, order=c(9,5,8))
  arima_model <- arima(pre_war$Close, order=c(1,0,2))
  res <- residuals(arima_model)
  plot(res)
  acf(res)
  fcast <- forecast(arima_model, h=9)
  titleStr <- paste("\n\n wrt ", commod_name)
  plot(fcast, title=title(titleStr))
  # points(all_data[war_start_index :nrow(all_data) , ])
  points(all_data)
  
  plot_title <- paste0("Autocorrelation of " ,commod_name, " with 95% lines in blue")
  
  df_title_str <- deparse(substitute(post_war))
  df_title_str <- deparse(substitute(all_data))
  plot_title <- paste0("Autocorrelation of " ,commod_name, " for ",  df_title_str,  "  with 95% lines in blue")
  acf(all_data$Close, type="correlation", main=plot_title)
  acf(pre_war$Close, type="correlation", main=plot_title)
  
  adf.test(all_data$Close, "stationary")
  adf.test(pre_war$Close, "stationary")
  
  