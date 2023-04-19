library(ggfortify)
install.packages("gptstudio")
library(forecast)
Sys.setenv(OPENAI_API_KEY = "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")
require(usethis)
library(ggplot2)
edit_r_environ(scope = "project")

# commod_names <- c("CrudeOil", "NatGas",  "Wheat",  "SoybeanOil" )
# commod_names <- c("CrudeOil")
commod_names <- c("CrudeOil", "NatGas",  "Wheat",  "SoybeanOil" )

commod_name <- "Wheat"



analyze_commod <- function(commod_name) {
  
  price_selection <- "Close"
  filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/commodities/",commod_name,".csv")
  cat("Filename is " , filename , "\n")
  data <- read.csv(filename)
  print(data)
  print(data$Close)
  data
  war_start_index <- which(data$Date == "2022-03-01")
  war_start_index
  # 
  pre_war <- data[0:(war_start_index-1),]
  # pre_war
  pre_war[] <- lapply(pre_war, as.numeric)
  sapply(data, class)
  # 
  war_data <- data[(war_start_index):nrow(data), ]
  war_data
  # arima_model <- arima(pre_war$Close, order=c(9,5,8))
  arima_model <- arima(pre_war[price_selection], order=c(1,0,2))
  fcast <- forecast(arima_model, h=9)
  titleStr <- paste("\n\n wrt ", commod_name)
  
  # Save ACF plot to file
  png("/home/david/Documents/Semester10/MTH5324/Project/commodities/thing1.png")
  acf_data <- acf(data, main="Autocorrelation of data with 95% lines in blue")
  dev.off()
  
  # Save forecast plot to file
  png("/home/david/Documents/Semester10/MTH5324/Project/commodities/thing2.png")
  plot(fcast)
  title(titleStr)
  dev.off()
  
  # # Plot data points and forecast
  # plot(data$Date, data$Close, type="l", xlab="Date", ylab="Price", main=paste0("Price of ", commod_name))
  # lines(fcast$mean, col="red")
  
}

plot_residuals <- function() {
}

  commod_name = "NatGas"
  filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/commodities/",commod_name,".csv")
  cat("Filename is " , filename , "\n")
  all_data <- read.csv(filename)
  print(all_data)
  print(all_data$Close)
  all_data
  war_start_index <- which(all_data$Date == "2022-03-01")
  war_start_index
  # 
  pre_war <- all_data[0:(war_start_index-1),]
  # pre_war
  pre_war[] <- lapply(pre_war, as.numeric)
  sapply(all_data, class)
  # 
  war_data <- all_data[(war_start_index):nrow(all_data), ]
    war_data
    
  arima_model <- arima(pre_war$Close, order=c(1,0,2))
  res <- residuals(arima_model)
#   png("/home/david/Documents/Semester10/MTH5324/Project/commodities/res_plot.png")
#   plot(res)
#   acf(res)
#   dev.off()
  
  fcast <- forecast(arima_model, h=9)
  titleStr <- paste("\n\n wrt ", commod_name)
  commod_name
  filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/commodities/forecast_", commod_name, ".png")
  filename
  png(filename)
  plot(fcast, title=title(titleStr))
  # points(all_data[war_start_index :nrow(all_data) , ])
  points(all_data$Close)
  all_data$Close
  dev.off()
  
  df_title_str <- deparse(substitute(all_data))
  file_name <- paste0("/home/david/Documents/Semester10/MTH5324/Project/commodities/",df_title_str, "_",commod_name, "_acf.png")
  plot_title <- paste0("Autocorrelation of " ,commod_name, " for ",  df_title_str,  "  with 95% lines in blue")
  png(file_name)
  acf(all_data$Close, type="correlation", main=plot_title)
  dev.off()
  
  df <- pre_war
  df_title_str <- deparse(substitute(pre_war))
  file_name <- paste0("/home/david/Documents/Semester10/MTH5324/Project/commodities/",df_title_str, "_",commod_name, "_acf.png")
  plot_title <- paste0("Autocorrelation of " ,commod_name, " for ",  df_title_str,  "  with 95% lines in blue")
  png(file_name)
  acf(pre_war$Close, type="correlation", main=plot_title)
  dev.off()
  
  
 #  df <- post_war
 #  df_title_str <- deparse(substitute(df))
 #  file_name <- paste0("/home/david/Documents/Semester10/MTH5324/Project/commodities/",df_title_str, "_",commod_name, ".png")
 #  plot_title <- paste0("Autocorrelation of " ,commod_name, " for ",  df_title_str,  "  with 95% lines in blue")
 #  png(file_name)
 #  acf(df$Close, type="correlation", main=plot_title)
 #  dev.off()
  
  