library(ggfortify)
library(forecast)
library(ggplot2)
library(tseries)

countries <- c("USA", "EURO", "JAP" , "CHINA")
col_subs <- c("SPENDING_PERCENT_OF_GDP"="spending % of GDP", "CURRENCY"=deparse(coin_symbol), "horse"="neigh", "INT"="")
country_units <- c("JAP"="Billions of chained 2015 yen", "EURO"="chained 2010 EUROS", "USA"="billions of US$", "CHINA"="Current US$")
country <- "CHINA"
country
coin_symbols <- list("JAP"="¥", "EURO"="€", "USA"="$", "CHINA"="CN¥")

csv_name <- paste0("/home/david/Documents/Semester10/MTH5324/Project/" , country  , "-bounded-2012.csv")
data <- read.csv(csv_name)

data

if (country == "CHINA") {
  start_index <- which(data$quarter == "2018Q1") 
  data <- data[start_index:nrow(data), 0:ncol(data)]
  data
  labelsD = data$quarter
}
labelsD
labels

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
rnorm(nrow(data), mean=0, sd=10e4)
data$GDP
data$GDP + rnorm(nrow(data), mean=0, sd=10e5)
data$GDP <- data$GDP + rnorm(nrow(data), mean=0, sd=10e4)
data$GDP
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

run_acfs_gdp <- function() {
  
  
df_name <- "pre_war"
filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/", country, "-" , df_name, "-gdp_acf.png")
title <- paste0("GDP ACF wrt " , country , " for " , df_name , " data")
filename
# title <- paste0("GDP Autocorrelation for ")
png(filename)

acf_dat <- acf(pre_war$GDP, main=title)
plot(acf_dat, main=title)
dev.off()
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
  
}

run_acfs_gdp()

metric <- "GDP"

find_d_term <- function(metric) {
  d_term <- 0
  print(data[metric])
  data[metric]
  temp_series <- data[, metric]
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

d_term <- find_d_term(metric)
d_term
substitute(d_term)

extraneous_vars <- c( "SPENDING_PERCENT_OF_GDP", "CURRENCY", "INT" )
ind_vars <- pre_war[ , extraneous_vars ]


  ##############################################
  #                               p,d,q
  #                           pacf, df test, acf
  arima_params <- c(1,4,3)
  metric <- "GDP"
  
  suffix <- "var1"
  params <- deparse(arima_params)
  params <- gsub(" ", "", params)
  params <- gsub("\\(", "", params)
  params <- gsub(")", "", params)
  params <- gsub("c", "", params)
  params <- gsub(",", "-", params)
  params
  # ind_vars <- pre_war[ , c("CPI", "INT",  "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]
  ind_vars <- pre_war[ , extraneous_vars ]
  
  # MAKE SURE THESE ORDER PARAMS MATCH THOSE ABOVE!
  model <- Arima(pre_war[, metric], order=arima_params, xreg = as.matrix(ind_vars))
  # corr_matrix <- cor(as.numeric(pre_war))
  # fcast <- forecast( model, xreg = as.matrix(ind_vars), h=1000 )
  # fcast <- forecast( auto.arima(pre_war), xreg = as.matrix(ind_vars), h=1000 )
  ind_vars <- war_data[ , extraneous_vars ]
  war_data
  # ind_vars <- pre_war[ , c("INT", "UNEMP", "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]
  metric <- "GDP"
  fcast <- forecast( model, xreg = as.matrix(ind_vars), h=10 )
  fcast$x
  fcast
  data$GDP
  filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/ARIMA_", country, "_params:", params,"_",suffix,  ".png")
  filename
  png(filename)
  
  
  labels <- data$quarter
  labels
  nrow(data)
  data
  entries <- seq(from=1, to=nrow(data), by=4)
  entries
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
  coin_symbol <- coin_symbols[country]
  coin_symbol
  col_subs
  col_subs["CURRENCY"]
  extraneous_vars_str <- gsub("SPENDING_PERCENT_OF_GDP", col_subs["SPENDING_PERCENT_OF_GDP"], extraneous_vars_str)
  extraneous_vars_str
  extraneous_vars_str <- gsub("CURRENCY", col_subs["CURRENCY"], extraneous_vars_str)
  extraneous_vars_str
  extraneous_vars_str <- gsub("\"", "", extraneous_vars_str)
  extraneous_vars_str <- gsub("c", "", extraneous_vars_str)
  extraneous_vars_str <- gsub("\\(", "", extraneous_vars_str)
  extraneous_vars_str <- gsub(")", "", extraneous_vars_str)
  extraneous_vars_str <- gsub("list", "", extraneous_vars_str)
  extraneous_vars_str
  extraneous_vars_str <- gsub(country, "currency: ", extraneous_vars_str)
  extraneous_vars_str <- gsub("  = ", "", extraneous_vars_str)
  extraneous_vars_str 
  
  country_name <- c("JAP"="Japan", "EURO"="Europe", "USA"="USA" , "CHINA", "China")
  plot_title <- paste0("ARIMAX " , params_str  ," forecast of  " , country_name[country] , " " , metric , " " ,  "\n wrt\n " , extraneous_vars_str)
  ylab_str <- paste0(country_name[country],  " " , metric , ": ", "(", country_units[country], ")")
  
  ylab_str
  max(data[, metric])
  lines(data[,"GDP"])
  dev.off()
  plot(fcast, xaxt="n", xlab="quarter",ylab=ylab_str, main=plot_title, ylim= c(min(data[, metric]), max(data[, metric])))   
  axis(1, at=entries, labels=abbrv)
  my_shape1 <- c(0.2, 0.5, 0.8, 0.5, 0.2, -0.2, -0.5, -0.2)
  my_shape2 <- c(1, 0.5, 0.8, 0.5, 0.2, -0.2, -0.5, -0.2)
  points(data[, metric], col=ifelse(data$quarter>="2022Q1", "red", "black"), pch=ifelse(data$quarter>="2022Q1",24 , 1))
  
  # CHINA, US$
  # EURO, chained 2010 EUROS
  # USA, billions of US$
  # JAPAN, billions of chained 2015 yen
  
  all_data
  dev.off()
  
    


metric <- "CURRENCY"


  d_term <- find_d_term(metric)
d_term

# ind_vars <- pre_war[ , c("CPI", "INT",  "SPENDING_PERCENT_OF_GDP", "GDP" ) ]
ind_vars <- pre_war[ , c("GDP",  "SPENDING_PERCENT_OF_GDP", "INT" ) ]

##############################################
# CURRENCY
##############################################





#   ##############################################
#   #                               p,d,q
#   #                           pacf, df test, acf
#   arima_params <- c(2,4,0)
#   metric <- "CURRENCY"
#   
#   suffix <- "var1"
#   params <- deparse(arima_params)
#   params <- gsub(" ", "", params)
#   params <- gsub("\\(", "", params)
#   params <- gsub(")", "", params)
#   params <- gsub("c", "", params)
#   params <- gsub(",", "-", params)
#   params
#   # ind_vars <- pre_war[ , c("CPI", "INT",  "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]
#   ind_vars <- pre_war[ , extraneous_vars ]
#   
#   # MAKE SURE THESE ORDER PARAMS MATCH THOSE ABOVE!
#   model <- Arima(pre_war[, metric], order=arima_params, xreg = as.matrix(ind_vars))
#   # corr_matrix <- cor(as.numeric(pre_war))
#   # fcast <- forecast( model, xreg = as.matrix(ind_vars), h=1000 )
#   # fcast <- forecast( auto.arima(pre_war), xreg = as.matrix(ind_vars), h=1000 )
#   ind_vars <- war_data[ , extraneous_vars ]
#   war_data
#   # ind_vars <- pre_war[ , c("INT", "UNEMP", "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]
#   fcast <- forecast( model, xreg = as.matrix(ind_vars), h=10 )
#   fcast$x
#   fcast
#   data$GDP
#   suffix <- metric
#   filename <- paste0("/home/david/Documents/Semester10/MTH5324/Project/acfs_gdp/ARIMA_", country, "_params:", params,"_",suffix,  ".png")
#   filename
#   png(filename)
#   
#   labels
#   entries <- seq(from=1, to=45, by=4)
#   entry_str <- labels[entries]
#   entry_str
#   abbrv <- paste0("'", substring(entry_str, 3, length(entry_str)))
#   abbrv
#   
#   # rm_indices <- seq(2, length(entries), by=2)
#   # rm_indices
#   # abbrv[rm_indices] = ""
#   # abbrv
#   # length(abbrv)
#   # length(entries)
#   # entries
#   # abbrv[entries]
#   params_str <- deparse(arima_params)
#   params_str <- gsub(" ", "", params_str)
#   params_str <- gsub("c", "", params_str)
#   extraneous_vars_str <- deparse(extraneous_vars)
#   extraneous_vars_str
#   coin_symbol <- coin_symbols[country]
#   coin_symbol
#   col_subs
#   col_subs["CURRENCY"]
#   extraneous_vars_str <- gsub("SPENDING_PERCENT_OF_GDP", col_subs["SPENDING_PERCENT_OF_GDP"], extraneous_vars_str)
#   extraneous_vars_str
#   extraneous_vars_str <- gsub("CURRENCY", col_subs["CURRENCY"], extraneous_vars_str)
#   extraneous_vars_str
#   extraneous_vars_str <- gsub("\"", "", extraneous_vars_str)
#   extraneous_vars_str <- gsub("c", "", extraneous_vars_str)
#   extraneous_vars_str <- gsub("\\(", "", extraneous_vars_str)
#   extraneous_vars_str <- gsub(")", "", extraneous_vars_str)
#   extraneous_vars_str <- gsub("list", "", extraneous_vars_str)
#   extraneous_vars_str
#   extraneous_vars_str <- gsub(country, "currency: ", extraneous_vars_str)
#   extraneous_vars_str <- gsub("  = ", "", extraneous_vars_str)
#   extraneous_vars_str 
#   
#   country_name <- c("JAP"="Japan", "EURO"="Europe", "USA"="USA" , "CHINA", "China")
#   plot_title <- paste0("ARIMAX " , params_str  ," forecast of  " , country_name[country] , " " , metric , " " ,  "\n wrt\n " , extraneous_vars_str)
#   ylab_str <- paste0(country_name[country],  " " , metric , ": ", "(", country_units[country], ")")
#   
#   ylab_str
#   plot(fcast, xaxt="n", xlab="quarter",ylab=ylab_str, main=plot_title)
#   axis(1, at=entries, labels=abbrv)
#   my_shape1 <- c(0.2, 0.5, 0.8, 0.5, 0.2, -0.2, -0.5, -0.2)
#   my_shape2 <- c(1, 0.5, 0.8, 0.5, 0.2, -0.2, -0.5, -0.2)
#   points(data[metric], col=ifelse(data$quarter>="2022Q1", "red", "black"), pch=ifelse(data$quarter>="2022Q1",24 , 1))
#   
#   # CHINA, US$
#   # EURO, chained 2010 EUROS
#   # USA, billions of US$
#   # JAPAN, billions of chained 2015 yen
#   
#   all_data
#   dev.off()
#   
#   