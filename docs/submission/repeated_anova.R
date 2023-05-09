# Load the required package
install.packages("tidyverse")
library(tidyverse)
library(ggpubr)
library(rstatix)
library(stats)

data <- read.csv("/home/david/Documents/Semester10/MTH5324/Project/USA-bounded-2012.csv")

# data$Date = diff(data$Date)
# data$GDP = diff(data$Date)
# data$GDP = diff(data$GDP)
# data$GDP
# data$
#   data$GDP = c(0, diff(data$GDP))

data$quarter
war_start_index <- which(data$quarter == "2022Q1")
war_start_index


pre_war <- data[0:(war_start_index-1),]
pre_war
pre_war[] <- lapply(pre_war, as.numeric)
sapply(data, class)

post_war <- data[(war_start_index):nrow(data), ]
post_war

quarter_indx <- grep("quarter", colnames(data))
pre_war <- pre_war[,-quarter_indx]

ind_vars <- pre_war[ , c("CPI", "INT", "UNEMP", "SPENDING_PERCENT_OF_GDP", "CURRENCY" ) ]

data("selfesteem", package = "datarium")
head(selfesteem, 3)

# 1. Compute group differences
grp.diff <- selfesteem %>%
  transmute(
    `t1-t2` = t1 - t2,
    `t1-t3` = t1 - t3,
    `t2-t3` = t2 - t3
  )
head(grp.diff, 3)

# 2. Compute the variances
grp.diff  %>% map(var)

selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
head(selfesteem, 3)

res <- anova_test(data = selfesteem, dv = score, wid = id, within = time)
res

