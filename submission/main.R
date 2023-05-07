# Sam Sharp & David E. Nieves
library(olsrr)
library(arsenal)
library(GGally)
data = read.csv("./Income.csv")
data[is.na(data$native.country) ]
data$column

# REMINDER: put into variables when using ols, or you will get
# invalid variable type error!
age = data$age
salary = data$salary
education = data$education
education.num = data$education.num
marital.status = data$marital
occupation = data$occupation
race = data$race
gender = data$gender
hours.per.week = data$hours.per.week
workclass = data$workclass
model <- lm(salary ~ age
            + race 
            + gender
            + workclass
            + education
            + education.num
            + hours.per.week)

summary(model)

# summary(comparedf(not_nulls, data))
# comparedf(not_nulls, data)

# ggpairs(data, cardinality_threshold = 50)

unique(data$workclass)
counts_df <- data.frame(race = character(),
                        gender = character(),
                        workclass = character(),
                        count = numeric(),
                        stringsAsFactors = FALSE)

length(unique(data$gender))
       length(unique(data$race))
              length(unique(data$workclass))

i = 0
counts_list <- list()
for(genderU in unique(data$gender)) {
  race_cnt = 0
  for(raceU in unique(data$race)) {
    for(classU in unique(data$workclass)) {
      cat("i=" , i , "\n")
      # get all df values that match
      # gender, race, class == gender, race, class
      # count...
      # now, data[gender, race, class] == count

      countU <- nrow(data[which(data$gender == genderU & data$race == raceU & data$workclass == classU),] )
      if(i == 0) {
        cat("CountU = ", countU)
      }      
      #countU <- sum(data$income == 1 & data$race == raceU & data$gender == genderU & data$workclass == classU)
      # count_df[nrow(count_df) + 1, ] <- c(count, gender, race, workclass)
      #label <- paste(race, gender, workclass, sep = " - ")
      counts_df <- rbind(counts_df, data.frame(race = raceU,
                                               gender = genderU,
                                               workclass = classU,
                                               count = countU,
                                               stringsAsFactor = FALSE))
    }
  }
  i = i + 1
}
counts_df

crace = counts_df$race
cgender = counts_df$gender
cclass = counts_df$workclass
ccount = counts_df$count


counts_model <- lm(ccount ~ crace
            + cgender
            + cclass)

summary(counts_model)

ols_step_best_subset(counts_model)


