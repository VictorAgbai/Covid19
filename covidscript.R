rm(list = ls()) #removes all stored data in environment

Covid_data <- read.csv("C:/Users/user/Downloads/COVID19_line_list_data.csv")

install.packages("Hmisc") #useful function for analysis
library(Hmisc) #import function

describe (Covid_data)

#Data cleaning
# In the death column, dates are included which is inconsistent with the values given
# ie 1 - for death, 0 - for no death

Covid_data$cleaned_death <- as.integer(Covid_data$death != 0)

unique(Covid_data$cleaned_death) #verify cleaned column

#Statistical Analysis
#death rate
sum(Covid_data$cleaned_death) / nrow(Covid_data)

#Death by gender
#hypothesis - men are more affected by women
men = subset(Covid_data, gender == "male")
women = subset(Covid_data, gender == "female")
mean(men$cleaned_death, na.rm = TRUE) #8.5%
mean(women$cleaned_death, na.rm = TRUE) #3.7%

#Statistical significance with t-test
t.test(men$cleaned_death, women$cleaned_death, alternative = "two.sided", conf.level = 0.99)
#men have from 0.8% to 8.8% chance of death @ 0.99 confidence level
#p-value: 0.002 which is less than 0.05 (Therefore, it is statistically significant)

#And sample data is representative of the population

#Death by age
#hypothesis - older people die more
dead = subset(Covid_data, cleaned_death == 1)
alive = subset(Covid_data, cleaned_death == 0)
mean(dead$age, na.rm = TRUE) 
mean(alive$age, na.rm = TRUE)
#About 20 years difference (68 and 48)

#Statistical significance with t-test
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99 )
# at a confidence level of 99%, people alive are between ages 15 and 25
# p_value is way less than 0.05 (Therefore, our hypothesis is statistically significant)

#Older people die more than younger people
