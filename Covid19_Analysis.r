rm(list=ls())  #removes all variables stored previously
library(Hmisc) #import

project <- read.csv("C:/Users/bhanu/OneDrive/Desktop/Covid 19 - Project/COVID19_line_list_data.csv")
project  #displays data in console
View(project) #displays data in a tabular format in separate window
describe(project)  #Hmisc command

#cleaned up death column
project$death_dummy <- as.integer(project$death != 0)
unique(project$death_dummy)

#death rate
sum(project$death_dummy) / nrow(project)

#Age
#claim: people who die are older than people who survived
dead = subset(project, death_dummy == 1)
alive = subset(project, death_dummy == 0)
mean(dead$age, na.rm = TRUE)  #displays the mean age of dead persons by removing the NA columns
mean(alive$age, na.rm = TRUE) #displays the mean age of alive persons by removing the NA columns

#Calculating Age difference
#is this statistically significant ?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.95)
#normally, if p-value < 0.05, we reject null hypothesis
#As, P-value ~ 0, we reject null hypothesis and conclude that this is statistically significant

#Gender
#Claim: Gender has no effect
men = subset(project, gender == "male")
women = subset(project, gender == "female")
mean(men$death_dummy)   #Mean =~ 8.5%
mean(women$death_dummy)  #Mean =~ 3.7%

#performing t-test
t.test(men$death_dummy, women$death_dummy, alternative= "two.sided", conf.level = 0.95)
# 95% confidence: men have from 0.8% to 8.8% higher chance of dying
# p-value = 0.002 < 0.05, so this is statistically significant
#Therefore, men have higher death rate than women and the sample is representation of that population