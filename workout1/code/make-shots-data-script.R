# Make-shots-data-script 
# MAKE SHOTS SCRIPT
# Input: This script imports the csv files of individual player data 
# output: This makes changes by adding a minute column to the csv files
#         and returns a much larger csv file containing information on all the players

library(readr)
library(dplyr)
library(stringr)


curry <- read.csv('Desktop/workout1/data/stephen-curry.csv', stringsAsFactors = FALSE)
curry$name = 'Stephen Curry'
curry$minute = (curry$period *12)- curry$minutes_remaining

sink('Desktop/workout1/output/Stephen-Curry-summary.txt')
summary(curry)
sink()


andre <- read.csv('../data/andre-iguodala.csv', stringsAsFactors = FALSE)
andre$name = 'Andre Iguodala'
andre$minute = (andre$period *12)- andre$minutes_remaining
sink('Desktop/workout1/output/Andre-Iguodala-summary.txt')
summary(andre)
sink()

dray <- read.csv('../data/draymond-green.csv', stringsAsFactors = FALSE, miss)
dray$name = 'Draymond Green'
dray$minute = (dray$period *12)- dray$minutes_remaining
sink('Desktop/workout1/output/Draymond-Green-summary.txt')
summary(dray)
sink()

kevin <- read.csv('../data/kevin-durant.csv', stringsAsFactors = FALSE)
kevin$name ='Kevin Durant'
kevin$minute = (kevin$period *12)- kevin$minutes_remaining
sink('Desktop/workout1/output/Kevin-Durant-summary.txt')
summary(kevin)
sink()

klay <- read.csv('../data/klay-thompson.csv', stringsAsFactors = FALSE)
klay$name = 'Klay Thompson'
klay$minute = (klay$period *12)-klay$minutes_remaining
sink('Desktop/workout1/output/Klay-Thompson-summary.txt')
summary(klay)
sink()

temp1 = rbind(curry,andre)

temp2 = rbind(temp1, dray)

temp3 = rbind(temp2, kevin)

temp4= rbind(temp3, klay)

write.csv(temp4, 'Desktop/workout1/data/shots-data.csv')

sink('Desktop/workout1/output/shots-data-summary.txt')
summary(temp4)

sink()
