library(tidyverse)

myct <- read.csv("Data/Myctophids_M_Temp.csv")
myct_num <- select(myct, Weight.x, mean_Temp)

aggregate(myct_num, by = list(myct$sciname), min)
aggregate(myct_num, by = list(myct$sciname), max)
