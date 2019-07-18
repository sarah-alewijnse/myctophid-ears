#### Joining all Data ####

library(tidyverse)

my_data <- read.csv("Data/My_Data.csv")

## Select

my_data_tidy <- dplyr::select(my_data, Family, sciname,
                              d13C, d18O, AspectRatio, UseTroph,
                              DepthRangeDeep)

Fam <- dplyr::select(my_data, Family, sciname)
Fam <- unique(Fam)
Fam <- Fam[order(Fam$sciname),]

## Average per species

my_data_avg <- aggregate(my_data_tidy, by = list(my_data_tidy$sciname), FUN = mean)

## Get n per species

n <- count(my_data, sciname)

## Put together

data <- cbind(my_data_avg, n, Fam)

data_tidy <- data[, 4:11]

data_tidy <- data_tidy[, c(8, 6, 7, 1:5)]
data_tidy$Paper <- "My_Data"
data_tidy$Myct <- "Other"
colnames(data_tidy) <- c("Family", "sciname", "n", "d13C", "d18O", "K_caud", "Troph", "Max_depth", "Paper", "Myct")

## Read in Sherwood & Rose

S_R <- read.csv("Data/Sherwood_Rose_Myct.csv")
S_R <- dplyr::select(S_R, -Length)

full <- rbind(data_tidy, S_R)

write.csv(full, "Data/My_Data_and_SR.csv", row.names = F)
