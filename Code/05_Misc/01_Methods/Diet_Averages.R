#### Diet d13C Mean and Range ####

library(tidyverse)

# Get diet data

source <- read.csv("Data/MixSIAR_Data/myct_source.csv")

diet <- filter(source, Sources == "Diet")

# Get master data

myct <- read.csv("Data/Myctophids_Master.csv")

spp <- select(myct, MyNumber, Label)

# Join

myct_diet <- left_join(diet, spp, by = "MyNumber")

#### Get averages ####

mean_diet <- aggregate(myct_diet, FUN = mean, by = list(myct_diet$Label))

min_diet <- aggregate(myct_diet, FUN = min, by = list(myct_diet$Label))
max_diet <- aggregate(myct_diet, FUN = max, by = list(myct_diet$Label))

diet_avgs <- data.frame(Species = mean_diet$Group.1,
                        mean_diet = mean_diet$Meand13C,
                        min_diet = min_diet$Meand13C,
                        max_diet = max_diet$Meand13C)
diet_avgs

write.csv(diet_avgs, "Data/MixSIAR_Data/Diet_Averages.csv", row.names = F)
