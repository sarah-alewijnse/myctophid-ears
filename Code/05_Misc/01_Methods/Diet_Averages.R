#### Diet d13C Mean and SD ####

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

sd_diet <- aggregate(myct_diet, FUN = sd, by = list(myct_diet$Label))

diet_avgs <- data.frame(Species = mean_diet$Group.1,
                        mean_diet = mean_diet$Meand13C,
                        sd_diet = sd_diet$Meand13C)
diet_avgs

write.csv(diet_avgs, "Data/MixSIAR_Data/Diet_Averages.csv", row.names = F)
