#### Combine M and Temperature ####

library(tidyverse)

m <- read.csv("Outputs/M_Values.csv")
temp <- read.csv("Outputs/Temperature.csv")

temp <- select(temp,
               MyNumber,
               mean_temp,
               sd_temp)

## Combine

m_temp <- left_join(m, temp, by = "MyNumber")

write.csv(m_temp, "Outputs/Combined.csv", row.names = F)
