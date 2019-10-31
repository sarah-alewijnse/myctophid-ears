#### Summary Statistics ####

library(tidyverse)

## M

M <- read.csv("Outputs/M/Posteriors/M_Post.csv")

M_means <- aggregate(M, by = list(M$MyNumber), FUN = mean)
colnames(M_means) <- c("MyNumber", "mean_M", "ds")
M_means <- select(M_means, MyNumber, mean_M)

M_sd <- aggregate(M, by = list(M$MyNumber), FUN = sd)
colnames(M_sd) <- c("MyNumber", "sd_M", "ds")
M_sd <- select(M_sd, MyNumber, sd_M)

M <- left_join(M_means, M_sd, by = "MyNumber")

## Temp

Temp <- read.csv("Outputs/Temperature/Posteriors/Temp_Post.csv")

Temp_means <- aggregate(Temp, by = list(Temp$MyNumber), FUN = mean)
colnames(Temp_means) <- c("MyNumber", "mean_Temp", "ds")
Temp_means <- select(Temp_means, MyNumber, mean_Temp)

Temp_sd <- aggregate(Temp, by = list(Temp$MyNumber), FUN = sd)
colnames(Temp_sd) <- c("MyNumber", "sd_Temp", "ds")
Temp_sd <- select(Temp_sd, MyNumber, sd_Temp)

Temp <- left_join(Temp_means, Temp_sd, by = "MyNumber")

## Belcher Metabolic Rate

Bel <- read.csv("Outputs/Belcher/Posteriors/Belcher_Post.csv")

e <- exp(1)
Bel$Metabol_Convert <- e^Bel$Metabol

Bel_means <- aggregate(Bel, by = list(Bel$MyNumber), FUN = mean)
colnames(Bel_means) <- c("MyNumber", "mean_log_Metabol", "ds", "mean_Metabol")
Bel_means <- select(Bel_means, MyNumber, mean_Metabol, mean_log_Metabol)

Bel_sd <- aggregate(Bel, by = list(Bel$MyNumber), FUN = sd)
colnames(Bel_sd) <- c("MyNumber", "sd_log_Metabol", "ds", "sd_Metabol")
Bel_sd <- select(Bel_sd, MyNumber, sd_Metabol, sd_log_Metabol)

Bel <- left_join(Bel_means, Bel_sd, by = "MyNumber")

## Combine

outputs <- left_join(Temp, M, by = "MyNumber")

myct <- read.csv("Myctophids_Master.csv")

all <- left_join(myct, outputs, by = "MyNumber")

write.csv(all, "Myctophids_M_Temp.csv", row.names = FALSE)

## With metabol

myct <- read.csv("Myctophids_M_Temp.csv")

all <- left_join(myct, Bel, by = "MyNumber")

write.csv(all, "Myctophids_M_Temp_Bel.csv", row.names = FALSE)
