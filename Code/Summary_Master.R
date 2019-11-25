#### Summary Statistics ####

library(tidyverse)

## Create function for standard error

se <- function(x) sd(x)/sqrt(length(x))

## M

M <- read.csv("Outputs/M/Posteriors/M_Post.csv")

M_means <- aggregate(M, by = list(M$MyNumber), FUN = mean)
colnames(M_means) <- c("MyNumber", "mean_M", "ds")
M_means <- select(M_means, MyNumber, mean_M)

M_sd <- aggregate(M, by = list(M$MyNumber), FUN = sd)
colnames(M_sd) <- c("MyNumber", "sd_M", "ds")
M_sd <- select(M_sd, MyNumber, sd_M)

M_se <- aggregate(M, by = list(M$MyNumber), FUN = se)
colnames(M_se) <- c("MyNumber", "se_M", "ds")
M_se <- select(M_se, MyNumber, se_M)

M <- left_join(M_means, M_sd, M_se, by = "MyNumber") %>%
  left_join(., M_se, by = "MyNumber")

## Temp

Temp <- read.csv("Outputs/Temperature/Posteriors/Temp_Post.csv")

Temp_means <- aggregate(Temp, by = list(Temp$MyNumber), FUN = mean)
colnames(Temp_means) <- c("MyNumber", "mean_Temp", "ds")
Temp_means <- select(Temp_means, MyNumber, mean_Temp)

Temp_sd <- aggregate(Temp, by = list(Temp$MyNumber), FUN = sd)
colnames(Temp_sd) <- c("MyNumber", "sd_Temp", "ds")
Temp_sd <- select(Temp_sd, MyNumber, sd_Temp)

Temp_se <- aggregate(Temp, by = list(Temp$MyNumber), FUN = se)
colnames(Temp_se) <- c("MyNumber", "se_Temp", "ds")
Temp_se <- select(Temp_se, MyNumber, se_Temp)

Temp <- left_join(Temp_means, Temp_sd, by = "MyNumber") %>%
  left_join(., Temp_se, by = "MyNumber")

## Belcher Metabolic Rate

Bel <- read.csv("Outputs/Belcher/Posteriors/Belcher_Post.csv")

# Change to oxygen consumption rather than log oxygen consumption
e <- exp(1)
Bel$Metabol_Convert <- e^Bel$Metabol

Bel_means <- aggregate(Bel, by = list(Bel$MyNumber), FUN = mean)
colnames(Bel_means) <- c("MyNumber", "mean_log_Metabol", "ds", "mean_Metabol")
Bel_means <- select(Bel_means, MyNumber, mean_Metabol, mean_log_Metabol)

Bel_sd <- aggregate(Bel, by = list(Bel$MyNumber), FUN = sd)
colnames(Bel_sd) <- c("MyNumber", "sd_log_Metabol", "ds", "sd_Metabol")
Bel_sd <- select(Bel_sd, MyNumber, sd_Metabol, sd_log_Metabol)

Bel_se <- aggregate(Bel, by = list(Bel$MyNumber), FUN = se)
colnames(Bel_se) <- c("MyNumber", "se_log_Metabol", "ds", "se_Metabol")
Bel_se <- select(Bel_se, MyNumber, se_Metabol, se_log_Metabol)

Bel <- left_join(Bel_means, Bel_sd, by = "MyNumber") %>%
  left_join(., Bel_se, by = "MyNumber")

## Combine

outputs <- left_join(Temp, M, by = "MyNumber") %>%
  left_join(., Bel, by = "MyNumber")

myct <- read.csv("Myctophids_Master.csv")

all <- left_join(myct, outputs, by = "MyNumber")

write.csv(all, "Myctophids_M_Temp_Bel.csv", row.names = FALSE)
