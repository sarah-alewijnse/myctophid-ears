#### Summary Statistics ####

# Get the summary statistics for various posteriors

# Load packages

library(tidyverse)

# Create function for standard error

se <- function(x) sd(x)/sqrt(length(x))

#### C_resp (M) ####

# Load file

M <- read.csv("Outputs/01_Parameter_Calculations/01_M/Posteriors/M_Post.csv")

# Get means

M_means <- aggregate(M, by = list(M$MyNumber), FUN = mean)
colnames(M_means) <- c("MyNumber", "mean_M", "ds")
M_means <- select(M_means, MyNumber, mean_M)

# Get standard deviations

M_sd <- aggregate(M, by = list(M$MyNumber), FUN = sd)
colnames(M_sd) <- c("MyNumber", "sd_M", "ds")
M_sd <- select(M_sd, MyNumber, sd_M)

# Get standard errors

M_se <- aggregate(M, by = list(M$MyNumber), FUN = se)
colnames(M_se) <- c("MyNumber", "se_M", "ds")
M_se <- select(M_se, MyNumber, se_M)

# Join

M <- left_join(M_means, M_sd, M_se, by = "MyNumber") %>%
  left_join(., M_se, by = "MyNumber")

#### Temperature ####

# Read file

Temp <- read.csv("Outputs/01_Parameter_Calculations/02_Temperature/Posteriors/Temp_Post.csv")

# Get means

Temp_means <- aggregate(Temp, by = list(Temp$MyNumber), FUN = mean)
colnames(Temp_means) <- c("MyNumber", "mean_Temp", "ds")
Temp_means <- select(Temp_means, MyNumber, mean_Temp)

# Get standard deviation

Temp_sd <- aggregate(Temp, by = list(Temp$MyNumber), FUN = sd)
colnames(Temp_sd) <- c("MyNumber", "sd_Temp", "ds")
Temp_sd <- select(Temp_sd, MyNumber, sd_Temp)

# Get standard error

Temp_se <- aggregate(Temp, by = list(Temp$MyNumber), FUN = se)
colnames(Temp_se) <- c("MyNumber", "se_Temp", "ds")
Temp_se <- select(Temp_se, MyNumber, se_Temp)

# Join

Temp <- left_join(Temp_means, Temp_sd, by = "MyNumber") %>%
  left_join(., Temp_se, by = "MyNumber")

#### Oxygen Consumption - Belcher et al. 2019 ####

# Load

Bel <- read.csv("Outputs/01_Parameter_Calculations/03_Oxygen_Consumption/Posteriors/Belcher_Post.csv")

# Change to oxygen consumption rather than log oxygen consumption

e <- exp(1)
Bel$Metabol_Convert <- e^Bel$Metabol

# Get means

Bel_means <- aggregate(Bel, by = list(Bel$MyNumber), FUN = mean)
colnames(Bel_means) <- c("MyNumber", "mean_log_Metabol", "ds", "mean_Metabol")
Bel_means <- select(Bel_means, MyNumber, mean_Metabol, mean_log_Metabol)

# Get standard deviations

Bel_sd <- aggregate(Bel, by = list(Bel$MyNumber), FUN = sd)
colnames(Bel_sd) <- c("MyNumber", "sd_log_Metabol", "ds", "sd_Metabol")
Bel_sd <- select(Bel_sd, MyNumber, sd_Metabol, sd_log_Metabol)

# Get standard errors

Bel_se <- aggregate(Bel, by = list(Bel$MyNumber), FUN = se)
colnames(Bel_se) <- c("MyNumber", "se_log_Metabol", "ds", "se_Metabol")
Bel_se <- select(Bel_se, MyNumber, se_Metabol, se_log_Metabol)

# Combine

Bel <- left_join(Bel_means, Bel_sd, by = "MyNumber") %>%
  left_join(., Bel_se, by = "MyNumber")

#### Combine all into single file ####

outputs <- left_join(Temp, M, by = "MyNumber") %>%
  left_join(., Bel, by = "MyNumber")

# Direct conversion of log mean oxygen consumption to oxygen consumption (ul/mg/h)

Bel$Dir_mean_Metabol <- e^(Bel$mean_log_Metabol)
Bel_Dir <- select(Bel, MyNumber, Dir_mean_Metabol)

outputs_d <- left_join(outputs, Bel_Dir, by = "MyNumber")

# Write into file

myct <- read.csv("Data/Myctophids_Master.csv")

all <- left_join(myct, outputs_d, by = "MyNumber")

write.csv(all, "Data/Myctophids_M_Temp_Bel.csv", row.names = FALSE)
