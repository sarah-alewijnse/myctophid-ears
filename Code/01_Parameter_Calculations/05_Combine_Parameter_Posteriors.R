#### Combine Posteriors ####

# Combines posteriors from all individuals into a single files

# Load packages

library(tidyverse)
library(MixSIAR) # Gibbs sampler

#### C_resp ####

# Set working directory

setwd("~/PhD/GitHub/mytophid-ears/Outputs/01_Parameter_Calculations/01_M/Posteriors")

# Read all .csv files in folder

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

tbl_with_sources <-
  list.files(pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))

names <- tbl_with_sources$filename
names <- substring(names, 11, 17)
names <- gsub (" ", "", names, fixed = TRUE)

tbl_with_sources$filename <- names

# Convert to table

post <- tbl_with_sources
post <- select(post, post_M, filename)
colnames(post) <- c("M", "MyNumber")

# Write into file

write.csv(post, "M_Post.csv", row.names = FALSE)

#### Temperature ####

# Set working directory

setwd("~/PhD/GitHub/mytophid-ears/Outputs/01_Parameter_Calculations/02_Temperature/Posteriors")

# Read all .csv files in folder

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

tbl_with_sources <-
  list.files(pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))

names <- tbl_with_sources$filename
names <- substring(names, 13, 19)
names <- gsub(" ", "", names, fixed = TRUE)
names <- gsub(".", "", names, fixed = TRUE)

tbl_with_sources$filename <- names

# Convert into table

post <- tbl_with_sources
post <- select(post, Temp, filename)
colnames(post) <- c("Temp", "MyNumber")

# Write into file

write.csv(post, "Temp_Post.csv", row.names = FALSE)

#### Belcher Log Oxygen Consumption (ul/mg/kg) ####

# Set working directory

setwd("~/PhD/GitHub/mytophid-ears/Outputs/01_Parameter_Calculations/03_Oxygen_Consumption/Posteriors")

# Read all .csv files in folder

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

tbl_with_sources <-
  list.files(pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))

names <- tbl_with_sources$filename
names <- substring(names, 8, 14)
names <- gsub(" ", "", names, fixed = TRUE)
names <- gsub(".", "", names, fixed = TRUE)

tbl_with_sources$filename <- names

# Convert into table

post <- tbl_with_sources
post <- select(post, Metabol, filename)
colnames(post) <- c("Metabol", "MyNumber")

# Write into file

write.csv(post, "Belcher_Post.csv", row.names = FALSE)