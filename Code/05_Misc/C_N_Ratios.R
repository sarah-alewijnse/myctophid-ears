#### C:N Ratios ####

library(tidyverse)

# Import data

c_n_ratio <- read.csv("Data/CN_Ratio.csv")

# PLot

ggplot(c_n_ratio, aes(Species, C_N_Ratio)) +
  geom_boxplot()

# Anova

spp <- aov(C_N_Ratio ~ Species, data = c_n_ratio)
summary(spp)

# Species averages

aggregate(c_n_ratio$C_N_Ratio, list(c_n_ratio$Species), mean)

# Join with C_resp

c_resp <- read.csv("Data/Myctophids_M_Temp.csv")

joined <- left_join(c_n_ratio, c_resp, by = "MyNumber")

# Plot C_resp

ggplot(joined, aes(mean_M, C_N_Ratio, Species.x)) +
  geom_point(aes(x = mean_M, y = C_N_Ratio, colour = Species.x))

# Plot d13C

ggplot(joined, aes(d13C, C_N_Ratio, Species.x)) +
  geom_point(aes(x = d13C, y = C_N_Ratio, colour = Species.x))

# Plot d13C muscle

ggplot(joined, aes(d13C_musc, C_N_Ratio, Species.x)) +
  geom_point(aes(x = d13C_musc, y = C_N_Ratio, colour = Species.x))
