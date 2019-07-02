#### Initial Plots ####

library(tidyverse)

myct <- read.csv("Data/Myctophids_Master.csv")

# Plot by species

ggplot(data = myct, aes(sciname, d13C)) +
  geom_boxplot()

# Plot with body size

ggplot(data = myct, aes(log10(Weight.x), d13C)) +
  geom_point(aes(color = sciname, size = 10))

# Plot with d18O

ggplot(data = myct, aes(d18O, d13C)) +
  geom_point(aes(color = sciname, size = 10))

# Plot with depth

ggplot(data = myct, aes(Min_Depth, d13C)) +
  geom_point(aes(color = sciname))

ggplot(data = myct, aes(Max_Depth, d13C)) +
  geom_point(aes(color = sciname))

ggplot(data = myct, aes(Max_Depth)) +
  geom_histogram(aes(fill = sciname))

