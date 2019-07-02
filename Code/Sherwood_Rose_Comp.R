#### Sherwood & Rose Comparsion ####

library(tidyverse)

d <- read.csv("Data/Sherwood_Rose_Myct.csv")
d <- select(d, -Troph)
d <- na.omit(d)
d_other <- filter(d, Family != "Myctophidae")
d_myct <- filter(d, Family == "Myctophidae")

# Plot - d13C vs Kcaud

ggplot(data = d_other, aes(x = K_caud, y = d13C)) +
  geom_point(aes(colour = Paper, size = 5)) +
  geom_smooth(method = "lm") +
  geom_point(data = d_myct, aes(x = K_caud, y = d13C, colour = sciname, size = 5))

plot(d_myct$K_caud, d_myct$d13C)

# Plot - d18O vs d13C

ggplot(data = d_other, aes(x = d18O, y = d13C)) +
  geom_point(aes(colour = Paper, size = 5)) +
  geom_smooth(method = "lm") +
  geom_point(data = d_myct, aes(x = d18O, y = d13C, colour = sciname, size = 5))
