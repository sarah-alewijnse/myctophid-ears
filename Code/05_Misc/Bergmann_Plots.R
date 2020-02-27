#### Bergmann Plots ####

library(tidyverse)
library(gridExtra)
library(rethinking)

myct <- read.csv("Myctophids_M_Temp_Bel.csv")
myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy <- filter(myct_tidy, !is.na(mean_M))
myct_tidy$ln_Weight <- log(myct_tidy$Weight.x)


#### Plot - Body Mass vs. Temp ####

cbPalette <- c("#0072B2", "#56B4E9", "#E69F00", "#D55E00", "#009E73", "#CC79A7")

plot1 <- ggplot(myct, aes(mean_Temp, Weight.x, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_errorbarh(aes(xmin = mean_Temp - se_Temp, # Horizontal
                     xmax = mean_Temp + se_Temp, col = sciname), alpha = 0.3, lwd = 1 )+ # Colour error-bars according to species
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab("Body Mass") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

plot1

#### Plot - Body Mass vs. Latiutde ####

plot2 <- ggplot(myct, aes(Lat_dec.x, log10(Weight.x), sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  xlab("Latitude") +
  ylab("Body Mass - Log 10") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

plot2

#### Plot - Latiude vs. M_oto ####

plot3 <- ggplot(myct, aes(Lat_dec.x, mean_M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  xlab("Latitude") +
  ylab("M_oto") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

plot3

#### Plot - Latitude vs. Temperature ####

plot4 <- ggplot(myct, aes(Lat_dec.x, mean_Temp, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  ylab(expression('Temperature ('*~degree*C*')')) +
  xlab("Latiutde") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

plot4
