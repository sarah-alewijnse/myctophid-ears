#### ANOVA - Difference in PRM Muscle with Location ####

library(tidyverse)

# Load and check data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

# Get PRM

PRM <- filter(myct, Label == "PRM")
glimpse(PRM)

#### Muscle d13C vs. Latitude ####

PRM_col <- c("#CC79A7")

PRM_plot <- ggplot(PRM, aes(Lat_dec.x.x, d13C_musc)) +
  scale_fill_manual(values = PRM_col) +
  scale_colour_manual(values = PRM_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21)) +
  # Add labels
  xlab("Latitude") +
  ylab("d13C Muscle") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

PRM_plot

musc_mod <- lm(d13C_musc ~ Lat_dec.x.x, data = PRM)
summary(musc_mod)

#### Otolith d13C vs. Latitude ####

PRM_plot <- ggplot(PRM, aes(Lat_dec.x.x, d13C)) +
  scale_fill_manual(values = PRM_col) +
  scale_colour_manual(values = PRM_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21)) +
  # Add labels
  xlab("Latitude") +
  ylab("d13C Otolith") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

PRM_plot

oto_mod <- lm(d13C ~ Lat_dec.x.x, data = PRM)
summary(oto_mod)
