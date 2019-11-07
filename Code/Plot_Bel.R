#### Graphs - Belcher RMR Estimates ####

library(tidyverse)

options(max.print=999999)

myct <- read.csv("Myctophids_M_Temp_Bel.csv")
glimpse(myct)

cbPalette <- c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#009E73", "#CC79A7")

  plot1 <- ggplot(myct, aes(mean_Metabol, mean_M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_errorbarh(aes(xmin = mean_Metabol - sd_Metabol, # Horizontal
                     xmax = mean_Metabol + sd_Metabol), alpha = 0.2, lwd = 1) + # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M), alpha = 0.2, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  geom_abline(intercept = 0.1859, slope = -0.0047, lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 24)) +
  xlab("Mass-Specific Oxygen Consumption") +
  ylab("M Values") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines
plot1
