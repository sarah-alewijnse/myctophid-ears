#### Graphs - Belcher RMR Estimates ####

library(tidyverse)

options(max.print=999999)

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

cbPalette <- c("#0072B2", "#56B4E9", "#E69F00", "#D55E00", "#009E73", "#CC79A7")

svg("Plots/01_Among_Species/04_Cresp_Oxygen_Consumption.svg", width = 10, height = 7)
ggplot(myct, aes(mean_Metabol, mean_M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_errorbarh(aes(xmin = mean_Metabol - se_Metabol, # Horizontal
                     xmax = mean_Metabol + se_Metabol, col = sciname), lwd = 1) + # Colour error-bars according to species
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  xlab(expression(paste("Oxygen Consumption (", mu, "l O" ["2"], " mg" ^"-1", " h" ^"-1", " )"))) +
  ylab(expression("C" ["resp"])) +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines
dev.off()
