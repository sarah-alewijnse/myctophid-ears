#### Combined Plots ####

library(tidyverse)
library(gridExtra)

myct <- read.csv("Outputs/Combined.csv")
myct$log_Weight <- log10(myct$Weight.x)
myct$log_Weight_sd <- log10(myct$Weight_SD)
myct$log_Weight_sd[myct$log_Weight_sd == "-Inf"] <- "0"
myct$log_Weight_sd <- as.numeric(myct$log_Weight_sd)
myct$log_Weight_sd <- abs(myct$log_Weight_sd)


#### Plot ####

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

plot1 <- ggplot(myct, aes(temp, M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_errorbarh(aes(xmin = temp - sd_temp, # Horizontal
                     xmax = temp + sd_temp,
                     col = sciname), alpha = 0.40, lwd = 1) + # Colour error-bars according to species
  geom_errorbar(aes(ymin = M - sd_M, # Vertical
                    ymax = M + sd_M,
                    col = sciname), alpha = 0.40, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab("M") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

plot2 <- ggplot(myct, aes(log_Weight, M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = M - sd_M, # Vertical
                    ymax = M + sd_M,
                    col = sciname), alpha = 0.40, lwd = 1) +
  geom_errorbarh(aes(xmin = log_Weight - log_Weight_sd,
                    xmax = log_Weight + log_Weight_sd,
                    col = sciname), alpha = 0.40, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  xlab("log10(Weight) (g)") +
  ylab("M") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

grid.arrange(plot1, plot2)
