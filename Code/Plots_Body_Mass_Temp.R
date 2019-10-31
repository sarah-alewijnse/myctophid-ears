#### Body Mass Temp Plots ####

library(tidyverse)
library(gridExtra)

myct <- read.csv("Myctophids_M_Temp.csv")
myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy <- filter(myct_tidy, !is.na(mean_M))
myct_tidy$ln_Weight <- log(myct_tidy$Weight.x)

## Get confidence intervals

min_temp <- -1.87780193 - 1.698682
max_temp <- 2.98660327 + 1.689986

preds <- data.frame(x = seq(min_temp, max_temp, 0.07502794))
y <- 0.1835 + preds * -0.0038
ymin <- 0.1501 + preds * -0.0088
ymax <- 0.2185 + preds * 0.0011

#### Plot ####

cbPalette <- c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#009E73", "#CC79A7")

plot1 <- ggplot(myct, aes(mean_Temp, mean_M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_errorbarh(aes(xmin = mean_Temp - sd_Temp, # Horizontal
                     xmax = mean_Temp + sd_Temp), alpha = 0.2, lwd = 1) + # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M), alpha = 0.2, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  geom_abline(intercept = 0.1835, slope = -0.0038, lwd = 1) +
  geom_ribbon(x = preds, y = y, ymin = ymin, ymax = ymax) +
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 24)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab("M Values") +
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

plot2 <- ggplot(myct_tidy, aes(ln_Weight, mean_M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M), alpha = 0.2, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 24)) +
  xlab("Ln(Body Mass) (g)") +
  ylab("M Values") +
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

grid.arrange(plot1, plot2)
