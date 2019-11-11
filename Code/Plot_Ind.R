### Plot Individual ###

library(tidyverse)
library(gridExtra)

myct <- read.csv("Myctophids_M_Temp.csv")
myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy <- filter(myct_tidy, !is.na(mean_M))
myct_tidy$ln_Weight <- log(myct_tidy$Weight.x)

## Subset to species

ELN <- filter(myct_tidy, Label == "ELN")
ELC <- filter(myct_tidy, Label == "ELC")
PRM <- filter(myct_tidy, Label == "PRM")
GYR <- filter(myct_tidy, Label == "GYR")
GYN <- filter(myct_tidy, Label == "GYN")
KRA <- filter(myct_tidy, Label == "KRA")

## Plot mass

ELN_col <- c("#56B4E9")

ELN_plot <- ggplot(ELN, aes(ln_Weight, mean_M)) +
  scale_fill_manual(values = ELN_col) +
  scale_colour_manual(values = ELN_col) +
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M, col = ELN_col), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(aes(intercept = 0.1963, slope = 0.0262), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(21)) +
  xlab("ln(Body Mass) (g)") +
  ylab(expression("M" ["oto"])) +
  ggtitle("Electrona antarctica") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

ELN_plot

ELC_col <- c("#0072B2")

ELC_plot <- ggplot(ELC, aes(ln_Weight, mean_M)) +
  scale_fill_manual(values = ELC_col) +
  scale_colour_manual(values = ELC_col) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M, col = ELC_col), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(aes(intercept = 0.1963, slope = 0.0262), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(22)) +
  xlab("ln(Body Mass) (g)") +
  ylab(expression("M" ["oto"])) +
  ggtitle("Electrona carlsbergi") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

ELC_plot

PRM_col <- c("#CC79A7")

PRM_plot <- ggplot(PRM, aes(ln_Weight, mean_M)) +
  scale_fill_manual(values = PRM_col) +
  scale_colour_manual(values = PRM_col) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M, col = PRM_col), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(aes(intercept = 0.1963, slope = 0.0262), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(21)) +
  xlab("ln(Body Mass) (g)") +
  ylab(expression("M" ["oto"])) +
  ggtitle("Protomyctophum bolini") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

PRM_plot

GYR_col <- c("#E69F00")

GYR_plot <- ggplot(GYR, aes(ln_Weight, mean_M)) +
  scale_fill_manual(values = GYR_col) +
  scale_colour_manual(values = GYR_col) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M, col = GYR_col), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(aes(intercept = 0.2162, slope = -0.0159), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(23)) +
  xlab("ln(Body Mass) (g)") +
  ylab(expression("M" ["oto"])) +
  ggtitle("Gymnoscopelus braueri") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

GYR_plot

GYN_col <- c("#D55E00")

GYN_plot <- ggplot(GYN, aes(ln_Weight, mean_M)) +
  scale_fill_manual(values = GYN_col) +
  scale_colour_manual(values = GYN_col) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M, col = "GYN_col"), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(aes(intercept = 0.1963, slope = 0.0262), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(24)) +
  xlab("ln(Body Mass) (g)") +
  ylab(expression("M" ["oto"])) +
  ggtitle("Gymnoscopelus nicholsi") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text (face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

GYN_plot

KRA_col <- c("#009E73")

KRA_plot <- ggplot(KRA, aes(ln_Weight, mean_M)) +
  scale_fill_manual(values = KRA_col) +
  scale_colour_manual(values = KRA_col) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M, col = KRA_col), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(aes(intercept = 0.1963, slope = 0.0262), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(25)) +
  xlab("ln(Body Mass) (g)") +
  ylab(expression("M" ["oto"])) +
  ggtitle("Krefftichtys anderssoni") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

KRA_plot

grid.arrange(ELN_plot, ELC_plot,
             GYR_plot, GYN_plot,
             KRA_plot, PRM_plot)

## Plot temp

ELN_col <- c("#56B4E9")

ELN_plot <- ggplot(ELN, aes(mean_Temp, mean_M)) +
  scale_fill_manual(values = ELN_col) +
  scale_colour_manual(values = ELN_col) +
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M, col = ELN_col), alpha = 0.3, lwd = 1) +
  geom_errorbarh(aes(xmin = mean_Temp - sd_Temp, # Horizontal
                    xmax = mean_Temp + sd_Temp, col = ELN_col), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(aes(intercept = 0.1963, slope = 0.0262), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(21)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("M" ["oto"])) +
  ggtitle("Electrona antarctica") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

ELN_plot

ELC_col <- c("#0072B2")

ELC_plot <- ggplot(ELC, aes(mean_Temp, mean_M)) +
  scale_fill_manual(values = ELC_col) +
  scale_colour_manual(values = ELC_col) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M, col = ELC_col), alpha = 0.3, lwd = 1) +
  geom_errorbarh(aes(xmin = mean_Temp - sd_Temp, # Horizontal
                     xmax = mean_Temp + sd_Temp, col = ELC_col), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(aes(intercept = 0.1963, slope = 0.0262), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(22)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("M" ["oto"])) +
  ggtitle("Electrona carlsbergi") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

ELC_plot

PRM_col <- c("#CC79A7")

PRM_plot <- ggplot(PRM, aes(mean_Temp, mean_M)) +
  scale_fill_manual(values = PRM_col) +
  scale_colour_manual(values = PRM_col) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M, col = PRM_col), alpha = 0.3, lwd = 1) +
  geom_errorbarh(aes(xmin = mean_Temp - sd_Temp, # Horizontal
                     xmax = mean_Temp + sd_Temp, col = PRM_col), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(aes(intercept = 0.1963, slope = 0.0262), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(21)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("M" ["oto"])) +
  ggtitle("Protomyctophum bolini") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

PRM_plot

GYR_col <- c("#E69F00")

GYR_plot <- ggplot(GYR, aes(mean_Temp, mean_M)) +
  scale_fill_manual(values = GYR_col) +
  scale_colour_manual(values = GYR_col) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M, col = GYR_col), alpha = 0.3, lwd = 1) +
  geom_errorbarh(aes(xmin = mean_Temp - sd_Temp, # Horizontal
                     xmax = mean_Temp + sd_Temp, col = GYR_col), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(aes(intercept = 0.2162, slope = -0.0159), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(23)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("M" ["oto"])) +
  ggtitle("Gymnoscopelus braueri") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

GYR_plot

GYN_col <- c("#D55E00")

GYN_plot <- ggplot(GYN, aes(mean_Temp, mean_M)) +
  scale_fill_manual(values = GYN_col) +
  scale_colour_manual(values = GYN_col) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M, col = GYN_col), alpha = 0.3, lwd = 1) +
  geom_errorbarh(aes(xmin = mean_Temp - sd_Temp, # Horizontal
                     xmax = mean_Temp + sd_Temp, col = GYN_col), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(aes(intercept = 0.1963, slope = 0.0262), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(24)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("M" ["oto"])) +
  ggtitle("Gymnoscopelus nicholsi") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text (face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

GYN_plot

KRA_col <- c("#009E73")

KRA_plot <- ggplot(KRA, aes(mean_Temp, mean_M)) +
  scale_fill_manual(values = KRA_col) +
  scale_colour_manual(values = KRA_col) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - sd_M, # Vertical
                    ymax = mean_M + sd_M, col = KRA_col), alpha = 0.3, lwd = 1) +
  geom_errorbarh(aes(xmin = mean_Temp - sd_Temp, # Horizontal
                     xmax = mean_Temp + sd_Temp, col = KRA_col), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(aes(intercept = 0.1963, slope = 0.0262), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(25)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("M" ["oto"])) +
  ggtitle("Krefftichtys anderssoni") +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

KRA_plot

grid.arrange(ELN_plot, ELC_plot,
             GYR_plot, GYN_plot,
             KRA_plot, PRM_plot)
