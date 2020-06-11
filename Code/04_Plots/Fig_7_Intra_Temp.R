### Plot Intraspecific Temperature ###

library(tidyverse)
library(gridExtra)

## Get data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
myct_tidy <- filter(myct_tidy, !is.na(mean_M)) # Remove those without C_resp

## Subset to species

ELN <- filter(myct_tidy, Label == "ELN")
ELC <- filter(myct_tidy, Label == "ELC")
PRM <- filter(myct_tidy, Label == "PRM")
GYR <- filter(myct_tidy, Label == "GYR")
GYN <- filter(myct_tidy, Label == "GYN")
KRA <- filter(myct_tidy, Label == "KRA")

#### ELN ####

ELN_col <- c("#0072B2")

ELN_plot <- ggplot(ELN, aes(mean_Temp, mean_M)) +
  scale_fill_manual(values = ELN_col) +
  scale_colour_manual(values = ELN_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21)) +
  scale_x_continuous(breaks = c(-0.5, 0, 0.5, 1)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("C" ["resp"])) +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

ELN_plot <- ELN_plot + labs(tag = "A")
ELN_plot 

#### ELC ####

ELC_col <- c("#56B4E9")

ELC_plot <- ggplot(ELC, aes(mean_Temp, mean_M)) +
  scale_fill_manual(values = ELC_col) +
  scale_colour_manual(values = ELC_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(22)) +
  scale_y_continuous(breaks = c(0.16, 0.17, 0.18)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("C" ["resp"])) +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

ELC_plot <- ELC_plot + labs (tag = "B")
ELC_plot

#### GYR ####

GYR_col <- c("#E69F00")

GYR_plot <- ggplot(GYR, aes(mean_Temp, mean_M)) +
  scale_fill_manual(values = GYR_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(23)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("C" ["resp"])) +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

GYR_plot <- GYR_plot + labs(tag = "C")
GYR_plot

#### GYN ####

GYN_col <- c("#D55E00")

GYN_plot <- ggplot(GYN, aes(mean_Temp, mean_M)) +
  scale_fill_manual(values = GYN_col) +
  scale_colour_manual(values = GYN_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(24)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("C" ["resp"])) +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text (face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

GYN_plot <- GYN_plot + labs(tag = "D")
GYN_plot

#### KRA ####

KRA_col <- c("#009E73")

KRA_plot <- ggplot(KRA, aes(mean_Temp, mean_M)) +
  scale_fill_manual(values = KRA_col) +
  scale_colour_manual(values = KRA_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(25)) +
  scale_x_continuous(breaks = c(0, 1, 2, 3),
                     labels = c("0.0", "1.0", "2.0", "3.0")) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("C" ["resp"])) +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

KRA_plot <- KRA_plot + labs(tag = "E")
KRA_plot

#### PRM ####

PRM_col <- c("#CC79A7")

PRM_plot <- ggplot(PRM, aes(mean_Temp, mean_M)) +
  scale_fill_manual(values = PRM_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("C" ["resp"])) +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

PRM_plot <- PRM_plot + labs(tag = "F")
PRM_plot


grid.arrange(ELN_plot, ELC_plot,
             GYR_plot, GYN_plot,
             KRA_plot, PRM_plot)
