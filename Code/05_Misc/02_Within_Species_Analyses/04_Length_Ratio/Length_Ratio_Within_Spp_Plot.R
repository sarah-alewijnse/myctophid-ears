### Plot Intraspecific Length Weight Ratio ###

# Load required packages

library(tidyverse)
library(gridExtra)

## Get data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")

# Create a table of max length (SL, mm)

max_SL <- data.frame(Label = c("ELN", "ELC", "GYR", "GYN", "KRA", "PRM"),
                     max_SL = c("115", "93", "162", "165", "74", "66"))

# Filter out crushed otoliths

myct <- filter(myct, Crushed == 0)

# Join to data

myct_SL <- left_join(myct, max_SL, by = "Label")
str(myct_SL)

# Get ratio

myct_SL$Ratio <- myct_SL$SL / as.numeric(myct_SL$max_SL)

myct_tidy <- filter(myct_SL, !is.na(mean_M))
myct_tidy <- filter(myct_tidy, !is.na(Ratio))

## Subset to species

ELN <- filter(myct_tidy, Label == "ELN")
ELC <- filter(myct_tidy, Label == "ELC")
PRM <- filter(myct_tidy, Label == "PRM")
GYR <- filter(myct_tidy, Label == "GYR")
GYN <- filter(myct_tidy, Label == "GYN")

#### ELN ####

ELN_col <- c("#0072B2")

ELN_plot <- ggplot(ELN, aes(Ratio, mean_M)) +
  scale_fill_manual(values = ELN_col) +
  scale_colour_manual(values = ELN_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21)) +
  xlab("Life Stage (SL/species maximum SL)") +
  ylab(expression("C" ["resp"])) +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

ELN_plot <- ELN_plot + labs(tag = "E")
ELN_plot

#### ELC ####

ELC_col <- c("#56B4E9")

ELC_plot <- ggplot(ELC, aes(Ratio, mean_M)) +
  scale_fill_manual(values = ELC_col) +
  scale_colour_manual(values = ELC_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(22)) +
  scale_y_continuous(breaks = c(0.16, 0.17, 0.18)) +
  xlab("Life Stage (SL/species maximum SL)") +
  ylab(expression("C" ["resp"])) +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

ELC_plot <- ELC_plot + labs(tag = "C")
ELC_plot

#### GYR ####

GYR_col <- c("#E69F00")

GYR_plot <- ggplot(GYR, aes(Ratio, mean_M)) +
  scale_fill_manual(values = GYR_col) +
  scale_colour_manual(values = GYR_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(23)) +
  scale_y_continuous(breaks = c(0.16, 0.17, 0.18)) +
  xlab("Life Stage (SL/species maximum SL)") +
  ylab(expression("C" ["resp"])) +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

GYR_plot <- GYR_plot + labs(tag = "D")
GYR_plot

#### GYN ####

GYN_col <- c("#D55E00")

GYN_plot <- ggplot(GYN, aes(Ratio, mean_M)) +
  scale_fill_manual(values = GYN_col) +
  scale_colour_manual(values = GYN_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(24)) +
  xlab("Life Stage (SL/species maximum SL)") +
  ylab(expression("C" ["resp"])) +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

GYN_plot <- GYN_plot + labs(tag = "A")
GYN_plot

#### PRM ####

PRM_col <- c("#CC79A7")

PRM_plot <- ggplot(PRM, aes(Ratio, mean_M)) +
  scale_fill_manual(values = PRM_col) +
  scale_colour_manual(values = PRM_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21)) +
  xlab("Life Stage (SL/species maximum SL)") +
  ylab(expression("C" ["resp"])) +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

PRM_plot <- PRM_plot + labs(tag = "B")
PRM_plot

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/Combined_Graph.svg", height = 10, width = 10)
grid.arrange(GYN_plot, PRM_plot,
             ELC_plot,
             GYR_plot, ELN_plot)
dev.off()
