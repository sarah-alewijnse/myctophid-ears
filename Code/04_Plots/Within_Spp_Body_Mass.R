### Plot Intraspecific Body Mass ###

library(tidyverse)
library(gridExtra)

## Get data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
myct_tidy <- filter(myct, Weight_SD == "0") # Remove those without body mass
myct_tidy <- filter(myct_tidy, !is.na(mean_M)) # Remove those without C_resp
myct_tidy$ln_Weight <- log(myct_tidy$Weight.x) # Convert weight to log weight

## Subset to species

ELN <- filter(myct_tidy, Label == "ELN")
ELC <- filter(myct_tidy, Label == "ELC")
PRM <- filter(myct_tidy, Label == "PRM")
GYR <- filter(myct_tidy, Label == "GYR")
GYN <- filter(myct_tidy, Label == "GYN")
KRA <- filter(myct_tidy, Label == "KRA")

#### ELN ####

ELN_col <- c("#0072B2")

ELN_plot <- ggplot(ELN, aes(ln_Weight, mean_M)) +
  scale_fill_manual(values = ELN_col) +
  scale_colour_manual(values = ELN_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21)) +
  xlab("ln(Body Mass) (g)") +
  ylab(expression("C" ["resp"])) +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

ELN_plot <- ELN_plot + labs(tag = "A")
ELN_plot

#### ELC ####

ELC_col <- c("#56B4E9")

ELC_plot <- ggplot(ELC, aes(ln_Weight, mean_M)) +
  scale_fill_manual(values = ELC_col) +
  scale_colour_manual(values = ELC_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(22)) +
  scale_y_continuous(breaks = c(0.16, 0.17, 0.18)) +
  xlab("ln(Body Mass) (g)") +
  ylab(expression("C" ["resp"])) +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

ELC_plot <- ELC_plot + labs(tag = "B")

#### GYR ####

GYR_col <- c("#E69F00")

GYR_plot <- ggplot(GYR, aes(ln_Weight, mean_M)) +
  scale_fill_manual(values = GYR_col) +
  scale_colour_manual(values = GYR_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(23)) +
  xlab("ln(Body Mass) (g)") +
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

#### GYN ####

GYN_col <- c("#D55E00")

GYN_plot <- ggplot(GYN, aes(ln_Weight, mean_M)) +
  scale_fill_manual(values = GYN_col) +
  scale_colour_manual(values = GYN_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(24)) +
  xlab("ln(Body Mass) (g)") +
  ylab(expression("C" ["resp"])) +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

GYN_plot <- GYN_plot + labs(tag = "D")

#### KRA ####

KRA_col <- c("#009E73")

KRA_plot <- ggplot(KRA, aes(ln_Weight, mean_M)) +
  scale_fill_manual(values = KRA_col) +
  scale_colour_manual(values = KRA_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(25)) +
  xlab("ln(Body Mass) (g)") +
  ylab(expression("C" ["resp"])) +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

KRA_plot <- KRA_plot + labs(tag = "E")

#### PRM ####

# Convert weight to z-score

PRM_weight_mean <- mean(PRM$ln_Weight) # Get species mean of weight
PRM_weight_sd <- sd(PRM$ln_Weight) # Get species standard deviation of weight
for(i in 1:length(PRM$ln_Weight)){ # Loop to get z-scores
  PRM$Weight_Z[i] <- (PRM$ln_Weight[i] - PRM_weight_mean) / PRM_weight_sd
}

PRM_col <- c("#CC79A7")

# Get scaling relationship 

load("LM_CI.Rdata")

PRM_ribbon <- lm.ci(PRM$Weight_Z, a = 0.168,
                    b = 0.012, a_up_ci = 0.178,
                    a_low_ci = 0.158, b_up_ci = 0.022,
                    b_low_ci = 0.0012)

# Set tick breaks

z_1 <- (-0.5 - mean(PRM$ln_Weight)) / sd(PRM$ln_Weight)
z_2 <- (0 - mean(PRM$ln_Weight)) / sd(PRM$ln_Weight)
z_3 <- (0.5 - mean(PRM$ln_Weight)) / sd(PRM$ln_Weight)

PRM_plot <- ggplot(PRM, aes(Weight_Z, mean_M)) +
  scale_fill_manual(values = PRM_col) +
  scale_colour_manual(values = PRM_col) +
  scale_x_continuous(breaks = c(z_1, z_2, z_3),
                     labels = c(-0.5, 0, 0.5)) +
  geom_polygon(data = PRM_ribbon, aes(x_ribbon, y_ribbon), alpha = 0.3) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  geom_segment(aes(x = min(Weight_Z), xend = max(Weight_Z), 
                   y = 0.168 + 0.012* min(Weight_Z), yend = 0.168 + 0.012* max(Weight_Z)), lwd = 1) +
  # Customise the theme
  scale_shape_manual(values = c(21)) +
  xlab("ln(Body Mass) (g)") +
  ylab(expression("C" ["resp"])) +
  # Add error-bars using sd
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  # Print the minor gridlines

PRM_plot <- PRM_plot + labs(tag = "F")

grid.arrange(ELN_plot, ELC_plot,
             GYR_plot, GYN_plot,
             KRA_plot, PRM_plot)

