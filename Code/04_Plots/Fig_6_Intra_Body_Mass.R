### Plot Intraspecific Body Mass ###

library(tidyverse)
library(gridExtra)

## Get data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
myct_tidy <- filter(myct, !is.na(Weight.x)) # Remove those without body mass
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

ELN_plot <- ELN_plot + labs(tag = "F")
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

ELC_plot <- ELC_plot + labs(tag = "C")

#### GYR ####

GYR_col <- c("#E69F00")

# Convert weight to z-score

GYR_weight_mean <- mean(GYR$ln_Weight) # Get species mean of weight
GYR_weight_sd <- sd(GYR$ln_Weight) # Get species standard deviation of weight
for(i in 1:length(GYR$ln_Weight)){ # Loop to get z-scores
  GYR$Weight_Z[i] <- (GYR$ln_Weight[i] - GYR_weight_mean) / GYR_weight_sd
}

# Get scaling relationship 

load("Code/04_Plots/LM_CI/LM_CI.Rdata")

model_GYR <- readRDS("Outputs/03_Linear_Models_Within_Species/GYR/M_T_W_model.rds")

#### Table Output ####

table_GYR <- precis(model_GYR, digits = 4, prob = 0.95, depth = 2)
table_GYR

GYR_ribbon <- lm.ci(GYR$Weight_Z, a = table_GYR[41, 1],
                    b = table_GYR[42, 1], a_up_ci = table_GYR[41, 4],
                    a_low_ci = table_GYR[41, 3], b_up_ci = table_GYR[42, 4],
                    b_low_ci = table_GYR[42, 3])

# Set tick breaks

z_1 <- (1.5 - mean(GYR$ln_Weight)) / sd(GYR$ln_Weight)
z_2 <- (2 - mean(GYR$ln_Weight)) / sd(GYR$ln_Weight)
z_3 <- (2.5 - mean(GYR$ln_Weight)) / sd(GYR$ln_Weight)

GYR_plot <- ggplot(GYR, aes(Weight_Z, mean_M)) +
  scale_fill_manual(values = GYR_col) +
  scale_colour_manual(values = GYR_col) +
  scale_x_continuous(breaks = c(z_1, z_2, z_3),
                     labels = c(1.5, 2, 2.5)) +
  geom_polygon(data = GYR_ribbon, aes(x_ribbon, y_ribbon), alpha = 0.3) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  geom_segment(aes(x = min(Weight_Z), xend = max(Weight_Z), 
                   y = table_GYR[41, 1] + table_GYR[42, 1]* min(Weight_Z), yend = table_GYR[41, 1] + table_GYR[42, 1]* max(Weight_Z)), lwd = 1) +
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

GYR_plot <- GYR_plot + labs(tag = "E")
GYR_plot

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

GYN_plot <- GYN_plot + labs(tag = "A")

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

KRA_plot <- KRA_plot + labs(tag = "D")

#### PRM ####

PRM_col <- c("#CC79A7")

PRM_plot <- ggplot(PRM, aes(Weight.x, mean_M)) +
  scale_fill_manual(values = PRM_col) +
  scale_colour_manual(values = PRM_col) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
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

PRM_plot <- PRM_plot + labs(tag = "B")
PRM_plot

svg("Plots/02_Within_Species/01_Cresp_Body_Mass.svg", height = 10, width = 10)
grid.arrange(GYN_plot, PRM_plot,
             ELC_plot, KRA_plot,
             GYR_plot, ELN_plot)
dev.off()
