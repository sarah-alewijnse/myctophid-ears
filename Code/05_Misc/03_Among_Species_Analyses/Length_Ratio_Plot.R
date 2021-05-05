#### Length Ratio Graph ####

# Load required packages

library(tidyverse)
library(gridExtra)

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
myct_tidy <- filter(myct, !is.na(Weight.x))
myct_tidy <- filter(myct_tidy, !is.na(mean_M))
myct_tidy$ln_Weight <- log(myct_tidy$Weight.x)
myct_tidy <- filter(myct_tidy, Crushed == 0)

#### Get length ratio ####

# Create a table of max length (SL, mm)

max_SL <- data.frame(Label = c("ELN", "ELC", "GYR", "GYN", "KRA", "PRM"),
                     max_SL = c("115", "93", "162", "165", "74", "66"))

# Join to data

myct_SL <- left_join(myct_tidy, max_SL, by = "Label")
str(myct_SL)

# Get ratio

myct_SL$Ratio <- myct_SL$SL / as.numeric(myct_SL$max_SL)

#### Temperature ####

cbPalette <- c("#0072B2", "#56B4E9", "#E69F00", "#D55E00", "#CC79A7") # Colourblind friendly palette

plot1 <- ggplot(myct_SL, aes(Ratio, mean_M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 21)) +
  xlab(expression("SL/Maximum SL (mm)")) +
  ylab(expression("C" ["resp"])) +
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

# Autosave as SVG

svg("Outputs/04_Misc/03_Among_Species/Length_Ratio_Graph.svg", height = 5, width = 8)
plot1
dev.off()
