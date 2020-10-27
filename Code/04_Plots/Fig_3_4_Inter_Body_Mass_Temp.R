#### Body Mass Temp Plots ####

library(tidyverse)
library(gridExtra)

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
myct_tidy <- filter(myct, !is.na(Weight.x))
myct_tidy <- filter(myct_tidy, !is.na(mean_M))
myct_tidy$ln_Weight <- log(myct_tidy$Weight.x)


#### Temperature ####

cbPalette <- c("#0072B2", "#56B4E9", "#E69F00", "#D55E00", "#009E73", "#CC79A7")

plot1 <- ggplot(myct_tidy, aes(mean_Temp, mean_M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
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

svg("Plots/01_Among_Species/03_Cresp_Temperature.svg", height = 5, width = 8)
plot1
dev.off()

#### Body mass ####

plot2 <- ggplot(myct_tidy, aes(ln_Weight, mean_M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  xlab("ln(Body Mass) (g)") +
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

svg("Plots/01_Among_Species/02_Cresp_Body_Mass.svg", height = 5, width = 8)
plot2
dev.off()
