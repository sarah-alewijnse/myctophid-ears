#### Belcher Comparison ####

library(tidyverse)
library(rethinking)
library(lme4)
library(lmerTest)

myct <- read.csv("Outputs/Combined.csv")

myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy$log10_Weight <- log10(myct_tidy$Weight.x)

## Get base estimate

myct_tidy$log_MR <- with(myct_tidy, -1.315-0.2665*log(Weight.x)+0.0848*temp)

## Test

mod <- lmer(M ~ log_MR + (1|sciname), data = myct_tidy)
summary(mod)

## Plot

ggplot(myct_tidy, aes(x = log_MR, y = M)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_errorbar(aes(ymin = M - sd_M, # Vertical
                    ymax = M + sd_M,
                    col = sciname), alpha = 0.40, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  xlab("ln(Mass-Specific Respiration)") +
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


