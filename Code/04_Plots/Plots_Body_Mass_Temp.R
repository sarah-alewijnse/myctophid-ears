#### Body Mass Temp Plots ####

library(tidyverse)
library(gridExtra)
library(rethinking)

myct <- read.csv("Myctophids_M_Temp_Bel.csv")
myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy <- filter(myct_tidy, !is.na(mean_M))
myct_tidy$ln_Weight <- log(myct_tidy$Weight.x)


#### Plot ####

cbPalette <- c("#0072B2", "#56B4E9", "#E69F00", "#D55E00", "#009E73", "#CC79A7")

plot1 <- ggplot(myct, aes(mean_Temp, mean_M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_errorbarh(aes(xmin = mean_Temp - se_Temp, # Horizontal
                     xmax = mean_Temp + se_Temp, col = sciname), alpha = 0.3, lwd = 1 )+ # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - se_M, # Vertical
                    ymax = mean_M + se_M, col = sciname), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  #geom_abline(intercept = 0.1835, slope = -0.0038, lwd = 1) +
  #geom_ribbon(x = preds, y = y, ymin = ymin, ymax = ymax) +
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  xlab(expression('Temperature ('*~degree*C*')')) +
  ylab(expression("M" ["oto"])) +
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

# Do in base

colours <- cbPalette[as.numeric(myct_tidy$sciname)]

shapes <- c(21, 22, 23, 24, 25, 21)
shapes <- shapes[as.numeric(myct_tidy$sciname)]

plot(mean_M ~ mean_Temp, data = myct_tidy, col = "black", bg = colours, pch = shapes)
  with(myct_tidy,
       arrows(x0 = mean_Temp, y0 = mean_M - se_M,
              x1 = mean_Temp, y1 = mean_M + se_M, code = 0, col = colours))
  with(myct_tidy,
       arrows(x0 = mean_Temp - se_Temp, y0 = mean_M,
              x1 = mean_Temp + se_Temp, y1 = mean_M, code = 0, col = colours))
  
model_M_T_W <- readRDS("Outputs/M_T_W/M_T_W_model.rds")
post <- extract.samples(model_M_T_W)
mu <- link(model_M_T_W, data = mod_list)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.95)
temp_seq <- seq(from = min(myct_tidy$mean_Temp), to = max(myct_tidy$mean_Temp, length.out = length(post)))
lines(temp_seq, mu.mean)


## Body mass

plot2 <- ggplot(myct_tidy, aes(ln_Weight, mean_M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  # Colour error-bars according to species
  geom_errorbar(aes(ymin = mean_M - se_M, # Vertical
                    ymax = mean_M + se_M, col = sciname), alpha = 0.3, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  xlab("ln(Body Mass) (g)") +
  ylab(expression("M" ["oto"])) +
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
