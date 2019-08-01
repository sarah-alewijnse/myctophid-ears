#### Belcher Comparison ####

library(tidyverse)
library(rtruncnorm)

myct <- read.csv("Outputs/Combined.csv")
myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy$ln_Weight <- log(myct_tidy$Weight.x)

Belcher_MR <- function(param_1, param_1_sd,
                       param_2, param_2_sd,
                       Ln_Weight, reps,
                       param_3, param_3_sd,
                       temp, temp_sd, temp_min, temp_max){
  
  # Calculate distributions with set.seeds to ensure reproducibility
  set.seed(param_1)
  dist_param_1 <- rnorm(reps, param_1, param_1_sd)
  set.seed(param_2)
  dist_param_2 <- rnorm(reps, param_2, param_2_sd)
  set.seed(param_3)
  dist_param_3 <- rnorm(reps, param_3, param_3_sd)
  set.seed(temp)
  dist_temp <- rtruncnorm(reps, temp_min, temp_max, temp, temp_sd)
  set.seed(Sys.time())
  
  # Calculate MR
  dist_MR <- dist_param_1-dist_param_2*Ln_Weight+dist_param_3*dist_temp
  min_MR <- min(dist_MR)
  max_MR <- max(dist_MR)
  max_dens <- which.max(density(dist_MR)$y)
  MR <- density(dist_MR)$x[max_dens]
  sd_MR <- sd(dist_MR)
  result <- data.frame(MR, sd_MR, min_MR, max_MR)
  return(result)
}

with(myct_tidy[4,],
     Belcher_MR(-1.315, 0.468,
                -0.2665, 0.0516,
                ln_Weight, 10000,
                0.0848, 0.0108,
                temp, sd_temp, min_temp, max_temp))

M_values <- data.frame()

for(i in 1:nrow(myct_tidy)){
  ms <- with(myct_tidy[i,],
             Belcher_MR(-1.315, 0.468,
                        -0.2665, 0.0516,
                        ln_Weight, 10000,
                        0.0848, 0.0108,
                        temp, sd_temp, min_temp, max_temp))
  M_values <- rbind(M_values, ms)
}

write.csv(M_values, "Outputs/Belcher_MR.csv", row.names = F)

#### Plot ####

myct_tidy <- cbind(myct_tidy, M_values)

cbPalette <- c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#009E73", "#CC79A7")

ggplot(myct_tidy, aes(MR, M, sciname)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  # Colour error-bars according to species
  geom_errorbarh(aes(xmin = MR - sd_MR,
                     xmax = MR + sd_MR,
                     col = sciname), alpha = 0.40, lwd = 1) +
  geom_errorbar(aes(ymin = M - sd_M, # Vertical
                    ymax = M + sd_M,
                    col = sciname), alpha = 0.40, lwd = 1) +
  geom_point(aes(fill = sciname, shape = sciname), size = 4) + # Colour points according to species
  # Customise the theme
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21)) +
  xlab("Ln(Mass-Specific Respiration)") +
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
