#### Species Density Plot - M ####

# Figure 2

# Load tidyverse

library(tidyverse)

# Load posterior predictions

post_M <- read.csv("Outputs/01_Parameter_Calculations/01_M/Posteriors/M_Post.csv")

# Join to species

myct <- read.csv("Data/Myctophids_Master.csv")
spp <- select(myct, MyNumber, sciname)

post_M_spp <- left_join(post_M, spp, by = "MyNumber")

# Get means of posteriors

means <- aggregate(post_M_spp, by = list(post_M_spp$sciname), FUN = mean)
colnames(means) <- c("sciname", "M", "MyNumber", "ds")
means <- select(means, sciname, M)

# Add labels 

means$label <- c("F", "C", "E", "A", "D", "B")

# Add labels to posteriors

post_M_spp$label <- post_M_spp$sciname
post_M_spp$label[post_M_spp$label == "Electrona antarctica"] <- "F"
post_M_spp$label[post_M_spp$label == "Gymnoscopelus braueri"] <- "E"
post_M_spp$label[post_M_spp$label == "Krefftichthys anderssoni"] <- "D"
post_M_spp$label[post_M_spp$label == "Electrona carlsbergi"] <- "C"
post_M_spp$label[post_M_spp$label == "Protomyctophum bolini"] <- "B"
post_M_spp$label[post_M_spp$label == "Gymnoscopelus nicholsi"] <- "A"

# Add intercepts from body mass temp model 

a <- data.frame(label = c("A",
                          "B",
                          "C",
                          "D",
                          "E",
                          "F"),
                M = c(0.149,
                      0.170,
                      0.172,
                      0.192,
                      0.201,
                      0.214))

# Plot (autosaves as SVG)

spp_labs <- c("Gymnoscopelus nicholsi", "Protomyctophum bolini",
              "Electrona carlsbergi", "Krefftichthys anderssoni",
              "Gymnoscopelus braueri", "Electrona antarctica")
names(spp_labs) <- c("A", "B", "C", "D", "E", "F")

cbp1 <- c("#D55E00", "#CC79A7", "#56B4E9", "#009E73", "#E69F00", "#0072B2") # Colourblind friendly palette

svg("Plots/01_Among_Species/01_Cresp_Density.svg", height = 9, width = 7)
ggplot(post_M_spp, aes(x = M, fill = label))+
  geom_density()+
  facet_wrap(~label, ncol = 1, strip.position = "top", labeller = labeller(label = spp_labs)) +
  scale_fill_manual(name = "label", values = cbp1) +
  scale_colour_manual(name = "label", values = cbp1) +
  scale_y_continuous(limits = c(0, 27), expand = c(0, 0))+
  ylab("Kernel Density")+
  xlab(expression("C" ["resp"]))+
  geom_vline(data = means, aes(xintercept = M), lwd = 1)+
  geom_vline(data = a, aes(xintercept = M), lwd = 1, linetype = "dotted") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
dev.off()
