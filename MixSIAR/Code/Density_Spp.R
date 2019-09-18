#### Species Density Plot ####

library(tidyverse)
library(MixSIAR)

post_M <- read.csv("MixSIAR/Outputs/M/Posteriors/M_Post.csv")

## Join with species

myct <- read.csv("Data/Myctophids_Master.csv")
spp <- select(myct, MyNumber, sciname)

post_M_spp <- left_join(post_M, spp, by = "MyNumber")

# Get means

means <- aggregate(post_M_spp, by = list(post_M_spp$sciname), FUN = mean)
colnames(means) <- c("sciname", "M", "MyNumber", "ds")
means <- select(means, sciname, M)

## Plot

cbp1 <- c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#009E73", "deeppink")

ggplot(post_M_spp, aes(x = M))+
  geom_density()+
  facet_grid(.~sciname)

ggplot(post_M_spp, aes(x = M, fill = sciname))+
  geom_density(alpha = 0.5, lwd = 1, colour = "black")+
  geom_vline(data = means, aes(xintercept = M, colour = sciname), lwd = 2)+
  scale_fill_manual(name = "Species", values = cbp1) +
  scale_colour_manual(name = "Species", values = cbp1) +
  scale_y_continuous(limits = c(0, 27), expand = c(0, 0))+
  ylab("Kernel Density")+
  xlab("M Values")+
  theme(panel.background = element_blank(),
        text = element_text(size = 25),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_text(),
        legend.key=element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
