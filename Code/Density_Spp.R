#### Species Density Plot - M ####

library(tidyverse)

post_M <- read.csv("Outputs/M/Posteriors/M_Post.csv")

## Join with species

myct <- read.csv("Myctophids_Master.csv")
spp <- select(myct, MyNumber, sciname)

post_M_spp <- left_join(post_M, spp, by = "MyNumber")

# Get means

means <- aggregate(post_M_spp, by = list(post_M_spp$sciname), FUN = mean)
colnames(means) <- c("sciname", "M", "MyNumber", "ds")
means <- select(means, sciname, M)

# Make sciname a factor

post_M_spp$sciname <- factor(post_M_spp$sciname, levels = 
                               c("Gymnoscopelus nicholsi",
                                 "Protomyctophum bolini",
                                 "Electrona carlsbergi",
                                 "Krefftichthys anderssoni",
                                 "Gymnoscopelus braueri",
                                 "Electrona antarctica"))

## Plot

cbp1 <- c("#D55E00", "#CC79A7", "#56B4E9", "#009E73", "#E69F00", "#0072B2")

ggplot(post_M_spp, aes(x = M, fill = sciname))+
  geom_density()+
  facet_wrap(~sciname, ncol = 1) +
  scale_fill_manual(name = "Species", values = cbp1) +
  scale_colour_manual(name = "Species", values = cbp1) +
  scale_y_continuous(limits = c(0, 27), expand = c(0, 0))+
  ylab("Kernel Density")+
  xlab("M Values")+
  geom_vline(data = means, aes(xintercept = M), lwd = 1)+
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))

#### Species Density Plot - Temperature ####

post_Temp <- read.csv("Outputs/Temperature/Posteriors/Temp_Post.csv")

## Join with species

myct <- read.csv("Myctophids_Master.csv")
spp <- select(myct, MyNumber, sciname)

post_Temp_spp <- left_join(post_Temp, spp, by = "MyNumber")

# Get means

means <- aggregate(post_Temp_spp, by = list(post_Temp_spp$sciname), FUN = mean)
colnames(means) <- c("sciname", "Temp", "MyNumber", "ds")
means <- select(means, sciname, Temp)

# Make sciname a factor

post_Temp_spp$sciname <- factor(post_Temp_spp$sciname, levels = 
                               c("Gymnoscopelus braueri",
                                 "Electrona antarctica",
                                 "Krefftichthys anderssoni",
                                 "Protomyctophum bolini",
                                 "Gymnoscopelus nicholsi",
                                 "Electrona carlsbergi"))

## Plot

cbp1 <- c("#E69F00", "#0072B2", "#009E73", "#CC79A7", "#D55E00", "#56B4E9")

ggplot(post_Temp_spp, aes(x = Temp, fill = sciname))+
  geom_density() +
  facet_wrap(~sciname, ncol = 1) +
  scale_fill_manual(name = "Species", values = cbp1) +
  scale_colour_manual(name = "Species", values = cbp1) +
  scale_y_continuous(limits = c(0, 0.27), expand = c(0, 0))+
  ylab("Kernel Density")+
  xlab("Temperature")+
  geom_vline(data = means, aes(xintercept = Temp), lwd = 1)+
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
