#### Clustering Analysis ####

library(tidyverse)
library(truncnorm)

myct <- read.csv("Outputs/Combined.csv")
glimpse(myct)

M <- myct_tidy$M
M <- as.data.frame(M)
wss <- (nrow(M)-1)*sum(apply(M,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(M, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Cluster analysis with two groups

fit <- kmeans(M, 2)

aggregate(myct_tidy, by = list(fit$cluster), FUN = mean)

clustering <- data.frame(myct_tidy, fit$cluster)

## Plot

cbp1 <- c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#009E73", "#CC79A7")

ggplot(clustering, aes(x = fit.cluster, fill = sciname)) +
  geom_bar(colour = "black") +
  scale_fill_manual(name = "Species", values = cbp1) +
  scale_x_continuous(breaks = seq(1, 2, 1)) +
  xlab("Cluster") +
  ylab("Number of Individuals") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_text(),
        legend.key=element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))

#### Do with Densities ####

## Get densities

M_values <- data.frame()

load("Functions/PseudoBayes_M_Dist.Rdata")

set.seed(1)

for(i in 1:nrow(myct)){
  ms <- with(myct[i,],
             PseudoBayes_M(d13C, 0.02, # SD based on values from isotope lab
                           Year.x, 10000,
                           DIC, 0.202, 0, 3, # From Tagliabue & Bopp, 2007
                           Suess, 0.202, -0.28, 0, # From Tagliabue & Bopp, 2007. SD same as for DIC
                           -0.07, 0.202, # From Tagliabue & Bopp, 2007. SD same as for DIC
                           Phyto, Phyto_sd, -31, -16.5, # From Magozzi et al. 2017. Based on values for Longhurst biogeographic provinces
                           -0.17, 0.202, # From Clive - per comms. SD same as for DIC
                           UseTroph, UseTrophSe, 2, 5, # From FishBase
                           0.8, 1.1, # From DeNiro & Epstein
                           0, 1.8)) # From Solomon et al. 
  M_values <- rbind(M_values, ms)
}

# Tidy for graph 

M_values <- as.data.frame(M_values)

M_values$sciname <- "sciname"

ELN <- M_values[1:190000,]
colnames(ELN) <- c("M", "sciname")
ELN$sciname <- "Electrona antarctica"

ELC <- M_values[190001:360000,]
colnames(ELC) <- c("M", "sciname")
ELC$sciname <- "Electrona carlsbergi"

GYR <- M_values[360001:560000,]
colnames(GYR) <- c("M", "sciname")
GYR$sciname <- "Gymnoscopelus braueri"

GYN <- M_values[560001:680000,]
colnames(GYN) <- c("M", "sciname")
GYN$sciname <- "Gymnoscopelus nicholsi"

KRA <- M_values[680001:880000,]
colnames(KRA) <- c("M", "sciname")
KRA$sciname <- "Krefftichthys anderssoni"

PRM <- M_values[880001:1080000,]
colnames(PRM) <- c("M", "sciname")
PRM$sciname <- "Protomyctophum bolini"

# Bind

add <- rbind(ELN, ELC, GYR, GYN, KRA, PRM)

M <- add$M
M <- as.data.frame(M)
wss <- (nrow(M)-1)*sum(apply(M,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(M, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Cluster analysis with two groups

fit <- kmeans(M, 3)

aggregate(add, by = list(fit$cluster), FUN = mean)

clustering <- data.frame(add, fit$cluster)

clustering$fit.cluster[clustering$fit.cluster == "1"] <- "c"
clustering$fit.cluster[clustering$fit.cluster == "2"] <- "a"
clustering$fit.cluster[clustering$fit.cluster == "3"] <- "b"

ggplot(clustering, aes(x = fit.cluster, fill = sciname)) +
  geom_bar(colour = "black") +
  scale_fill_manual(name = "Species", values = cbp1) +
  xlab("Cluster") +
  ylab("Count of M Values") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_text(),
        legend.key=element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
