#### Differences Between Species - d13C ####

library(tidyverse)
library(car)
library(FSA)

myct <- read.csv("Data/Myctophids_Master.csv")

## Check for normal distributon with K-S test

hist(myct$d13C)

avg <- mean(myct$d13C, na.rm = TRUE)
sd <- sd(myct$d13C, na.rm = TRUE)

ks.test(myct$d13C, "pnorm", avg, sd)

# Normal distribution ok

## Check for homogeneity of variances with Levene's test

leveneTest(d13C ~ sciname, data = myct)

# Homogeneity of variances not ok

## Proceed with Kruska-Wallis Test

kruskal.test(d13C ~ sciname, data = myct)

# Significant difference

## Proceed with post-hoc analysis (Dunn-Test)

# Order groups by median

boxplot(d13C ~ sciname,
        data = myct)

myct$sciname = factor(myct$sciname, 
                     levels = c("Electrona antarctica", 
                                "Gymnoscopelus braueri",
                                "Krefftichthys andersoni",
                                "Protomyctophum bolini",
                                "Electrona carlsbergi",
                                "Gymnoscopelus nicholsi"))

# Carry out test

myct_na <- select(myct, sciname, d13C)
myct_na <- na.omit(myct_na)

d13C <- myct_na$d13C
sciname <- myct_na$sciname

dunnTest(d13C ~ sciname,
         method = "bonferroni")

# Significant differences between:
  # E. antarctica & E. carlsbergi
  # E. antarctica & G. nicholsi
  # E. carlsbergi & G. braueri
  # E. carlsbergi & K. andersoni
  # G. nicholsi & K. andersoni
  # E. antarctica & P. bolini
  # G. braueri & P. bolini
  # K. andersoni & P. bolini
  # G. braueri & G. nicholsi
