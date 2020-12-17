#### Comparison between Methods ####

library(tidyverse)

oto_der <- read.csv("Outputs/01_Parameter_Calculations/04_Conversions/This_Study_Otolith_Derived.csv")
allo_der <- read.csv("Outputs/01_Parameter_Calculations/04_Conversions/This_Study_Equation_Derived.csv")
Bel_2020 <- read.csv("Outputs/04_Misc/03_Bel_2020_Data_Conversion/Bel_2020_Conversion.csv")

# Tidy

oto_der$Species <- c("Electrona antarctica",
                     "Electrona carlsbergi",
                     "Gymnoscopelus braueri",
                     "Gymnoscopelus nicholsi",
                     "Krefftichthys anderssoni",
                     "Protomyctophum bolini")
oto_der <- oto_der[, -c(1,2)]
colnames(oto_der) <- c("C_resp_oto", "mg_kg_oto", "Species")

# Tidy

allo_der <- allo_der[, -c(1, 3)]
colnames(allo_der) <- c("Species", "mg_kg_allo", "C_resp_allo")

# Join

combined <- left_join(oto_der, allo_der, by = "Species")

# Get differences

combined$diff_mg_kg <- abs(combined$mg_kg_oto - combined$mg_kg_allo)

# Get as a percent of otolith derived

combined$diff_mg_kg_percent <- (combined$diff_mg_kg/combined$mg_kg_oto) * 100
combined$diff_mg_kg_percent <- round(combined$diff_mg_kg_percent, 2)

write.csv(combined, "Outputs/04_Misc/05_Method_Comparison/Method_Comparison.csv", row.names = F)
