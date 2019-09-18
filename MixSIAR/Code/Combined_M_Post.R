#### Combine Posteriors ####

setwd("~/PhD/GitHub/mytophid-ears/MixSIAR/Outputs/M/Posteriors")

library(tidyverse)
library(MixSIAR)

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

tbl_with_sources <-
  list.files(pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))

names <- tbl_with_sources$filename
names <- substring(names, 11, 17)
names <- gsub (" ", "", names, fixed = TRUE)

tbl_with_sources$filename <- names

post <- tbl_with_sources
post <- select(post, post_M, filename)
colnames(post) <- c("M", "MyNumber")

write.csv(post, "M_Post.csv", row.names = FALSE)
