### DIC from Maps ###

# Load the requisit packages

library(ggmap)
library(maps)
library(dplyr)
library(jpeg)
library(ggplot2)
library(grid)

# Load the data - choose one

library(readr)
Myctophids <- read_csv("Data/Myctophids.csv", 
                       col_types = cols(Lat_dec = col_number(), 
                                        Long_dec = col_number()))

list <- filter(Myctophids, Otolith != "NA")
list <- select(list, Lat_dec, Long_dec)
list <- unique(list)

write.csv(list, "Data/Metadata/Water_Data/Water_Data.csv", row.names = F)

##### Read in Data ######

list <- read.csv("Data/Metadata/Water_Data/Water_Data.csv")

data <- list[20,] # Change number for each data point

# Load DIC map

DIC_map <- readJPEG("Data/Metadata/Water_Data/Maps/DIC_Map.jpg")

# Plot DIC 

plot(1:2, type='n', main="", xlab="x", ylab="y", xlim = c(-180, 180), ylim = c(-90, 90))
lim <- par()
rasterImage(DIC_map, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
points(data$Long_dec, data$Lat_dec, cex = 0.5, pch = 16, col = "red")

# Load Seuss map

seuss_map <- readJPEG("Data/Metadata/Water_Data/Maps/Seuss_map.jpg")

# Plot Seuss

plot(1:2, type='n', main="", xlab="x", ylab="y", xlim = c(-180, 180), ylim = c(-90, 90))
lim <- par()
rasterImage(seuss_map, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
points(data$Long_dec, data$Lat_dec, cex = 0.5, pch = 16, col = "red")

###
# Get colours by hand and add to csv file
###
