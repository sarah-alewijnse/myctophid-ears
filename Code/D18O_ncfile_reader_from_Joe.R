library(maptools)
library(ggrepel)
library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)
library(raster)
library(ggmap)
library(ggplot2)
library(dplyr)
library(datasets)
library(reshape2)
library(fields)
library(rgeos)
library(gdistance)
library(lubridate)
library(magick)
library(animation)
library(MASS)
library(stargazer)
library(spatstat)
library(viridis)
library(ggthemes)
library(maps)
library(readr)

###reading what files are in the path####
filesNSTemp= list.files('Data/Metadata/Water_Data',
                        pattern='*.nc',full.names=TRUE)

####opening specific file####

D18O = nc_open(filesNSTemp[1])
print(D18O)

print(paste("The file has",D18O$nvars,"variables,",D18O$ndims,"dimensions and",D18O$natts,"NetCDF attributes"))

### Read the whole nc file and individual files ###

attributes(D18O$var)$names

d18o <- ncvar_get(D18O, attributes(D18O$var)$names[1])

Longitude <- ncvar_get(D18O, 'lon')
Latitude <- ncvar_get(D18O, 'lat')
Depth <- ncvar_get(D18O, 'depth')
D18O_var <- ncvar_get(D18O, 'd18o')


### turning them into vectors ###

LongitudeVector=as.vector(Longitude)
LatitudeVector=as.vector(Latitude)
DepthVector=as.vector(Depth)
D18O_vector=as.vector(D18O_var)

## getting dimensions ###

LongitudeDim=dim(Longitude)
LatitudeDim=dim(Latitude)
DepthDim=dim(Depth)

### combining the dimensions and vectors .. making them the same length### 

DepthVector=rep(c(DepthVector), times = LatitudeDim[1]*LongitudeDim[1] )
Depth=sort(DepthVector, decreasing = FALSE)

Longitude=rep(c(LongitudeVector), times = DepthDim*LatitudeDim[1]  )


LatitudeVector=rep(c(LatitudeVector), times = LongitudeDim[1]  )
Latitude=sort(LatitudeVector, decreasing = FALSE)
Latitude=rep(c(Latitude), times = DepthDim[1]  )


D18O_vals <- D18O_vector

whole <- cbind(D18O_vals, Depth, Latitude, Longitude)
whole <- as.data.frame(whole)

### Tidy station data

Station_Data <- read_csv("Data/Metadata/Station_Data.csv")

Station_Data$Lat_Round <- round((Station_Data$Lat_dec + 0.5) * 0.5 )/0.5 - 0.5
Station_Data$Long_Round <- round((Station_Data$Long_dec + 0.5) * 0.5 )/0.5 - 0.5
Station_Data$Depth_Mid <- rowMeans(Station_Data[,5:6])
Station_Data$Depth_Mid_Round <- round(Station_Data$Depth_Mid, -2)

# Change values slightly so we have data

Station_Data[c(1:5, 7:8),12] <- -47.5
Station_Data[c(10, 30), 14] <- 100

### Get results

result <- data.frame()

for(i in 1:nrow(Station_Data)){
  
  Stn <- slice(Station_Data, i)
  
  whole_depth <- filter(whole, Depth == Stn$Depth_Mid_Round)
  whole_depth_lat <- filter(whole_depth, Latitude == Stn$Lat_Round)
  whole_depth_lat_long <- filter(whole_depth_lat, Longitude == Stn$Long_Round)
  whole_depth_lat_long <- as.data.frame(whole_depth_lat_long)
  result <- rbind(result, whole_depth_lat_long)
}
result

## Join and write

Station_Data_Oxy <- cbind(Station_Data, result)

## Tidy and write

Station_Data_Oxy_Tidy <- dplyr::select(Station_Data_Oxy,
                                -Sal,
                                -Depth,
                                -Lat_Round,
                                -Long_Round,
                                -Depth_Mid_Round,
                                -Depth,
                                -Latitude,
                                -Longitude)

write.csv(Station_Data_Oxy_Tidy, "Data/Metadata/Station_Data.csv")
