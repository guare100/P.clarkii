##################################################
# Title: XXX                                     #
# Scripts led by:                                #
# Tommaso Cancellario & Simone Guareschi         #
#                                                #
# Creation: 2022 - 05 - 05                       #
# Last update: 2023 - 05 - 20                    #
##################################################

#--------------------------------#
# Climatic variables preparation #
#--------------------------------#

# Load libraries ----
library(sf) # <- GIS
library(raster) # <- GIS
library(rgdal) # <- GIS

# Set working directory ----
# setwd("./")

# Calculate monthly mean temperature and precipitation for each year----
# Repeat for Tmax - Tmin - Prec

for(i in 1:12){
print(paste("-----", i, "-----"))
# Change .min based on variable type
pattern = paste0(".min_", i, "_") # <- Period 1 and 2
# pattern = ifelse(i<10, paste0(".pr_0", i, "_"), paste0(".pr_", i, "_")) # <- Period 3 and 4

var <- list.files(path = "./", # Folder containing variables
                   pattern = pattern, all.files = TRUE, full.names = TRUE)
var <- stack(var)

# Parallelized
beginCluster()
var.mean <- clusterR(var, fun = mean)
endCluster()

# Save rasters
writeRaster(var.mean, filename = paste0("./_", i, "_mean_XX_YY.tif"),
            format = "GTiff", overwrite = FALSE)
}; tm(i, var)

#plot(var.mean)

# Crop Variables to World extention ----
# Load .shp
world <- st_read("./World_No_Lakes.shp")
plot(world[1])

#-------#
# T min #
#-------#
tmin_41_50 <- list.files(path = "./tmin_mean_1941_1950/",
                         pattern = ".tif$", all.files = TRUE,
                         full.names = TRUE)
  
tmin_66_75 <- list.files(path = "./tmin_mean_1966_1975/",
                         pattern = ".tif$", all.files = TRUE,
                         full.names = TRUE)

tmin_91_00 <- list.files(path = "./tmin_mean_1991_2000/",
                         pattern = ".tif$", all.files = TRUE,
                         full.names = TRUE)

tmin_09_18 <- list.files(path = "./tmin_mean_2009_2018/",
                         pattern = '.tif$', all.files = TRUE,
                         full.names = TRUE)

tmin_41_50 <- stack(tmin_41_50)
tmin_66_75 <- stack(tmin_66_75)
tmin_91_00 <- stack(tmin_91_00)
tmin_09_18 <- stack(tmin_09_18)


#-------#
# T MAX #
#-------#
tmax_41_50 <- list.files(path = "./tmax_mean_1941_1950/",
                         pattern = ".tif$", all.files = TRUE,
                         full.names = TRUE)

tmax_66_75 <- list.files(path = "./tmax_mean_1966_1975/",
                         pattern = ".tif$", all.files = TRUE,
                         full.names = TRUE)

tmax_91_00 <- list.files(path = "./tmax_mean_1991_2000/",
                         pattern = ".tif$", all.files = TRUE,
                         full.names = TRUE)

tmax_09_18 <- list.files(path = "./tmax_mean_2009_2018/",
                         pattern = ".tif$", all.files = TRUE,
                         full.names = TRUE)

tmax_41_50 <- stack(tmax_41_50)
tmax_66_75 <- stack(tmax_66_75)
tmax_91_00 <- stack(tmax_91_00)
tmax_09_18 <- stack(tmax_09_18)

#------#
# PREC #
#------#
prec_41_50 <- list.files(path = "./prec_mean_1941_1950/",
                         pattern = ".tif$", all.files = TRUE,
                         full.names = TRUE)

prec_66_75 <- list.files(path = "./prec_mean_1966_1975/",
                         pattern = ".tif$", all.files = TRUE,
                         full.names = TRUE)

prec_91_00 <- list.files(path = "./prec_mean_1991_2000/",
                         pattern = ".tif$", all.files = TRUE,
                         full.names = TRUE)

prec_09_18 <- list.files(path = "./prec_mean_2009_2018/",
                         pattern = ".tif$", all.files = TRUE,
                         full.names = TRUE)

prec_41_50 <- stack(prec_41_50)
prec_66_75 <- stack(prec_66_75)
prec_91_00 <- stack(prec_91_00)
prec_09_18 <- stack(prec_09_18)

# Crop and mask 
# Karger et al (2017) Scientific Data. CHELSA CRUST. 
# Naprecg convention:
# CHELSA_<variable>_<z-scale>_<month>_<Version>_land.tif
# tmax = monthly mean of daily maximum temperature [°C*10]
# tmin = monthly mean of daily minimum temperature [°C*10]

#### TMin

# 41-50
min_value <- unique(minValue(tmin_41_50)) # Check min values
tmpfilter <- tmin_41_50[[1]] > min_value # Create a mask with the min value
plot(tmpfilter)
tmin_41_50.sub <- mask(tmin_41_50, tmpfilter, maskvalue=0) # Mask main raster
minValue(tmin_41_50.sub); maxValue(tmin_41_50.sub) # Check min and max values
tmin_41_50.sub <- tmin_41_50.sub/10 # Divide by 10
minValue(tmin_41_50.sub); maxValue(tmin_41_50.sub) # Check again
tmin_41_50.sub <- mask(crop(tmin_41_50.sub, extent(world)), world) # Crop with World
plot(tmin_41_50.sub[[1]])

# Save rasters
for(i in 1:12){
  writeRaster(tmin_41_50.sub [[i]],
  filename = paste0("./crop/tmin/tmin_41_50/", names(tmin_41_50.sub)[i], "_crop.tif"),
              format = "GTiff", overwrite = FALSE)
}; rm(i)

# 66-75
min_value <- unique(minValue(tmin_66_75)) # Check min values
tmpfilter <- tmin_66_75[[1]] > min_value # Create a mask with the min value
plot(tmpfilter)
tmin_66_75.sub <- mask(tmin_66_75, tmpfilter, maskvalue=0) # Mask main raster
minValue(tmin_66_75.sub); maxValue(tmin_66_75.sub) # Check min and max values
tmin_66_75.sub <- tmin_66_75.sub/10 # Divide by 10
minValue(tmin_66_75.sub); maxValue(tmin_66_75.sub) # Check again
tmin_66_75.sub <- mask(crop(tmin_66_75.sub, extent(world)), world) # Crop with World
plot(tmin_66_75.sub[[1]])

# Save rasters
for(i in 1:12){
  writeRaster(tmin_66_75.sub [[i]],
  filename = paste0("./crop/tmin/tmin_66_75/", names(tmin_66_75.sub)[i], "_crop.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)

## for 1991-2000 and 2009-2018 a new procedure is needed. Values are K/10.
## Info available here: https://chelsa-climate.org/downloads/ see technical specifications 2.1
## e.g. <- ((tmin_91_00/10)-273.15) # from kelvin to Celsius (data are K/10)

# 91-00
tmin_91_00.sub <- (tmin_91_00/10) - 273.15
tmin_91_00.sub <- mask(crop(tmin_91_00.sub, extent(world)), world) # Crop with World
plot(tmin_91_00.sub[[1]])

# Save rasters
for(i in 1:12){ 
  writeRaster(tmin_91_00.sub[[i]], 
  filename = paste0("./crop/tmin/tmin_91_00/", names(tmin_91_00.sub)[i], "_crop.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)

# 09-18
tmin_09_18.sub <- (tmin_09_18/10) - 273.15
tmin_09_18.sub <- mask(crop(tmin_09_18.sub, extent(world)), world) # Crop with World
plot(tmin_09_18.sub[[1]])

# Save rasters
for(i in 1:12){
  writeRaster(tmin_09_18.sub[[i]],
  filename = paste0("./raster_variables/crop/tmin/tmin_09_18/", names(tmin_09_18.sub)[i], "_crop.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)



##### TMax

# 41-50
min_value <- unique(minValue(tmax_41_50)) # Check min values
tmpfilter <- tmax_41_50[[1]] > min_value # Create a mask with the min value
plot(tmpfilter)
tmax_41_50.sub <- mask(tmax_41_50, tmpfilter, maskvalue=0) # Mask main raster
minValue(tmax_41_50.sub); maxValue(tmax_41_50.sub) # Check min and max values
tmax_41_50.sub <- tmax_41_50.sub/10 # Divide by 10
minValue(tmax_41_50.sub); maxValue(tmax_41_50.sub) # Check again
tmax_41_50.sub <- mask(crop(tmax_41_50.sub, extent(world)), world) # Crop with World
plot(tmax_41_50.sub[[1]])

# Save rasters
for(i in 1:12){
  writeRaster(tmax_41_50.sub [[i]],
  filename = paste0("./crop/tmax/tmax_41_50/", names(tmax_41_50.sub)[i], "_crop.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)

# 66-75
min_value <- unique(minValue(tmax_66_75)) # Check min values
tmpfilter <- tmax_66_75[[1]] > min_value # Create a mask with the min value
plot(tmpfilter)
tmax_66_75.sub <- mask(tmax_66_75, tmpfilter, maskvalue=0) # Mask main raster
minValue(tmax_66_75.sub); maxValue(tmax_66_75.sub) # Check min and max values
tmax_66_75.sub <- tmax_66_75.sub/10 # Divide by 10
minValue(tmax_66_75.sub); maxValue(tmax_66_75.sub) # Check again
tmax_66_75.sub <- mask(crop(tmax_66_75.sub, extent(world)), world) # Crop with World
plot(tmax_66_75.sub[[1]])

# Save rasters
for(i in 1:12){
  writeRaster(tmax_66_75.sub [[i]], 
  filename = paste0("./crop/tmax/tmax_66_75/", names(tmax_66_75.sub)[i], "_crop.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)

# 91-00
tmax_91_00.sub <- (tmax_91_00/10) - 273.15
tmax_91_00.sub <- mask(crop(tmax_91_00.sub, extent(world)), world) # Crop with World
plot(tmax_91_00.sub[[1]])

# Save rasters
for(i in 1:12){
  writeRaster(tmax_91_00.sub [[i]], 
  filename = paste0("./crop/tmax/tmax_91_00/", names(tmax_91_00.sub)[i], "_crop.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)

# 09-18
tmax_09_18.sub <- (tmax_09_18/10) - 273.15
tmax_09_18.sub <- mask(crop(tmax_09_18.sub, extent(world)), world) # Crop with World
plot(tmax_09_18.sub[[1]])

# Save rasters
for(i in 1:12){
  writeRaster(tmax_09_18.sub [[i]], 
  filename = paste0("./crop/tmax/tmax_09_18/", names(tmax_09_18.sub)[i], "_crop.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)



#### Prec

# 41-50
min_value <- unique(minValue(prec_41_50)) # Check min values
tmpfilter <- prec_41_50[[1]] > min_value # Create a mask with the min value
plot(tmpfilter)
prec_41_50.sub <- mask(prec_41_50, tmpfilter, maskvalue=0) # Mask main raster
minValue(prec_41_50.sub); maxValue(prec_41_50.sub) # Check min and max values
prec_41_50.sub <- mask(crop(prec_41_50.sub, extent(world)), world) # Crop with World
plot(prec_41_50.sub[[1]])

#Save rasters 
for(i in 1:12){
  writeRaster(prec_41_50.sub [[i]],
  filename = paste0("./crop/prec/prec_41_50/", names(prec_41_50.sub)[i], "_crop.tif"),
              format = "GTiff", overwrite = FALSE)
}; rm(i)

## 66-75
min_value <- unique(minValue(prec_66_75)) # Check min values
tmpfilter <- prec_66_75[[1]] > min_value # Create a mask with the min value
plot(tmpfilter)
prec_66_75.sub <- mask(prec_66_75, tmpfilter, maskvalue=0) # Mask main raster
minValue(prec_66_75.sub); maxValue(prec_66_75.sub) # Check min and max values
prec_66_75.sub <- mask(crop(prec_66_75.sub, extent(world)), world) # Crop with World
plot(prec_66_75.sub[[1]])

# Save rasters
for(i in 1:12){
  writeRaster(prec_66_75.sub [[i]], 
  filename = paste0("./crop/prec/prec_66_75/", names(prec_66_75.sub)[i], "_crop.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)


# 91-00
prec_91_00.sub <- mask(crop(prec_91_00, extent(world)), world) # Crop with World
plot(prec_91_00.sub[[1]])
prec_91_00.sub <- prec_91_00.sub/100

# Save rasters
for(i in 1:12){
  writeRaster(prec_91_00.sub[[i]], 
  filename = paste0("./crop/prec/prec_91_00/", names(prec_91_00.sub)[i], "_crop.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)

# 09-2018
prec_09_18.sub <- mask(crop(prec_09_18, extent(world)), world) # Crop with World
prec_09_18.sub <- prec_09_18.sub/100
plot(prec_09_18.sub[[1]])

# Save rasters
for(i in 1:12){
  writeRaster(prec_09_18.sub[[i]], 
  filename = paste0("./crop/prec/prec_09_18/", names(prec_09_18.sub)[i], "_crop.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)


# Check maps extention and resolution ----
extent(tmin_41_50.sub); extent(tmin_66_75.sub); extent(tmin_91_00.sub); extent(tmin_09_18.sub) # tmin
extent(tmax_41_50.sub); extent(tmax_66_75.sub); extent(tmax_91_00.sub); extent(tmax_09_18.sub) # tmax
extent(prec_41_50.sub); extent(prec_66_75.sub); extent(prec_91_00.sub); extent(prec_09_18.sub) # prec

xres(tmin_41_50.sub); xres(tmin_66_75.sub); xres(tmin_91_00.sub); xres(tmin_09_18.sub) # tmin
xres(tmax_41_50.sub); xres(tmax_66_75.sub); xres(tmax_91_00.sub); xres(tmax_09_18.sub) # tmax
xres(prec_41_50.sub); xres(prec_66_75.sub); xres(prec_91_00.sub); xres(prec_09_18.sub) # prec

# Clean workspace
rm(list=ls()); gc()