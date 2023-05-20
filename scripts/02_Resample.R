##################################################
# Title: XXX                                     #
# Scripts led by:                                #
# Tommaso Cancellario & Simone Guareschi         #
#                                                #
# Creation: 2022 - 05 - 05                       #
# Last update: 2023 - 05 - 20                    #
##################################################

#-----------------------------------------#
# Climatic variables preparation: resample#
#-----------------------------------------#

# Load libraries ----
library(sf) # <- GIS
library(raster) # <- GIS
library(rgdal) # <- GIS


# Set working directory ----
# setwd("./")

# Create resampled maps ----

# Load elevation raster
elevation <- raster("./wc2.1_10m_elev.tif")

#-------#
# T min #
#-------#
tmin_41_50 <- list.files(path = "./crop/tmin/tmin_41_50/", 
                         pattern = ".tif$", all.files = TRUE, 
                         full.names = TRUE)

tmin_66_75 <- list.files(path = "./crop/tmin/tmin_66_75/", 
                         pattern = ".tif$", all.files = TRUE, 
                         full.names = TRUE)

tmin_91_00 <- list.files(path = "./crop/tmin/tmin_91_00/", 
                         pattern = ".tif$", all.files = TRUE, 
                         full.names = TRUE)

tmin_09_18 <- list.files(path = "./crop/tmin/tmin_09_18/", 
                         pattern = '.tif$', all.files = TRUE, 
                         full.names = TRUE)

tmin_41_50 <- stack(tmin_41_50)
tmin_66_75 <- stack(tmin_66_75)
tmin_91_00 <- stack(tmin_91_00)
tmin_09_18 <- stack(tmin_09_18)

# Resample according to elevation raster
tmin_41_50_res <- resample(x = tmin_41_50, y = elevation, method = "bilinear")
tmin_66_75_res <- resample(x = tmin_66_75, y = elevation, method = "bilinear")
tmin_91_00_res <- resample(x = tmin_91_00, y = elevation, method = "bilinear")
tmin_09_18_res <- resample(x = tmin_09_18, y = elevation, method = "bilinear")

# Check extention and resolution
extent(tmin_41_50_res) == extent(elevation); xres(tmin_41_50_res) == xres(elevation)
extent(tmin_66_75_res) == extent(elevation); xres(tmin_66_75_res) == xres(elevation)
extent(tmin_91_00_res) == extent(elevation); xres(tmin_91_00_res) == xres(elevation)
extent(tmin_09_18_res) == extent(elevation); xres(tmin_09_18_res) == xres(elevation)

# Save rasters
for(i in 1:12){
  writeRaster(tmin_66_75_res [[i]], 
  filename = paste0("./resemble/tmin/tmin_66_75/", names(tmin_66_75_res)[i], "_res.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)

#-------# 
# T MAX #
#-------#
tmax_41_50 <- list.files(path = "./crop/tmax/tmax_41_50/", 
                         pattern = ".tif$", all.files = TRUE, 
                         full.names = TRUE)

tmax_66_75 <- list.files(path = "./crop/tmax/tmax_66_75/", 
                         pattern = ".tif$", all.files = TRUE, 
                         full.names = TRUE)

tmax_91_00 <- list.files(path = "./crop/tmax/tmax_91_00/", 
                         pattern = ".tif$", all.files = TRUE, 
                         full.names = TRUE)

tmax_09_18 <- list.files(path = "./raster_variables/crop/tmax/tmax_09_18/", 
                         pattern = ".tif$", all.files = TRUE, 
                         full.names = TRUE)

tmax_41_50 <- stack(tmax_41_50)
tmax_66_75 <- stack(tmax_66_75)
tmax_91_00 <- stack(tmax_91_00)
tmax_09_18 <- stack(tmax_09_18)

# Resample according to elevation raster
tmax_41_50_res <- resample(x = tmax_41_50, y = elevation, method = "bilinear")
tmax_66_75_res <- resample(x = tmax_66_75, y = elevation, method = "bilinear")
tmax_91_00_res <- resample(x = tmax_91_00, y = elevation, method = "bilinear")
tmax_09_18_res <- resample(x = tmax_09_18, y = elevation, method = "bilinear")

# Check extention and resolution
extent(tmax_41_50_res) == extent(elevation); xres(tmax_41_50_res) == xres(elevation)
extent(tmax_66_75_res) == extent(elevation); xres(tmax_66_75_res) == xres(elevation)
extent(tmax_91_00_res) == extent(elevation); xres(tmax_91_00_res) == xres(elevation)
extent(tmax_09_18_res) == extent(elevation); xres(tmax_09_18_res) == xres(elevation)

# Save rasters
for(i in 1:12){
  writeRaster(tmax_09_18_res [[i]], 
  filename = paste0("./resemble/tmax/tmax_09_18/", names(tmax_09_18_res)[i], "_res.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)

#------# 
# PREC #
#------#
prec_41_50 <- list.files(path = "./crop/prec/prec_41_50/", 
                         pattern = ".tif$", all.files = TRUE, 
                         full.names = TRUE)

prec_66_75 <- list.files(path = "./crop/prec/prec_66_75/", 
                         pattern = ".tif$", all.files = TRUE, 
                         full.names = TRUE)

prec_91_00 <- list.files(path = "./crop/prec/prec_91_00/", 
                         pattern = ".tif$", all.files = TRUE, 
                         full.names = TRUE)

prec_09_18 <- list.files(path = "./crop/prec/prec_09_18/", 
                         pattern = ".tif$", all.files = TRUE, 
                         full.names = TRUE)

prec_41_50 <- stack(prec_41_50)
prec_66_75 <- stack(prec_66_75)
prec_91_00 <- stack(prec_91_00)
prec_09_18 <- stack(prec_09_18)

# Resample according to elevation raster
prec_41_50_res <- resample(x = prec_41_50, y = elevation, method = "bilinear")
prec_66_75_res <- resample(x = prec_66_75, y = elevation, method = "bilinear")
prec_91_00_res <- resample(x = prec_91_00, y = elevation, method = "bilinear")
prec_09_18_res <- resample(x = prec_09_18, y = elevation, method = "bilinear")

# Check extention and resolution
extent(prec_41_50_res) == extent(elevation); xres(prec_41_50_res) == xres(elevation)
extent(prec_66_75_res) == extent(elevation); xres(prec_66_75_res) == xres(elevation)
extent(prec_91_00_res) == extent(elevation); xres(prec_91_00_res) == xres(elevation)
extent(prec_09_18_res) == extent(elevation); xres(prec_09_18_res) == xres(elevation)

# Save rasters
for(i in 1:12){
  writeRaster(prec_09_18_res [[i]], 
  filename = paste0("./resemble/prec/prec_09_18/", names(prec_09_18_res)[i], "_res.tif"), 
              format = "GTiff", overwrite = FALSE)
}; rm(i)

# Clean workspace
rm(list=ls()); gc()
