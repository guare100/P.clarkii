##################################################
# Title: XXX                                     #
# Scripts led by:                                #
# Tommaso Cancellario & Simone Guareschi         #
#                                                #
# Creation: 2022 - 05 - 05                       #
# Last update: 2023 - 05 - 20                    #
##################################################

#--------------------------------------------#
# Bioclimatic and Envirem variables          #
#--------------------------------------------#

# Load libraries ----
library(dismo) # <- Bioclimatic variables
library(envirem) # <- Envirem variables
library(sf) # <- GIS
library(raster) # <- GIS
library(rgdal) # <- GIS

# Set working directory ----
# setwd("./")

# Create Biovar and Envirem variables ----

###############
# 1941 - 1950 #
###############
tmin <- list.files(path = "./resemble/tmin/tmin_41_50/", 
                   pattern = ".tif$", all.files = TRUE, 
                   full.names = TRUE)

tmax <- list.files(path = "./resemble/tmax/tmax_41_50/", 
                   pattern = ".tif$", all.files = TRUE, 
                   full.names = TRUE)

prec <- list.files(path = "./resemble/prec/prec_41_50/", 
                   pattern = ".tif$", all.files = TRUE, 
                   full.names = TRUE)

tmin <- stack(tmin)
tmax <- stack(tmax)
prec <- stack(prec)

# Check extention and resolution
extent(tmin) == extent(tmax); xres(tmin) == xres(tmax)
extent(tmin) == extent(prec); xres(tmin) == xres(prec)

# Biovariables Creation ----
bioVar <- biovars(prec = prec, tmin = tmin, tmax = tmax)

# plot(bioVar[[1]])
# extent(bioVar) == extent(elevation); xres(bioVar) == xres(elevation)

# Save rasters
for(i in 1:length(names(bioVar))){
  writeRaster(bioVar[[i]], 
              filename = paste0("./biovar/41_50/", names(bioVar)[i], "_bv_41_50.tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Envirem variable creation ----

# Create folder to store tmax, tmin and prec
# Copy file into the new folder
file.copy(from = list.files(path = "./resemble/tmin/tmin_41_50/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE),
          to = "./envirem/tmax_tmin_prec_41_50/")

file.copy(from = list.files(path = "./resemble/tmax/tmax_41_50/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE),
          to = "./envirem/tmax_tmin_prec_41_50/")

file.copy(from = list.files(path = "./raster_variables/resemble/prec/prec_41_50/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE),
          to = "./envirem/tmax_tmin_prec_41_50/")

# Check the variables names
verifyFileStructure("./envirem/tmax_tmin_prec_41_50/", returnFileNames = FALSE)

# Set variable name
assignNames(tmax = "tmax_##_mean_crop_res",
            tmin = "tmin_##_mean_crop_res",
            precip = "prec_##_mean_crop_res")

verifyFileStructure("./envirem/tmax_tmin_prec_41_50/", returnFileNames = FALSE)

# Estimate solar radiation from raster space
rasterTemplate <- raster("./wc2.1_10m_elev.tif") # Elevation map

# Calculate monthly solar radiation. (See Help for details)
ETsolradRasters(rasterTemplate = rasterTemplate, year = 0, 
                outputDir = "./envirem/tmax_tmin_prec_41_50/", 
                overwrite = FALSE)

# Check variable names again
verifyFileStructure("./envirem/tmax_tmin_prec_41_50/", 
returnFileNames = FALSE)

# List of climate raster
climFiles <- list.files(path = "./envirem/tmax_tmin_prec_41_50/",
                        pattern = ".tif$", all.files = TRUE, 
                        full.names = TRUE)
climFiles <- climFiles[!grepl("solrad", climFiles)]

# List of solar radiation rasters
solarFiles <- list.files(path = "./envirem/tmax_tmin_prec_41_50/", 
                         pattern = "solrad", all.files = TRUE, 
                         full.names = TRUE)

# Load climatic and solar radiation rasters
climStack <- stack(climFiles)
solarStack <- stack(solarFiles)

# Visual check
# par(mfrow=c(1, 2))
# plot(climStack[[1]])
# plot(solarStack[[1]])
# dev.off()

# Check extension and resolution
extent(climStack) == extent(solarStack); xres(climStack) == xres(solarStack)

# Remove unnecessary objects
rm(climFiles, solarFiles)

# Check names
verifyRasterNames(climStack, solradstack = solarStack)

# Envimer variables creation.
envirem.var <- layerCreation(climStack, solarStack, var = "all")

# Save rasters
for(i in 1:length(names(envirem.var))){
  writeRaster(envirem.var [[i]], 
  filename = paste0("./envirem/envirem_41_50/", names(envirem.var)[i], "_env_41_50.tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Remove folder with tmin, tmax, prec, and solar
# unlink("./.../tmax_tmin_prec/", recursive = TRUE)

###############
# 1966 - 1975 #
###############
tmin <- list.files(path = "./resemble/tmin/tmin_66_75/", 
                   pattern = ".tif$", all.files = TRUE, 
                   full.names = TRUE)

tmax <- list.files(path = "./resemble/tmax/tmax_66_75/", 
                   pattern = ".tif$", all.files = TRUE, 
                   full.names = TRUE)

prec <- list.files(path = "./resemble/prec/prec_66_75/", 
                   pattern = ".tif$", all.files = TRUE, 
                   full.names = TRUE)

tmin <- stack(tmin)
tmax <- stack(tmax)
prec <- stack(prec)

# Check extention and resolution
extent(tmin) == extent(tmax); xres(tmin) == xres(tmax)
extent(tmin) == extent(prec); xres(tmin) == xres(prec)

# Biovariables Creation ----
bioVar <- biovars(prec = prec, tmin = tmin, tmax = tmax)

# plot(bioVar[[1]])
# extent(bioVar) == extent(elevation); xres(bioVar) == xres(elevation)

# Save rasters
for(i in 1:length(names(bioVar))){
  writeRaster(bioVar[[i]], 
              filename = paste0("./biovar/66_75/", names(bioVar)[i], "_bv_66_75.tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Envirem variable creation ----

# Create folder to store tmax, tmin and prec
# Copy file into the new folder
file.copy(from = list.files(path = "./resemble/tmin/tmin_66_75/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE),
          to = "./envirem/tmax_tmin_prec_66_75/")

file.copy(from = list.files(path = "./resemble/tmax/tmax_66_75/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE),
          to = "./envirem/tmax_tmin_prec_66_75/")

file.copy(from = list.files(path = "./resemble/prec/prec_66_75/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE),
          to = "./envirem/tmax_tmin_prec_66_75/")

# Check the variables names
verifyFileStructure("./envirem/tmax_tmin_prec_66_75/", returnFileNames = FALSE)

# Set variable name
assignNames(tmax = "tmax_##_mean_crop_res",
            tmin = "tmin_##_mean_66_75_crop_res",
            precip = "prec_##_mean_66_75_crop_res")

verifyFileStructure("./envirem/tmax_tmin_prec_66_75/", returnFileNames = FALSE)

# Estimate solar radiation from raster space
rasterTemplate <- raster("./wc2.1_10m_elev.tif") # Elevation map

# Calculate monthly solar radiation. 1975 - 1950 = 25
ETsolradRasters(rasterTemplate = rasterTemplate, year = 25, 
                outputDir = "./envirem/tmax_tmin_prec_66_75/", 
                overwrite = FALSE)

# Check variable names again
verifyFileStructure("./envirem/tmax_tmin_prec_66_75/", returnFileNames = FALSE)

# List of climate raster
climFiles <- list.files(path = "./raster_variables/envirem/tmax_tmin_prec_66_75/",
                        pattern = ".tif$", all.files = TRUE, 
                        full.names = TRUE)
climFiles <- climFiles[!grepl("solrad", climFiles)]

# List of solar radiation rasters
solarFiles <- list.files(path = "./raster_variables/envirem/tmax_tmin_prec_66_75/", 
                         pattern = "solrad", all.files = TRUE, 
                         full.names = TRUE)

# Load climatic and solar radiation rasters
climStack <- stack(climFiles)
solarStack <- stack(solarFiles)

# Visual check
# par(mfrow=c(1, 2))
# plot(climStack[[1]])
# plot(solarStack[[1]])
# dev.off()

# Check extension and resolution
extent(climStack) == extent(solarStack); xres(climStack) == xres(solarStack)

# Remove unnecessary objects
rm(climFiles, solarFiles)

# Check names
verifyRasterNames(climStack, solradstack = solarStack)

# Envimer variables creation.
envirem.var <- layerCreation(climStack, solarStack, var = "all")

# Save rasters
for(i in 1:length(names(envirem.var))){
  writeRaster(envirem.var [[i]], 
  filename = paste0("./envirem/envirem_66_75/", names(envirem.var)[i], "_env_66_75.tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Remove folder with tmin, tmax, prec, and solar id
# unlink("./.../tmax_tmin_prec/", recursive = TRUE)

###############
# 1991 - 2000 #
###############
tmin <- list.files(path = "./resemble/tmin/tmin_91_00/", 
                   pattern = ".tif$", all.files = TRUE, 
                   full.names = TRUE)

tmax <- list.files(path = "./resemble/tmax/tmax_91_00/", 
                   pattern = ".tif$", all.files = TRUE, 
                   full.names = TRUE)

prec <- list.files(path = "./resemble/prec/prec_91_00/", 
                   pattern = ".tif$", all.files = TRUE, 
                   full.names = TRUE)

tmin <- stack(tmin)
tmax <- stack(tmax)
prec <- stack(prec)

# Check extention and resolution
extent(tmin) == extent(tmax); xres(tmin) == xres(tmax)
extent(tmin) == extent(prec); xres(tmin) == xres(prec)

# Biovariables Creation ----
bioVar <- biovars(prec = prec, tmin = tmin, tmax = tmax)

# plot(bioVar[[1]])
# extent(bioVar) == extent(elevation); xres(bioVar) == xres(elevation)

# Save rasters
for(i in 1:length(names(bioVar))){
  writeRaster(bioVar[[i]], 
              filename = paste0("./biovar/91_00/", names(bioVar)[i], "_bv_91_00.tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Envirem variable creation ----

# Create folder to store tmax, tmin and prec
# Copy file into the new folder
file.copy(from = list.files(path = "./resemble/tmin/tmin_91_00/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE),
          to = "./envirem/tmax_tmin_prec_91_00/")

file.copy(from = list.files(path = "./resemble/tmax/tmax_91_00/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE),
          to = "./envirem/tmax_tmin_prec_91_00/")

file.copy(from = list.files(path = "./resemble/prec/prec_91_00/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE),
          to = "./envirem/tmax_tmin_prec_91_00/")

# Check variables names
verifyFileStructure("./envirem/tmax_tmin_prec_91_00/", returnFileNames = FALSE)

# Set variable name
assignNames(tmax = "tmax_##_mean_crop_res",
            tmin = "tmin_##_mean_crop_res",
            precip = "prec_##_mean_91_00_crop_res")

verifyFileStructure("./envirem/tmax_tmin_prec_91_00/", returnFileNames = FALSE)

# Estimate solar radiation from raster space
rasterTemplate <- raster("./wc2.1_10m_elev.tif") # Elevation map

## Calculate monthly solar radiation. 2000 - 1950 = 50
ETsolradRasters(rasterTemplate = rasterTemplate, year = 50, 
                outputDir = "./envirem/tmax_tmin_prec_91_00/", 
                overwrite = FALSE)

# Check variable names again
verifyFileStructure("./envirem/tmax_tmin_prec_91_00/", returnFileNames = FALSE)

# List of climate raster
climFiles <- list.files(path = "./envirem/tmax_tmin_prec_91_00/",
                        pattern =".tif$", all.files = TRUE, full.names = TRUE)
climFiles <- climFiles[!grepl("solrad", climFiles)]

# List of solar radiation rasters
solarFiles <- list.files(path = "./envirem/tmax_tmin_prec_91_00/", 
                         pattern = "solrad", all.files = TRUE, full.names = TRUE)

# Load climatic and solar radiation rasters
climStack <- stack(climFiles)
solarStack <- stack(solarFiles)

# Visual check
# par(mfrow=c(1, 2))
# plot(climStack[[1]])
# plot(solarStack[[1]])
# dev.off()

# Check extension and resolution
extent(climStack) == extent(solarStack); xres(climStack) == xres(solarStack)

# Remove unnecessary objects
rm(climFiles, solarFiles)

# Check names
verifyRasterNames(climStack, solradstack = solarStack)

# Envimer variables creation
envirem.var <- layerCreation(climStack, solarStack, var = "all")

# Save rasters
for(i in 1:length(names(envirem.var))){
  writeRaster(envirem.var [[i]], 
  filename = paste0("./envirem/envirem_91_00/", names(envirem.var)[i], "_env_91_00.tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Remove folder with tmin, tmax, prec, and solar id
# unlink("./.../tmax_tmin_prec/", recursive = TRUE)

###############
# 2009 - 2018 #
###############
tmin <- list.files(path = "./resemble/tmin/tmin_09_18/", 
                   pattern = ".tif$", all.files = TRUE, 
                   full.names = TRUE)

tmax <- list.files(path = "./resemble/tmax/tmax_09_18/", 
                   pattern = ".tif$", all.files = TRUE, 
                   full.names = TRUE)

prec <- list.files(path = "./resemble/prec/prec_09_18/", 
                   pattern = ".tif$", all.files = TRUE, 
                   full.names = TRUE)

tmin <- stack(tmin)
tmax <- stack(tmax)
prec <- stack(prec)

# Check extention and resolution
extent(tmin) == extent(tmax); xres(tmin) == xres(tmax)
extent(tmin) == extent(prec); xres(tmin) == xres(prec)


# Biovariables Creation ----
bioVar <- biovars(prec = prec, tmin = tmin, tmax = tmax)

# plot(bioVar[[1]])
# extent(bioVar) == extent(elevation); xres(bioVar) == xres(elevation)

# Save rasters
for(i in 1:length(names(bioVar))){
  writeRaster(bioVar[[i]], 
              filename = paste0("./biovar/09_18/", names(bioVar)[i], "_bv_09_18.tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Envirem variable creation ----

# Create folder to store tmax, tmin and prec
# Copy file into the new folder
file.copy(from = list.files(path = "./resemble/tmin/tmin_09_18/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE),
          to = "./envirem/tmax_tmin_prec_09_18/")

file.copy(from = list.files(path = "./resemble/tmax/tmax_09_18/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE),
          to = "./envirem/tmax_tmin_prec_09_18/")

file.copy(from = list.files(path = "./resemble/prec/prec_09_18/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE),
          to = "./envirem/tmax_tmin_prec_09_18/")

# Check variables names
verifyFileStructure("./envirem/tmax_tmin_prec_09_18/", returnFileNames = FALSE)

# Set variable name
assignNames(tmax = "tmax_##_mean_09_18_crop_res",
            tmin = "tmin_##_mean_09_18_crop_res",
            precip = "prec_##_mean_09_18_crop_res")

verifyFileStructure("./envirem/tmax_tmin_prec_09_18/", returnFileNames = FALSE)

# Estimate solar radiation from raster space
rasterTemplate <- raster("./raster_variables/wc2.1_10m_elev.tif")

# Calculate monthly solar radiation. 2018 - 1950 = 68
ETsolradRasters(rasterTemplate = rasterTemplate, year = 68, 
                outputDir = "./envirem/tmax_tmin_prec_09_18/", 
                overwrite = FALSE)

# Check variable names again
verifyFileStructure("./envirem/tmax_tmin_prec_09_18/", returnFileNames = FALSE)

# List of climate raster
climFiles <- list.files(path = "./raster_variables/envirem/tmax_tmin_prec_09_18/",
                        pattern =".tif$", all.files = TRUE, full.names = TRUE)
climFiles <- climFiles[!grepl("solrad", climFiles)]

# List of solar radiation rasters
solarFiles <- list.files(path = "./raster_variables/envirem/tmax_tmin_prec_09_18/", 
                         pattern = "solrad", all.files = TRUE, full.names = TRUE)

# Load climatic and solar radiation rasters
climStack <- stack(climFiles)
solarStack <- stack(solarFiles)

# Visual check
# par(mfrow=c(1, 2))
# plot(climStack[[1]])
# plot(solarStack[[1]])
# dev.off()

# Check extension and resolution
extent(climStack) == extent(solarStack); xres(climStack) == xres(solarStack)

# Remove unnecessary objects
rm(climFiles, solarFiles)

# Check names
verifyRasterNames(climStack, solradstack = solarStack)

# Envimer variables creation
envirem.var <- layerCreation(climStack, solarStack, var = "all")

# Create folder containing all the envirem variables

# Save rasters
for(i in 1:length(names(envirem.var))){
  writeRaster(envirem.var [[i]], 
  filename = paste0("./envirem/envirem_09_18/", names(envirem.var)[i], "_env_09_18.tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Remove folder with tmin, tmax, prec, and solar id
# unlink("./.../tmax_tmin_prec/", recursive = TRUE)

# Clean workspace
rm(list=ls()); gc()

#-----------------#
# Crop South pole #
#-----------------#

world <- st_read("./shape_files/World_NO_Lakes/World_No_Lakes.shp")
elevation <- raster("./wc2.1_10m_elev.tif")

# Crop elevation raster
elevation.sub <- mask(crop(elevation, extent(world)), world)

# Save raster
writeRaster(elevation.sub, 
filename = paste0("./variables_model/elevation_nopoles.tif"), 
            format = "GTiff", overwrite = FALSE)

# Crop biovariable rasters
biovar_41_50 <- list.files(path = "./biovar/41_50/", 
                         pattern = ".tif$", all.files = TRUE, 
                         full.names = TRUE)
biovar_66_75 <- list.files(path = "./biovar/66_75/", 
                           pattern = ".tif$", all.files = TRUE, 
                           full.names = TRUE)
biovar_91_00 <- list.files(path = "./biovar/91_00/", 
                           pattern = ".tif$", all.files = TRUE, 
                           full.names = TRUE)
biovar_09_18 <- list.files(path = "./biovar/09_18/", 
                           pattern = ".tif$", all.files = TRUE, 
                           full.names = TRUE)

biovar_41_50 <-stack(biovar_41_50)
biovar_66_75 <-stack(biovar_66_75)
biovar_91_00 <-stack(biovar_91_00)
biovar_09_18 <-stack(biovar_09_18)

biovar_41_50 <- mask(crop(biovar_41_50, extent(world)), world)
biovar_66_75 <- mask(crop(biovar_66_75, extent(world)), world)
biovar_91_00 <- mask(crop(biovar_91_00, extent(world)), world)
biovar_09_18 <- mask(crop(biovar_09_18, extent(world)), world)

# Save rasters
for(i in 1:length(names(biovar_41_50))){
  writeRaster(biovar_41_50 [[i]], 
  filename = paste0("./variables_model/41_50/", names(biovar_41_50 )[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:length(names(biovar_66_75))){
  writeRaster(biovar_66_75 [[i]], 
  filename = paste0("./variables_model/66_75/", names(biovar_66_75 )[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:length(names(biovar_91_00))){
  writeRaster(biovar_91_00 [[i]], 
  filename = paste0("./variables_model/91_00/", names(biovar_91_00 )[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:length(names(biovar_09_18))){
  writeRaster(biovar_09_18 [[i]], 
  filename = paste0("./variables_model/09_18/", names(biovar_09_18 )[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Crop Envirem rasters
envirem_41_50 <- list.files(path = "./envirem/envirem_41_50/", 
                           pattern = ".tif$", all.files = TRUE, 
                           full.names = TRUE)
envirem_66_75 <- list.files(path = "./envirem/envirem_66_75/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE)
envirem_91_00 <- list.files(path = "./envirem/envirem_91_00/", 
                            pattern = ".tif$", all.files = TRUE, 
                            full.names = TRUE)
envirem_09_18 <- list.files(path = "./envirem/envirem_09_18/", 
                            pattern = ".tif$", all.files = TRUE, full.names = TRUE)

envirem_41_50 <-stack(envirem_41_50)
envirem_66_75 <-stack(envirem_66_75)
envirem_91_00 <-stack(envirem_91_00)
envirem_09_18 <-stack(envirem_09_18)

envirem_41_50 <- mask(crop(envirem_41_50, extent(world)), world)
envirem_66_75 <- mask(crop(envirem_66_75, extent(world)), world)
envirem_91_00 <- mask(crop(envirem_91_00, extent(world)), world)
envirem_09_18 <- mask(crop(envirem_09_18, extent(world)), world)

# Save rasters
for(i in 1:length(names(envirem_41_50))){
  writeRaster(envirem_41_50 [[i]], 
  filename = paste0("./variables_model/41_50/", names(envirem_41_50 )[i], ".tif"), 
              format = "GTiff", overwrite = TRUE)}; rm(i)

for(i in 1:length(names(envirem_66_75))){
  writeRaster(envirem_66_75 [[i]], 
  filename = paste0("./variables_model/66_75/", names(envirem_66_75 )[i], ".tif"), 
              format = "GTiff", overwrite = TRUE)}; rm(i)

for(i in 1:length(names(envirem_91_00))){
  writeRaster(envirem_91_00 [[i]], 
  filename = paste0("./variables_model/91_00/", names(envirem_91_00 )[i], ".tif"), 
              format = "GTiff", overwrite = TRUE)}; rm(i)

for(i in 1:length(names(envirem_09_18))){
  writeRaster(envirem_09_18 [[i]], 
  filename = paste0("./variables_model/09_18/", names(envirem_09_18 )[i], ".tif"), 
              format = "GTiff", overwrite = TRUE)}; rm(i)

# Clean workspace
rm(list=ls()); gc()