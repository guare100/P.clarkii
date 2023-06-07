####################################################################################################
# Title: Insights from the past: Invasion trajectory and niche trends of a global aquatic invader  #                                            #
#                                                                                                  #
# Scripts led by: Tommaso Cancellario & Simone Guareschi                                           #
#                                                                                                  #
# Creation: 2022 - 05 - 05                                                                         #
# Last update: 2023 - 05 - 20                                                                      #
####################################################################################################

#---------------------#
# Uniform raster maps #
#---------------------#

# Load libraries ----
library(raster)

# Set working directory ----
# setwd("./")

# Load selected raster maps
variables_sub_50 <-   stack(
    c(elevation_nopoles  = "./variables_model/elevation_nopoles.tif",
      bio15_bv_41_50  = "./variables_model/41_50/bio15_bv_41_50.tif",
      PETseasonality_env_41_50 = "./variables_model/41_50/PETseasonality_env_41_50.tif",
      bio18_bv_41_50 = "./variables_model/41_50/bio18_bv_41_50.tif",
      bio1_bv_41_50="./variables_model/41_50/bio1_bv_41_50.tif",
      embergerQ_env_41_50="./variables_model/41_50/embergerQ_env_41_50.tif",
      aridityIndexThornthwaite_env_41_50= "./variables_model/41_50/aridityIndexThornthwaite_env_41_50.tif"
    )
  )

variables_sub_75 <- stack(
    c(elevation_nopoles  = "./variables_model/elevation_nopoles.tif",
      bio15_bv_66_75  = "./variables_model/66_75/bio15_bv_66_75.tif",
      PETseasonality_env_66_75 = "./variables_model/66_75/PETseasonality_env_66_75.tif",
      bio18_bv_66_75 = "./variables_model/66_75/bio18_bv_66_75.tif",
      bio1_bv_66_75="./variables_model/66_75/bio1_bv_66_75.tif",
      embergerQ_env_66_75="./variables_model/66_75/embergerQ_env_66_75.tif",
      aridityIndexThornthwaite_env_66_75= "./variables_model/66_75/aridityIndexThornthwaite_env_66_75.tif"
    )
  )

variables_sub_00 <- stack(
    c(elevation_nopoles  = "./variables_model/elevation_nopoles.tif",
      bio15_bv_91_00  = "./variables_model/91_00/bio15_bv_91_00.tif",
      PETseasonality_env_91_00 = "./variables_model/91_00/PETseasonality_env_91_00.tif",
      bio18_bv_91_00 = "./variables_model/91_00/bio18_bv_91_00.tif",
      bio1_bv_91_00="./variables_model/91_00/bio1_bv_91_00.tif",
      embergerQ_env_91_00="./variables_model/91_00/embergerQ_env_91_00.tif",
      aridityIndexThornthwaite_env_91_00= "./variables_model/91_00/aridityIndexThornthwaite_env_91_00.tif"
    )
  )

variables_sub_19 <- stack(
    c(elevation_nopoles  = "./variables_model/elevation_nopoles.tif",
      bio15_bv_09_18  = "./variables_model/09_18/bio15_bv_09_18.tif",
      PETseasonality_env_09_18 = "./variables_model/09_18/PETseasonality_env_09_18.tif",
      bio18_bv_09_18 = "./variables_model/09_18/bio18_bv_09_18.tif",
      bio1_bv_09_18="./variables_model/09_18/bio1_bv_09_18.tif",
      embergerQ_env_09_18="./variables_model/09_18/embergerQ_env_09_18.tif",
      aridityIndexThornthwaite_env_09_18= "./variables_model/09_18/aridityIndexThornthwaite_env_09_18.tif"
    )
  )

layer_NA <- variables_sub_50*variables_sub_75*variables_sub_00*variables_sub_19

layer_NA.1<-layer_NA[[1]]*layer_NA[[2]]*layer_NA[[3]]*layer_NA[[4]]*layer_NA[[5]]*layer_NA[[6]]*layer_NA[[7]]

# writeRaster(layer_NA.1, filename = paste0("./raster_variables/variables_model/variables_def_checkNA/", "layer_NA.tif"), 
#            format = "GTiff", overwrite = FALSE)

# Mask raster files
variables_sub_50.mask <- mask(variables_sub_50, layer_NA.1)
variables_sub_75.mask <- mask(variables_sub_75, layer_NA.1)
variables_sub_00.mask <- mask(variables_sub_00, layer_NA.1)
variables_sub_19.mask <- mask(variables_sub_19, layer_NA.1)

# Save rasters
for(i in 1:length(names(variables_sub_50.mask ))){
    writeRaster(variables_sub_50.mask  [[i]], 
    filename = paste0("./variables_model/variables_def_checkNA/41_50/", names(variables_sub_50.mask )[i], "_checkNA.tif"), 
              format = "GTiff", overwrite = TRUE)
}; rm(i)

for(i in 1:length(names(variables_sub_75.mask ))){
  writeRaster(variables_sub_75.mask  [[i]], 
  filename = paste0("./variables_model/variables_def_checkNA/66_75/", names(variables_sub_75.mask )[i], "_checkNA.tif"), 
              format = "GTiff", overwrite = TRUE)
}; rm(i)

for(i in 1:length(names(variables_sub_00.mask ))){
  writeRaster(variables_sub_00.mask  [[i]], 
  filename = paste0("./variables_model/variables_def_checkNA/91_00/", names(variables_sub_00.mask )[i], "_checkNA.tif"), 
              format = "GTiff", overwrite = TRUE)
}; rm(i)

for(i in 1:length(names(variables_sub_19.mask ))){
  writeRaster(variables_sub_19.mask  [[i]], 
  filename = paste0("./variables_model/variables_def_checkNA/09_18/", names(variables_sub_19.mask )[i], "_checkNA.tif"), 
              format = "GTiff", overwrite = TRUE)
}; rm(i)


# Check map dimensions
length(variables_sub_50.mask[[2]][variables_sub_50.mask[[2]] >= 0])
length(variables_sub_75.mask[[2]][variables_sub_75.mask[[2]] >= 0])
length(variables_sub_00.mask[[2]][variables_sub_00.mask[[2]] >= 0])
length(variables_sub_19.mask[[2]][variables_sub_19.mask[[2]] >= 0])

# Clean workspace
rm(list=ls()); gc()
