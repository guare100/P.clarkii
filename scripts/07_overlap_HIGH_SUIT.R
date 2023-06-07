####################################################################################################
# Title: Insights from the past: Invasion trajectory and niche trends of a global aquatic invader  #                                            #
#                                                                                                  #
# Scripts led by: Tommaso Cancellario & Simone Guareschi                                           #
#                                                                                                  #
# Creation: 2022 - 05 - 05                                                                         #
# Last update: 2023 - 05 - 20                                                                      #
####################################################################################################

#---------------------------#
# Overlap high suitability  #
#---------------------------#

# Load libraries ----
library(raster)
library(ggplot2)

# Set working directory ----
# setwd("./")


# Download raster maps from OSF
# https://osf.io/3nhxs/?view_only=504e72f76b7d489aa488bddefdac21f1


# Period: 1950 - 2022
# HS >= 500 (Suitability)  (overlap 27.2%)

over.50_22 <- raster("./overlap50_22_500.tif")
plot(over.50_22)

# Convert raster to dataframe
over.50_22.df <- as.data.frame(over.50_22, xy = TRUE)
head(over.50_22.df)
unique(over.50_22.df$overlap50_22)

# Remove NA
over.50_22.df <- over.50_22.df[complete.cases(over.50_22.df), ]
unique(over.50_22.df$overlap50_22)

length(which(over.50_22.df$overlap50_22 == 1)) # <- one scenario
length(which(over.50_22.df$overlap50_22 == 2)) # <- overlap

# Total suitability
length(which(over.50_22.df$overlap50_22 == 1)) + length(which(over.50_22.df$overlap50_22 == 2)) 

# % overlap of tot suitability [52434 : 100 = 14652 : x]
# % overlap of tot suitability [47321 : 100 = 12867 : x] results 27.2
(length(which(over.50_22.df$overlap50_22 == 2)) * 100)/(length(which(over.50_22.df$overlap50_22 == 1)) + 
                                                          length(which(over.50_22.df$overlap50_22 == 2)))


# Period: 1975-2022
# HS >= 500 (Suitability) (overlap 37.5%)

over.75_22 <- raster("./overlap75_22_500.tif")
plot(over.75_22)

# Convert raster to dataframe
over.75_22.df <- as.data.frame(over.75_22, xy=TRUE)
head(over.75_22.df)
unique(over.75_22.df$overlap75_22)

# Remove NA
over.75_22.df <- over.75_22.df[complete.cases(over.75_22.df), ]
unique(over.75_22.df$overlap75_22)

length(which(over.75_22.df$overlap75_22 == 1)) # <- one scenario
length(which(over.75_22.df$overlap75_22 == 2)) # <- overlap

# Total suitability
length(which(over.75_22.df$overlap75_22 == 1)) + length(which(over.75_22.df$overlap75_22 == 2)) 

# % overlap of tot suitability [53394 : 100 = 23038 : x]
# % overlap of tot suitability [47788 : 100 = 17925 : x]
(length(which(over.75_22.df$overlap75_22 == 2)) * 100)/(length(which(over.75_22.df$overlap75_22 == 1)) + 
                                                          length(which(over.75_22.df$overlap75_22 == 2)))


# Period: 2000-2022
# HS >= 500 (Suitability) (overlap 72.7%)

over.00_22 <- raster("./overlap00_22_500.tif")
plot(over.00_22)

# Convert raster to dataframe
over.00_22.df <- as.data.frame(over.00_22, xy=TRUE)
head(over.00_22.df)
unique(over.00_22.df$overlap00_22)

# Remove NA
over.00_22.df <- over.00_22.df[complete.cases(over.00_22.df), ]
unique(over.00_22.df$overlap00_22)

length(which(over.00_22.df$overlap00_22 == 1)) # <- one scenario
length(which(over.00_22.df$overlap00_22 == 2)) # <- overlap

# Total suitability
length(which(over.00_22.df$overlap00_22 == 1)) + length(which(over.00_22.df$overlap00_22 == 2)) 

# % overlap of tot suitability [56374 : 100 = 42848 : x]
# % overlap of tot suitability [57588 : 100 = 34595 : x]
(length(which(over.00_22.df$overlap00_22 == 2)) * 100)/(length(which(over.00_22.df$overlap00_22 == 1)) + 
                                                          length(which(over.00_22.df$overlap00_22 == 2)))
