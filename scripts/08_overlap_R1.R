
#####################################################################################################
# Title: Insights from the past: Invasion trajectory and niche trends of a global freshwater invader#                                            #
#                                                                                                   #
# Scripts led by: Tommaso Cancellario & Simone Guareschi                                            #
#                                                                                                   #
# Creation: 2022 - 05 - 05                                                                          #
# Last update: 2023 - 09 - 10                                                                       #
#####################################################################################################

#--------------------------#
# Suitability and Overlap  #
# R1.Supplementary Mat: S6 #
#--------------------------#

######### BINARY MAPS: using a threshold depending on the function results based on TSS

# Set working directory --------------------------------------------------------

setwd("./")

# Load packages
library(raster)
library(ggplot2)


#### 1950 (raster files available on OSF. See data availability statement)

over.50_22 <- raster("./Results/binary_R1/overlap_50_22_R1.tif")

plot(over.50_22)

over.50_22.df <- as.data.frame(over.50_22, xy=TRUE)
head(over.50_22.df)

unique(over.50_22.df$overlap_50_22_R1)

# Remove NA
over.50_22.df <- over.50_22.df[complete.cases(over.50_22.df), ]
unique(over.50_22.df$overlap_50_22_R1)

length(which(over.50_22.df$overlap_50_22_R1 == 1)) # <- one scenario
length(which(over.50_22.df$overlap_50_22_R1 == 2)) # <- overlap

# Total suitability
length(which(over.50_22.df$overlap_50_22_R1 == 1)) + length(which(over.50_22.df$overlap_50_22_R1 == 2)) 


# Version R1: 78118: 100= 44746 : X. 
# Results: 57.3%

(length(which(over.50_22.df$overlap_50_22_R1 == 2)) * 100)/(length(which(over.50_22.df$overlap_50_22_R1 == 1)) + 
                                                          length(which(over.50_22.df$overlap_50_22_R1 == 2)))

#### 1975

over.75_22 <- raster("./overlap_75_22_R1.tif")

plot(over.75_22)

over.75_22.df <- as.data.frame(over.75_22, xy=TRUE)
head(over.75_22.df)

unique(over.75_22.df$overlap_75_22_R1)

# Remove NA
over.75_22.df <- over.75_22.df[complete.cases(over.75_22.df), ]
unique(over.75_22.df$overlap_75_22_R1)

length(which(over.75_22.df$overlap_75_22_R1 == 1)) # <- one scenario
length(which(over.75_22.df$overlap_75_22_R1 == 2)) # <- overlap

# Total suitability
length(which(over.75_22.df$overlap_75_22_R1 == 1)) + length(which(over.75_22.df$overlap_75_22_R1 == 2)) 

# % overlap of tot suitability 91950 : 100 = 59007 : x
# Results: 64.2%

(length(which(over.75_22.df$overlap_75_22_R1 == 2)) * 100)/(length(which(over.75_22.df$overlap_75_22_R1 == 1)) + 
                                                          length(which(over.75_22.df$overlap_75_22_R1 == 2)))

#### 2000 (third and last comparison) 

over.00_22 <- raster("./overlap_00_22_R1.tif")

plot(over.00_22)

over.00_22.df <- as.data.frame(over.00_22, xy=TRUE)
head(over.00_22.df)

unique(over.00_22.df$overlap_00_22_R1)

# Remove NA
over.00_22.df <- over.00_22.df[complete.cases(over.00_22.df), ]
unique(over.00_22.df$overlap_00_22_R1)

length(which(over.00_22.df$overlap_00_22_R1 == 1)) # <- one scenario
length(which(over.00_22.df$overlap_00_22_R1 == 2)) # <- overlap

# Total suitability
length(which(over.00_22.df$overlap_00_22_R1 == 1)) + length(which(over.00_22.df$overlap_00_22_R1 == 2)) 

# % overlap of tot suitability 94655 : 100 = 65999 : x. 
# Results: 69.7%

(length(which(over.00_22.df$overlap_00_22_R1 == 2)) * 100)/(length(which(over.00_22.df$overlap_00_22_R1 == 1)) + 
                                                          length(which(over.00_22.df$overlap_00_22_R1 == 2)))




