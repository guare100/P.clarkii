##################################################
# Title: XXX                                     #
# Scripts led by:                                #
# Tommaso Cancellario & Simone Guareschi         #
#                                                #
# Creation: 2022 - 05 - 05                       #
# Last update: 2023 - 05 - 20                    #
##################################################

#----------------------#
# Correlation analysis #
#----------------------#

# Load libraries ----
library(HH) # <- Variance Inflarion Factor (VIF)
library(raster) # <- GIS

# Set working directory ----
# setwd("./")

# Load raster maps ----
elevation <- raster("./variables_model/elevation_nopoles.tif") # <- Elevation
bioenvirem <- list.files(path = "./variables_model/41_50/", 
                      pattern = ".tif$", all.files = TRUE, 
                      full.names = TRUE) # <- Biovariables and envirem

bioenvirem <- stack(bioenvirem)

# Create a total stack including all variables.
variablesStack <- stack(elevation, bioenvirem)
# names(variablesStack)

# Remove unnecessary file
rm(elevation, bioenvirem)

# Convert stack to dataframe.
variable.df <- as.data.frame(variablesStack); rm(variablesStack)

# Remove NA from dataframe
variable.df <- na.omit(variable.df)

# Correlation matrix
variable.cor <- cor(variable.df)

# Convert the correlation matrix to distance matrix
variable.dist <- as.dist(abs(variable.cor))

# Distance dendrogram
variable.cluster <- hclust(1-variable.dist)

# Correlation dendrogram (save and plot)
pdf("./Results/plots/dendrogram.pdf", width = 12, height = 12, pointsize = 20)
plot(variable.cluster, main = "Dendrogram Correlation Variables",
     sub = "Red Line correlation = 0.5; Blue line correlation = 0.7")
     abline(h = 0.5, col = "red")
     abline(h = 0.3, col = "blue")
dev.off()

# Extract selected variables.
variables.subset <- c("bio1_bv_41_50",
                      "bio2_bv_41_50",
                      "bio3_bv_41_50",
                      "bio8_bv_41_50",
                      "bio12_bv_41_50",
                      "bio15_bv_41_50",
                      "bio17_bv_41_50",
                      "bio18_bv_41_50",
                      "aridityIndexThornthwaite_env_41_50",
                      "embergerQ_env_41_50",
                      "PETseasonality_env_41_50",
                       "elevation_nopoles")

# Dataframe only with these variables
variable.select <- variable.df[,variables.subset]
colnames(variable.select)

# Check linear combination between predictor variables with VIF.
vif.res <- vif(variable.select)
sort(vif.res)

# Rimove: bio3
variable.select$bio3_bv_41_50 <- NULL
vif.res <- vif(variable.select)
sort(vif.res)

# Rimove: bio12
variable.select$bio12_bv_41_50 <- NULL
vif.res <- vif(variable.select)
sort(vif.res)

# Rimove: bio02
variable.select$bio2_bv_41_50 <- NULL
vif.res <- vif(variable.select)
sort(vif.res)

# Rimove: bio17
variable.select$bio17_bv_41_50 <- NULL
vif.res <- vif(variable.select)
sort(vif.res)

# Rimove: bio08
variable.select$bio8_bv_41_50 <- NULL
vif.res <- vif(variable.select)
sort(vif.res)

# VIF final list (used when modelling)
# elevation_nopoles: 1.147633
# bio1_bv_41_50: 2.289834
# bio15_bv_41_50: 1.532849
# bio18_bv_41_50: 2.055949
# aridityIndexThornthwaite_env_41_50: 2.818987
# embergerQ_env_41_50: 2.547754
# PETseasonality_env_41_50: 1.585526
