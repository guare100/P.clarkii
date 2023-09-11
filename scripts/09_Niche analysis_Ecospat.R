#######################################################################################################
# Title: Insights from the past: Invasion trajectory and niche trends of a global freshwater invader  #                                            
#                                                                                                     #
# Scripts led by: Tommaso Cancellario & Simone Guareschi                                              #
#                                                                                                     #
# Creation: 2022 - 05 - 05                                                                            #
# Last update: 2023 - 05 - 20                                                                         #
#######################################################################################################

## Code adapted from:
## Di Cola et al. 2017. ecospat: an R package to support spatial analyses and modeling of species niches and distributions. Ecography 40: 774-787
## Lustenhouwer and Parker 2022. Beyond tracking climate: niche evolution during native range expansion and its implications for novel invasions. Journal of Biogeography 49(8), 1481-1493

#-------------------------------------------------------------------------#
# COUE analysis with ecospat. overlap, stability, equivalency, similarity #
#-------------------------------------------------------------------------#

# Load libraries ----
library(ade4) #<-PCA
library(dismo)
library(ecospat) # v. 3.4
library(ggplot2)
library(raster) # <- GIS
library(rgdal) # <- GIS
library(sf) # <- GIS
library(vegan)

# Load functions
source("./funcions_plot_ecospat.R")

# Set working directory ----
# setwd("./")

# Load occurrence data ----
clean1950 <- read.csv("./1950_clean.csv")
clean1975 <- read.csv("./1975_clean.csv")
clean2000 <- read.csv("./2000_clean.csv")
clean2022 <- read.csv("./2022_clean.csv")

clean1950$y <- round(clean1950$y, digits=4); clean1950$x <- round(clean1950$x, digits=4)
clean1975$y <- round(clean1975$y, digits=4); clean1975$x <- round(clean1975$x, digits=4)
clean2000$y <- round(clean2000$y, digits=4); clean2000$x <- round(clean2000$x, digits=4)
clean2022$y <- round(clean2022$y, digits=4); clean2022$x <- round(clean2022$x, digits=4)

# 1950
# Load raster variables
variables_sub_50 <- 
  raster::stack(
    c(elevation_nopoles  = "./elevation_nopoles_checkNA.tif",
      bio15_bv_41_50  = "./bio15_bv_41_50_checkNA.tif",
      PETseasonality_env_41_50 = "./PETseasonality_env_41_50_checkNA.tif",
      bio18_bv_41_50 = "./bio18_bv_41_50_checkNA.tif",
      bio1_bv_41_50="./bio1_bv_41_50_checkNA.tif",
      embergerQ_env_41_50="./embergerQ_env_41_50_checkNA.tif",
      aridityIndexThornthwaite_env_41_50= "./aridityIndexThornthwaite_env_41_50_checkNA.tif"
    )
  )

# Convert occurrences in spatial format
clean1950.sp <- SpatialPointsDataFrame(coords = clean1950[ ,c("x", "y")], 
                                       data = clean1950, 
                                       proj4string = variables_sub_50@crs)

# Extract values and convert to dataframe
occ.raster_50 <- as.data.frame(raster::extract(variables_sub_50, clean1950.sp))

occ.raster_50$date <- rep("1950", nrow(occ.raster_50))
occ.raster_50 <- cbind(occ.raster_50, clean1950[ ,c("x", "y")])
occ.raster_50 <- occ.raster_50[complete.cases(occ.raster_50), ]

head(occ.raster_50)

# 1975
# Load raster variables
variables_sub_75 <- 
  raster::stack(
    c(elevation_nopoles  = "./elevation_nopoles_checkNA.tif",
      bio15_bv_66_75  = "./bio15_bv_66_75_checkNA.tif",
      PETseasonality_env_66_75 = "./PETseasonality_env_66_75_checkNA.tif",
      bio18_bv_66_75 = "./bio18_bv_66_75_checkNA.tif",
      bio1_bv_66_75="./bio1_bv_66_75_checkNA.tif",
      embergerQ_env_66_75="./embergerQ_env_66_75_checkNA.tif",
      aridityIndexThornthwaite_env_66_75= "./aridityIndexThornthwaite_env_66_75_checkNA.tif")
  )

# Convert occurrences in spatial format
clean1975.sp <- SpatialPointsDataFrame(coords = clean1975[ ,c("x", "y")], 
                                       data = clean1975, 
                                       proj4string = variables_sub_75@crs)

# Extract values and convert to dataframe
occ.raster_75 <- as.data.frame(raster::extract(variables_sub_75, clean1975.sp))

occ.raster_75$date <- rep("1975", nrow(occ.raster_75))
occ.raster_75 <- cbind(occ.raster_75, clean1975[ ,c("x", "y")])
occ.raster_75 <- occ.raster_75[complete.cases(occ.raster_75),]

head(occ.raster_75)

# 2000
# Load raster variables
variables_sub_00 <- 
  raster::stack(
    c(elevation_nopoles  = "./elevation_nopoles_checkNA.tif",
      bio15_bv_91_00  = "./bio15_bv_91_00_checkNA.tif",
      PETseasonality_env_91_00 = "./PETseasonality_env_91_00_checkNA.tif",
      bio18_bv_91_00 = "./bio18_bv_91_00_checkNA.tif",
      bio1_bv_91_00="./bio1_bv_91_00_checkNA.tif",
      embergerQ_env_91_00="./embergerQ_env_91_00_checkNA.tif",
      aridityIndexThornthwaite_env_91_00= "./aridityIndexThornthwaite_env_91_00_checkNA.tif"
    )
  )

# Convert occurrences in spatial format
clean2000.sp <- SpatialPointsDataFrame(coords = clean2000[ ,c("x", "y")], 
                                       data = clean2000, 
                                       proj4string = variables_sub_00@crs)

# Extract values and convert to dataframe
occ.raster_00 <- as.data.frame(raster::extract(variables_sub_00, clean2000.sp))

occ.raster_00$date <- rep("2000", nrow(occ.raster_00))
occ.raster_00 <- cbind(occ.raster_00, clean2000[ ,c("x", "y")])
occ.raster_00 <- occ.raster_00[complete.cases(occ.raster_00),]

head(occ.raster_00)


# 2022
# Load raster variables
variables_sub_22 <- 
  raster::stack(
    c(elevation_nopoles  = "/elevation_nopoles_checkNA.tif",
      bio15_bv_09_18  = "/bio15_bv_09_18_checkNA.tif",
      PETseasonality_env_09_18 = "/PETseasonality_env_09_18_checkNA.tif",
      bio18_bv_09_18 = "/bio18_bv_09_18_checkNA.tif",
      bio1_bv_09_18="/bio1_bv_09_18_checkNA.tif",
      embergerQ_env_09_18="/embergerQ_env_09_18_checkNA.tif",
      aridityIndexThornthwaite_env_09_18= "/aridityIndexThornthwaite_env_09_18_checkNA.tif"
    )
  )

# Convert occurrences in spatial format
clean2022.sp <- SpatialPointsDataFrame(coords = clean2022[ ,c("x", "y")], 
                                       data = clean2022, 
                                       proj4string = variables_sub_22@crs)

# Extract values and convert to dataframe
occ.raster_22 <- as.data.frame(raster::extract(variables_sub_22, clean2022.sp))

occ.raster_22$date <- rep("2022", nrow(occ.raster_22))
occ.raster_22 <- cbind(occ.raster_22, clean2022[ ,c("x", "y")])
occ.raster_22 <- occ.raster_22[complete.cases(occ.raster_22),]

head(occ.raster_22)

# Add column with presence
occ.raster_50$presence <- 1
occ.raster_75$presence <- 1
occ.raster_00$presence <- 1
occ.raster_22$presence <- 1

# Back ground points
# 1950+random
set.seed(1234)
bpoints_50 <- randomPoints(variables_sub_50[[1]], 10000, p = occ.raster_50[,9:10] , excludep = TRUE)
bpoints_50.df <- as.data.frame(bpoints_50)

# Convert background in spatial format
bpoints_50.spt <- SpatialPointsDataFrame(coords = bpoints_50.df[ ,c("x", "y")], 
                                         data = bpoints_50.df, 
                                         proj4string = variables_sub_50@crs)

# Extract values and convert to dataframe
bpoints_50.variables <- data.frame(extract(variables_sub_50, bpoints_50.spt))
bpoints_50.variables$date <- 1950
bpoints_50.variables <- cbind(bpoints_50.variables, bpoints_50.df)
bpoints_50.variables$presence <- 0

head(bpoints_50.variables)

occ50andbpoints <- rbind(occ.raster_50, bpoints_50.variables)

# Change column names
colnames(occ50andbpoints)<-c("elevation","bio15", "PETseasonality", "bio18", "bio01", "embergerQ", "aridityIndex", "date", "x", "y", "presence")

# 1975+random
set.seed(1255)
bpoints_75 <- randomPoints(variables_sub_75[[1]], 10000, p = occ.raster_75[,9:10] , excludep = TRUE)
bpoints_75.df <- as.data.frame(bpoints_75)

# Convert background in spatial format
bpoints_75.spt <- SpatialPointsDataFrame(coords = bpoints_75.df[ ,c("x", "y")], 
                                         data = bpoints_75.df, 
                                         proj4string = variables_sub_75@crs)

# Extract values and convert to dataframe
bpoints_75.variables <- data.frame(extract(variables_sub_75, bpoints_75.spt))
bpoints_75.variables$date <- 1975
bpoints_75.variables <- cbind(bpoints_75.variables, bpoints_75.df)
bpoints_75.variables$presence <- 0

head(bpoints_75.variables)

occ75andbpoints <- rbind(occ.raster_75, bpoints_75.variables)

# Change column names
colnames(occ75andbpoints) <- colnames(occ50andbpoints)


# 2000+random
set.seed(1266)
bpoints_00 <- randomPoints(variables_sub_00[[1]], 10000, p = occ.raster_00[,9:10] , excludep = TRUE)
bpoints_00.df <- as.data.frame(bpoints_00)

# Convert background in spatial format
bpoints_00.spt <- SpatialPointsDataFrame(coords = bpoints_00.df[ ,c("x", "y")], 
                                         data = bpoints_00.df, 
                                         proj4string = variables_sub_00@crs)

# Extract values and convert to dataframe
bpoints_00.variables <- data.frame(extract(variables_sub_00, bpoints_00.spt))
bpoints_00.variables$date <- 2000
bpoints_00.variables <- cbind(bpoints_00.variables, bpoints_00.df)
bpoints_00.variables$presence <- 0

head(bpoints_00.variables)

occ00andbpoints <- rbind(occ.raster_00,bpoints_00.variables)

# Change column names
colnames(occ00andbpoints)<-colnames(occ50andbpoints)

# 2022+random
set.seed(1277)
bpoints_22 <- randomPoints(variables_sub_22[[1]], 10000, p = occ.raster_22[,9:10] , excludep = TRUE)
bpoints_22.df <- as.data.frame(bpoints_22)

# Convert background in spatial format
bpoints_22.spt <- SpatialPointsDataFrame(coords = bpoints_22.df[ ,c("x", "y")], 
                                         data = bpoints_22.df, 
                                         proj4string = variables_sub_22@crs)

# Extract values and convert to dataframe
bpoints_22.variables <- data.frame(extract(variables_sub_22, bpoints_22.spt))
bpoints_22.variables$date <- 2022
bpoints_22.variables <- cbind(bpoints_22.variables, bpoints_22.df)
bpoints_22.variables$presence <- 0

head(bpoints_22.variables)

occ22andbpoints <- rbind(occ.raster_22,bpoints_22.variables)

# Change column names
colnames(occ22andbpoints)<-colnames(occ50andbpoints)


# ECOSPAT 3.4

# 1950-1975

# PCA
pca.env.50_75 <- dudi.pca(rbind(occ50andbpoints, occ75andbpoints)[ , 1:7], scannf = FALSE, nf = 2)
ecospat.plot.contrib(contrib = pca.env.50_75$co, eigen = pca.env.50_75$eig)

scores.globclim.50_75 <- pca.env.50_75$li # PCA scores for the whole study area 

scores.sp.50<- suprow(pca.env.50_75, occ50andbpoints[which(occ50andbpoints[ , "presence"] == 1), 1:7])$li # PCA scores for the 50 
head(scores.sp.50)

scores.sp.75 <- suprow(pca.env.50_75, occ75andbpoints[which(occ75andbpoints[ , "presence"] == 1), 1:7])$li # PCA scores for the 75 
head(scores.sp.75)

scores.clim.50 <- suprow(pca.env.50_75, occ50andbpoints[ , 1:7])$li # PCA scores for the whole 50 study area 
scores.clim.75 <- suprow(pca.env.50_75, occ75andbpoints[ , 1:7])$li # PCA scores for the whole 75 study area

grid.clim.50 <- ecospat.grid.clim.dyn(glob = scores.globclim.50_75,
                                      glob1 = scores.clim.50, 
                                      sp = scores.sp.50, R = 200, 
                                      th.sp = 0)

grid.clim.75 <- ecospat.grid.clim.dyn(glob = scores.globclim.50_75,
                                      glob1 = scores.clim.75, 
                                      sp = scores.sp.75, R = 200, 
                                      th.sp = 0)

D.pastVpresent <- ecospat.niche.overlap(grid.clim.50, grid.clim.75, cor=TRUE)$D
D.pastVpresent

#set.seed(3333)
eq.test.50v75 <- ecospat.niche.equivalency.test(grid.clim.50, grid.clim.75,
                                                rep = 500, overlap.alternative = "higher", 
                                                ncores = 8)

# Equivalency test
ecospat.plot.overlap.test(eq.test.50v75, "D", "Equivalency")
eq.test.50v75$p.D

# Note that in the case of invasive species, we fixed the native niche as a reference and shifted only the invasive niche (rand.type = 2). 
# In cases where there are no assumption about a reference niche (e.g. sister species), both niches can be simultaneously shifted (rand.type = 1).
# shifting both niches (rand.type=1) Lustenhouwer 2022 in comparing time periods (past vs present) in native range used 1.

sim.test.50v75 <- ecospat.niche.similarity.test(grid.clim.50, grid.clim.75,
                                                rep = 500, overlap.alternative = "higher")

# Similarity test
ecospat.plot.overlap.test(sim.test.50v75 , "D", "Similarity")

# H0 = niche similarity (niches are as similar to each other as expected by chance)
# To test for niche conservatism, use the alternative 'greater', i.e. the niche overlap is more similar than random expectations.

# Visualizing niche categories, niche dynamics and climate analogy between time periods
cutoff <- 0.1

myplot(grid.clim.50, grid.clim.75, quant = cutoff, 
       interest = 1, # interest=1 plots past, interest=2 plots present density
       title = "Niche Overlap 1950_1975", 
       name.axis1 = "PC1", name.axis2 = "PC2",
       colz1 = "#eab98b", colz2 = "#9adef9", colinter = "#5f1988",
       colZ1 = "#e29f60", colZ2 = "#52c6f5")

my.centroid(scores.sp.50, scores.sp.75, scores.clim.50, scores.clim.75, 
            col.sp="white", col.clim="black")

ecospat.niche.dyn.index (grid.clim.50, grid.clim.75,
                         intersection = 0.1)

# Directly the metrics:
ecospat.niche.dyn.index (grid.clim.50, grid.clim.75,
                         intersection = 0.1)$dynamic.index.w

# 1950-2000

# PCA
pca.env.50_00 <- dudi.pca(rbind(occ50andbpoints, occ00andbpoints)[ , 1:7], scannf = FALSE, nf = 2)
ecospat.plot.contrib(contrib = pca.env.50_00$co, eigen = pca.env.50_00$eig)

scores.globclim.50_00 <- pca.env.50_00$li # PCA scores for the whole study area 

scores.sp.50<- suprow(pca.env.50_00 , occ50andbpoints[which(occ50andbpoints[ , "presence"] == 1), 1:7])$li # PCA scores for the 50 
head(scores.sp.50)

scores.sp.00 <- suprow(pca.env.50_00, occ00andbpoints[which(occ00andbpoints[ , "presence"] == 1), 1:7])$li # PCA scores for the 00 
head(scores.sp.00)

scores.clim.50 <- suprow(pca.env.50_00, occ50andbpoints[,1:7])$li # PCA scores for the whole 50 study area 
scores.clim.00 <- suprow(pca.env.50_00, occ00andbpoints[,1:7])$li # PCA scores for the whole 00 study area

grid.clim.50 <- ecospat.grid.clim.dyn(glob=scores.globclim.50_00,
                                      glob1=scores.clim.50, sp=scores.sp.50, R=200, th.sp=0)

grid.clim.00 <- ecospat.grid.clim.dyn(glob=scores.globclim.50_00,
                                      glob1=scores.clim.00, sp=scores.sp.00, R=200, th.sp=0)

D.pastVpresent <- ecospat.niche.overlap(grid.clim.50, grid.clim.00, cor=TRUE)$D
D.pastVpresent

eq.test.50v00 <- ecospat.niche.equivalency.test(grid.clim.50, grid.clim.00,
                                                rep=500, overlap.alternative = "higher", ncores = 8) 

# Equivalency test
ecospat.plot.overlap.test(eq.test.50v00, "D", "Equivalency")
eq.test.50v00$p.D

sim.test.50v00 <- ecospat.niche.similarity.test(grid.clim.50, grid.clim.00,
                                                rep = 500, overlap.alternative = "higher")

# Similarity test
ecospat.plot.overlap.test(sim.test.50v00 , "D", "Similarity")

# Visualizing niche categories, niche dynamics and climate analogy between time periods
cutoff <- 0.1

myplot(grid.clim.50, grid.clim.00, quant=cutoff, 
       interest = 1, 
       title = "Niche Overlap 1950_2000", 
       name.axis1 = "PC1", name.axis2 = "PC2",
       colz1 = "#eab98b", colz2 = "#9adef9", colinter = "#5f1988",
       colZ1 = "#e29f60", colZ2 = "#52c6f5")

my.centroid(scores.sp.50, scores.sp.00, scores.clim.50, scores.clim.00, 
            col.sp = "white", col.clim = "black")

ecospat.niche.dyn.index (grid.clim.50, grid.clim.00,
                         intersection = 0.1)

# 1950-2022

#PCA

pca.env.50_22 <- dudi.pca(rbind(occ50andbpoints, occ22andbpoints)[ , 1:7], scannf = FALSE, nf = 2)

# Adjust plot orienttion
scores.globclim.50_22 <- pca.env.50_22$li # PCA scores for the whole study area 
scores.globclim.50_22$Axis2 <- -1*scores.globclim.50_22$Axis2
pca.env.50_22$li <- scores.globclim.50_22

contrib<-pca.env.50_22$co
contrib$Comp2 <- -1*contrib$Comp2
pca.env.50_22$co <- contrib

contribc1<-pca.env.50_22$c1
contribc1$CS2 <- -1*contribc1$CS2
pca.env.50_22$c1 <- contribc1
  
ecospat.plot.contrib(contrib = contrib, eigen = pca.env.50_22$eig)

scores.sp.50<- suprow(pca.env.50_22 , occ50andbpoints[which(occ50andbpoints[ , "presence"] == 1), 1:7])$li # PCA scores for the 50 
head(scores.sp.50)

scores.sp.22 <- suprow(pca.env.50_22, occ22andbpoints[which(occ22andbpoints[ , "presence"] == 1), 1:7])$li # PCA scores for the 22 
head(scores.sp.22)

scores.clim.50 <- suprow(pca.env.50_22, occ50andbpoints[ , 1:7])$li # PCA scores for the whole 50 study area 
scores.clim.22 <- suprow(pca.env.50_22, occ22andbpoints[ , 1:7])$li # PCA scores for the whole 22 study area

grid.clim.50 <- ecospat.grid.clim.dyn(glob = scores.globclim.50_22,
                                      glob1 = scores.clim.50, 
                                      sp = scores.sp.50, R = 200, th.sp = 0)

grid.clim.22 <- ecospat.grid.clim.dyn(glob = scores.globclim.50_22,
                                      glob1 = scores.clim.22, 
                                      sp = scores.sp.22, R = 200, th.sp = 0)

D.pastVpresent <- ecospat.niche.overlap(grid.clim.50, grid.clim.22, cor = TRUE)$D
D.pastVpresent

eq.test.50v22 <- ecospat.niche.equivalency.test(grid.clim.50, grid.clim.22,
                                                rep = 500, overlap.alternative = "higher", 
                                                ncores = 8) 

# Equivalency test
ecospat.plot.overlap.test(eq.test.50v22, "D", "Equivalency")
eq.test.50v22$p.D

sim.test.50v22 <- ecospat.niche.similarity.test(grid.clim.50, grid.clim.22,
                                                rep = 500, overlap.alternative = "higher")

# Similarity test
ecospat.plot.overlap.test(sim.test.50v22 , "D", "Similarity")

# Visualizing niche categories, niche dynamics and climate analogy between time periods
cutoff <- 0.1

myplot(grid.clim.50, grid.clim.22, quant = cutoff, 
       interest = 1,
       title = "Niche Overlap 1950_2022", 
       name.axis1 = "PC1", name.axis2 = "PC2",
       colz1 = "#eab98b", colz2 = "#9adef9", colinter = "#5f1988",
       colZ1 = "#e29f60", colZ2 = "#52c6f5")

my.centroid(scores.sp.50, scores.sp.22, scores.clim.50, scores.clim.22, 
            col.sp = "white", col.clim = "black") 

ecospat.niche.dyn.index (grid.clim.50, grid.clim.22,
                         intersection = 0.1)



# 1975_2000 

# PCA

pca.env.75_00 <- dudi.pca(rbind(occ75andbpoints, occ00andbpoints)[ , 1:7], scannf = FALSE, nf = 2)
ecospat.plot.contrib(contrib = pca.env.75_00$co, eigen = pca.env.75_00$eig)

scores.globclim.75_00 <- pca.env.75_00$li # PCA scores for the whole study area 

scores.sp.75<- suprow(pca.env.75_00 , occ75andbpoints[which(occ75andbpoints[ , "presence"] == 1), 1:7])$li # PCA scores for the 75 
head(scores.sp.75)

scores.sp.00 <- suprow(pca.env.75_00, occ00andbpoints[which(occ00andbpoints[ , "presence"] == 1), 1:7])$li # PCA scores for the 00 
head(scores.sp.00)

scores.clim.75 <- suprow(pca.env.75_00, occ75andbpoints[ , 1:7])$li # PCA scores for the whole 75 study area 
scores.clim.00 <- suprow(pca.env.75_00, occ00andbpoints[ , 1:7])$li # PCA scores for the whole 00 study area

grid.clim.75 <- ecospat.grid.clim.dyn(glob = scores.globclim.75_00,
                                      glob1 = scores.clim.75, 
                                      sp=scores.sp.75, R = 200, th.sp = 0)

grid.clim.00 <- ecospat.grid.clim.dyn(glob = scores.globclim.75_00,
                                      glob1 = scores.clim.00, 
                                      sp = scores.sp.00, R = 200, th.sp = 0)

D.pastVpresent <- ecospat.niche.overlap(grid.clim.75, grid.clim.00, cor = TRUE)$D
D.pastVpresent

eq.test.75v00 <- ecospat.niche.equivalency.test(grid.clim.75, grid.clim.00,
                                                rep = 500, overlap.alternative = "higher", 
                                                ncores = 8) 

# Equivalency test
ecospat.plot.overlap.test(eq.test.75v00, "D", "Equivalency")
eq.test.75v00$p.D


sim.test.75v00 <- ecospat.niche.similarity.test(grid.clim.75, grid.clim.00,
                                                rep = 500, overlap.alternative = "higher")

# Similarity test
ecospat.plot.overlap.test(sim.test.75v00 , "D", "Similarity")

cutoff <- 0.1

myplot(grid.clim.75, grid.clim.00, quant = cutoff, 
       interest = 1,
       title = "Niche Overlap 1975_2000", 
       name.axis1 = "PC1", name.axis2 = "PC2",
       colz1 = "#eab98b", colz2 = "#9adef9", colinter = "#5f1988",
       colZ1 = "#e29f60", colZ2 = "#52c6f5")

my.centroid(scores.sp.75, scores.sp.00, scores.clim.75, scores.clim.00, 
            col.sp="white", col.clim="black")

ecospat.niche.dyn.index (grid.clim.75, grid.clim.00,
                         intersection = 0.1)


# 1975_2022

# PCA

pca.env.75_22 <- dudi.pca(rbind(occ75andbpoints, occ22andbpoints)[ , 1:7], scannf = FALSE, nf = 2)
ecospat.plot.contrib(contrib = pca.env.75_22$co, eigen = pca.env.75_22$eig)

scores.globclim.75_22 <- pca.env.75_22$li # PCA scores for the whole study area 

scores.sp.75 <- suprow(pca.env.75_22 , occ75andbpoints[which(occ75andbpoints[ , "presence"] == 1), 1:7])$li # PCA scores for the 75 
head(scores.sp.75)

scores.sp.22 <- suprow(pca.env.75_22, occ22andbpoints[which(occ22andbpoints[ , "presence"] == 1), 1:7])$li # PCA scores for the 22 
head(scores.sp.22)

scores.clim.75 <- suprow(pca.env.75_22, occ75andbpoints[ , 1:7])$li # PCA scores for the whole 75 study area 
scores.clim.22 <- suprow(pca.env.75_22, occ22andbpoints[ , 1:7])$li # PCA scores for the whole 22 study area

grid.clim.75 <- ecospat.grid.clim.dyn(glob = scores.globclim.75_22,
                                      glob1 = scores.clim.75, 
                                      sp = scores.sp.75, R = 200, th.sp = 0)

grid.clim.22 <- ecospat.grid.clim.dyn(glob = scores.globclim.75_22,
                                      glob1 = scores.clim.22, 
                                      sp = scores.sp.22, R = 200, th.sp = 0)

D.pastVpresent <- ecospat.niche.overlap(grid.clim.75, grid.clim.22, cor = TRUE)$D
D.pastVpresent

eq.test.75v22 <- ecospat.niche.equivalency.test(grid.clim.75, grid.clim.22,
                                                rep = 500, overlap.alternative = "higher", 
                                                ncores = 8) 

# Equivalency test
ecospat.plot.overlap.test(eq.test.75v22, "D", "Equivalency")
eq.test.75v22$p.D

sim.test.75v22 <- ecospat.niche.similarity.test(grid.clim.75, grid.clim.22,
                                                rep = 500, overlap.alternative = "higher")

# Similarity test
ecospat.plot.overlap.test(sim.test.75v22 , "D", "Similarity")

# Visualizing niche categories, niche dynamics and climate analogy between time periods
cutoff <- 0.1

myplot(grid.clim.75, grid.clim.22, quant = cutoff, 
       interest = 1,
       title = "Niche Overlap 1975_2022", 
       name.axis1 = "PC1", name.axis2 = "PC2",
       colz1 = "#eab98b", colz2 = "#9adef9", colinter = "#5f1988",
       colZ1 = "#e29f60", colZ2 = "#52c6f5")

my.centroid(scores.sp.75, scores.sp.22, scores.clim.75, scores.clim.22, 
            col.sp = "white", col.clim = "black")

ecospat.niche.dyn.index (grid.clim.75, grid.clim.22,
                         intersection=0.1)


# 2000-2022

# PCA

pca.env.00_22 <- dudi.pca(rbind(occ00andbpoints, occ22andbpoints)[ , 1:7], scannf = FALSE, nf = 2)
scores.globclim.00_22 <- pca.env.00_22$li # PCA scores for the whole study area 

# Adjust plot orienttion
scores.globclim.00_22$Axis2 <- -1*scores.globclim.00_22$Axis2
scores.globclim.00_22$Axis1 <- -1*scores.globclim.00_22$Axis1
pca.env.00_22$li <- scores.globclim.00_22

contrib <- pca.env.00_22$co
contrib$Comp2 <- -1*contrib$Comp2
contrib$Comp1 <- -1*contrib$Comp1
pca.env.00_22$co <- contrib

contribc1 <- pca.env.00_22$c1
contribc1$CS2 <- -1*contribc1$CS2
contribc1$CS1 <- -1*contribc1$CS1
pca.env.00_22$c1 <- contribc1

ecospat.plot.contrib(contrib = contrib, eigen = pca.env.00_22$eig)


scores.sp.00<- suprow(pca.env.00_22 , occ00andbpoints[which(occ00andbpoints[ , "presence"] == 1), 1:7])$li # PCA scores for the 00 
head(scores.sp.00)

scores.sp.22 <- suprow(pca.env.00_22, occ22andbpoints[which(occ22andbpoints[ , "presence"] == 1), 1:7])$li # PCA scores for the 22 
head(scores.sp.22)

scores.clim.00 <- suprow(pca.env.00_22, occ00andbpoints[ , 1:7])$li # PCA scores for the whole 00 study area 
scores.clim.22 <- suprow(pca.env.00_22, occ22andbpoints[ , 1:7])$li # PCA scores for the whole 22 study area

grid.clim.00 <- ecospat.grid.clim.dyn(glob = scores.globclim.00_22,
                                      glob1 = scores.clim.00, 
                                      sp = scores.sp.00, R = 200, th.sp = 0)

grid.clim.22 <- ecospat.grid.clim.dyn(glob = scores.globclim.00_22,
                                      glob1 = scores.clim.22, 
                                      sp = scores.sp.22, R = 200, th.sp = 0)

D.pastVpresent <- ecospat.niche.overlap(grid.clim.00, grid.clim.22, cor = TRUE)$D
D.pastVpresent

eq.test.00v22 <- ecospat.niche.equivalency.test(grid.clim.00, grid.clim.22,
                                                rep = 500, overlap.alternative = "higher", 
                                                ncores = 8) 

# Equivalency test
ecospat.plot.overlap.test(eq.test.00v22, "D", "Equivalency")
eq.test.00v22$p.D


sim.test.00v22 <- ecospat.niche.similarity.test(grid.clim.00, grid.clim.22,
                                                rep = 500, overlap.alternative = "higher")

# Similarity test
ecospat.plot.overlap.test(sim.test.00v22 , "D", "Similarity")


# Visualizing niche categories, niche dynamics and climate analogy between time periods. 
cutoff <- 0.1

myplot(grid.clim.00, grid.clim.22, quant = cutoff, 
       interest = 1,
       title = "Niche Overlap 2000_2022", 
       name.axis1 = "PC1", name.axis2 = "PC2",
       colz1 = "#eab98b", colz2 = "#9adef9", colinter = "#5f1988",
       colZ1 = "#e29f60", colZ2 = "#52c6f5")

my.centroid(scores.sp.00, scores.sp.22, scores.clim.00, scores.clim.22, 
            col.sp="white", col.clim="black")

ecospat.niche.dyn.index (grid.clim.00, grid.clim.22,
                         intersection = 0.1)
