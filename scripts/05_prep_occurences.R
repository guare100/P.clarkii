#####################################################################################################
# Title: Insights from the past: Invasion trajectory and niche trends of a global freshwater invader#                                            
#                                                                                                   #
# Scripts led by: Tommaso Cancellario & Simone Guareschi                                            #
#                                                                                                   #
# Creation: 2022 - 05 - 05                                                                          #
# Last update: 2023 - 05 - 10                                                                       #
#####################################################################################################

#-------------------------#
# Occurrences preparation #
#-------------------------#


# Load libraries ----
library(raster) # 
library(rgeos) # 

# Set working directory ----
# setwd("./")

# Load elevation raster
elevation <- raster("./variables_model/variables_def_checkNA/solo_ELEV.tif")

# Load occurrences. 1950 ----
occur_1950 <- read.csv("./occurrences/DatabaseClarkii2022_full2_50.csv")
occur_1950 <- occur_1950[,c("Latitude", "Longitude")]
occur_1950$species <- "P.clarkii"

occur_1950.extract <- extract(x = elevation, y = occur_1950[ , c("Longitude","Latitude")])
occur_1950$elevation <- occur_1950.extract

occur_1950 <- occur_1950[!is.na(occur_1950$elevation), ]

# unique(is.na(occur_1950$elevation))

# Remove duplicates
duplic <- duplicated(occur_1950[ , c("Longitude", "Latitude")])
occur_1950 <- occur_1950[!duplic, ]

# Reduce spatial correlation: thinning procedure
res.deg <- xres(elevation)
empty.cell <- 1
minimum.dist <- res.deg * empty.cell
# minimum.dist * 111.19 # <- in km

# Reduce spatial correlation
sp.spatial.cor <- as.data.frame(occur_1950[ ,c("Longitude", "Latitude")])
colnames(sp.spatial.cor) <- c("x", "y")

xy.thin.1 <- SDMworkshop::reduceSpatialCorrelation(
  xy = sp.spatial.cor,
  variables = elevation,
  minimum.distance = minimum.dist)

sp1950 <- cbind("P.clarkii", xy.thin.1)

# plot(elevation)
# points(sp1950$x, sp1950$y, col = "red", pch = 20, cex =.3)
# points(occur_1950$Longitude, occur_1950$Latitude, pch = 20, cex = .3)
# dev.off()

# Save occurrences
write.csv(sp1950, "./occurrences/1950_clean.csv", row.names = FALSE)

# Load occurrence. 1975 ----
occur_1975 <- read.csv("./occurrences/DatabaseClarkii2022_full2_75.csv")

occur_1975.extract <- extract(x = elevation, y = occur_1975[ , c("Longitude","Latitude")])
occur_1975$elevation <- occur_1975.extract

occur_1975 <- occur_1975[!is.na(occur_1975$elevation), ]

# unique(is.na(occur_1975$elevation))

# Remove duplicates
duplic <- duplicated(occur_1975[ , c("Longitude", "Latitude")])
occur_1975 <- occur_1975[!duplic, ]

# Reduce spatial correlation: thinning procedure
sp.spatial.cor <- as.data.frame(occur_1975[ ,c("Longitude","Latitude")])
colnames(sp.spatial.cor) <- c("x", "y")

xy.thin.1 <- SDMworkshop::reduceSpatialCorrelation(
  xy = sp.spatial.cor,
  variables = elevation,
  minimum.distance = minimum.dist)

sp1975 <- cbind("P.clarkii", xy.thin.1)

# plot(elevation)
# points(sp1975$x, sp1975$y, col = "red", pch = 20, cex = .3)
# points(occur_1975$Longitude, occur_1975$Latitude, pch = 20, cex = .3)
# dev.off()

# Save occurrences
write.csv(sp1975, "./occurrences/1975_clean.csv", row.names=FALSE)

# Load occurrences. 2000 ----
occur_2000 <- read.csv("./occurrences/DatabaseClarkii2022_full2_2000.csv")

occur_2000.extract <- extract(x = elevation, y = occur_2000[ , c("Longitude","Latitude")])
occur_2000$elevation <- occur_2000.extract

occur_2000<- occur_2000[!is.na(occur_2000$elevation), ]

# unique(is.na(occur_2000$elevation))

# Remove duplicates
duplic <- duplicated(occur_2000[ , c("Longitude", "Latitude")])
occur_2000 <- occur_2000[!duplic, ]

# Reduce spatial correlation: thinning procedure
sp.spatial.cor <- as.data.frame(occur_2000[,c("Longitude","Latitude")])
colnames(sp.spatial.cor) <- c("x", "y")

xy.thin.1 <- SDMworkshop::reduceSpatialCorrelation(
  xy = sp.spatial.cor,
  variables = elevation,
  minimum.distance = minimum.dist)

sp2000 <- cbind("P.clarkii", xy.thin.1)

# plot(elevation)
# points(sp2000$x, sp2000$y, col = "red", pch = 20, cex = .3)
# points(occur_2000$Longitude, occur_2000$Latitude, pch = 20, cex = .3)
# dev.off()

# Save occurrences
write.csv(sp2000, "./occurrences/2000_clean.csv", row.names=FALSE)


# Load occurrences. 2022 ----
occur_2022<-read.csv("./occurrences/DatabaseClarkii2022_full2_2022.csv")

occur_2022.extract <- extract(x = elevation, y = occur_2022[ , c("Longitude","Latitude")])
occur_2022$elevation <- occur_2022.extract

occur_2022 <- occur_2022[!is.na(occur_2022$elevation), ]

# unique(is.na(occur_2022$elevation))

# Remove duplicates
duplic <- duplicated(occur_2022[ , c("Longitude", "Latitude")])
occur_2022 <- occur_2022[!duplic, ]

# Reduce spatial correlation: thinning procedure
sp.spatial.cor <- as.data.frame(occur_2022[ ,c("Longitude","Latitude")])
colnames(sp.spatial.cor) <- c("x", "y")

xy.thin.1 <- SDMworkshop::reduceSpatialCorrelation(
  xy = sp.spatial.cor,
  variables = elevation,
  minimum.distance = minimum.dist)

sp2022 <- cbind("P.clarkii", xy.thin.1)

# plot(elevation)
# points(sp2019$x, sp2019$y, col = "red", pch = 20, cex = .3)
# points(occur_2022$LONG, occur_2022$LAT, pch = 20, cex = .3)
# dev.off()

# Save occurrences
write.csv(sp2022, "./occurrences/2022_clean.csv", row.names=FALSE)
