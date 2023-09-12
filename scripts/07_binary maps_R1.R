#####################################################################################################
# Title: Insights from the past: Invasion trajectory and niche trends of a global freshwater invader#                                            
#                                                                                                   #
# Scripts led by: Tommaso Cancellario & Simone Guareschi                                            #
#                                                                                                   #
# Creation: 2022 - 05 - 05                                                                          #
# Last update: 2023 - 09 - 10                                                                       #
#####################################################################################################

#---------------------------------------------#
# Binary maps                                 #
# Analysis to find cutoff value based on TSS  #
# Used for: Figure 4 and S6                   #
#---------------------------------------------#

# Load libraries
library(rgdal) 
library(raster) 
library(biomod2) # version 4.1-2

# Set working directory --------------------------------------------------------

setwd("./")

# Load saved models from the folders.
# tutorial: https://rstudio-pubs-static.s3.amazonaws.com/46255_c18cef0dd2784d979d874981ebfe873c.html

################ 1950

myBiomodEM_50<- load("./mod1950.word4.1/.BIOMOD_DATA/AllModels/formated.input.data")
myBiomodEM_50<- get(myBiomodEM_50)

myBiomodEM_50@coord

coord50<-myBiomodEM_50@coord

coord50$PA<-NA

# n. of presence=78
coord50$PA[1:78]<-1

coord50$PA[79:length(coord50$PA)]<-0

coord50$PA <- as.numeric(coord50$PA)

# considering proj to 2022 with 1950 occurences
suit50_2022<-raster("./mod1950.word4.1_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd")

PA1950_2022.extract <- extract(x = suit50_2022, y = coord50[ , c("x","y")])

bm_FindOptimStat(metric.eval = "TSS", obs = coord50$PA, fit = PA1950_2022.extract)
#cutoff 225 (1950 to 2022)

################ 1975

myBiomodEM_75<- load("./mod1975.word4.1/.BIOMOD_DATA/AllModels/formated.input.data")
myBiomodEM_75<- get(myBiomodEM_75)

myBiomodEM_75@coord

coord75<-myBiomodEM_75@coord

coord75$PA<-NA

# n. of presence=265
coord75$PA[1:265]<-1

coord75$PA[266:length(coord75$PA)]<-0

coord75$PA <- as.numeric(coord75$PA)

# considering proj to 2022 with 1975 occurences
suit75_2022<-raster("./mod1975.word4.1_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd")

PA1975_2022.extract <- extract(x = suit75_2022, y = coord75[ , c("x","y")])

bm_FindOptimStat(metric.eval = "TSS", obs = coord75$PA, fit = PA1975_2022.extract)
#cutoff 210

################ 2000

myBiomodEM_2000<- load("./mod2000.word4.1/.BIOMOD_DATA/AllModels/formated.input.data")
myBiomodEM_2000<- get(myBiomodEM_2000)

myBiomodEM_2000@coord

coord2000<-myBiomodEM_2000@coord

coord2000$PA<-NA

# n. of presence=691
coord2000$PA[1:691]<-1

coord2000$PA[692:length(coord2000$PA)]<-0

coord2000$PA <- as.numeric(coord2000$PA)

# considering proj to 2022 with 2000 records

suit2000_2022<-raster("./mod2000.word4.1_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd")

PA2000_2022.extract <- extract(x = suit2000_2022, y = coord2000[ , c("x","y")])

bm_FindOptimStat(metric.eval = "TSS", obs = coord2000$PA, fit = PA2000_2022.extract)
#cutoff 241


################ 2022

myBiomodEM_2022<- load("./mod2022.word4.1/.BIOMOD_DATA/AllModels/formated.input.data")
myBiomodEM_2022<- get(myBiomodEM_2022)

myBiomodEM_2022@coord

coord2022<-myBiomodEM_2022@coord

coord2022$PA<-NA

# n. of presence=2995
coord2022$PA[1:2995]<-1

coord2022$PA[2996:length(coord2022$PA)]<-0

coord2022$PA <- as.numeric(coord2022$PA)

suit2022_2022<-raster("./mod2022.word4.1_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd")

PA2022_2022.extract <- extract(x = suit2022_2022, y = coord2022[ , c("x","y")])

bm_FindOptimStat(metric.eval = "TSS", obs = coord2022$PA, fit = PA2022_2022.extract)
# cutoff 396




