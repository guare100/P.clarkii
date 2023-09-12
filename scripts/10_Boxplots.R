#######################################################################################################
# Title: Insights from the past: Invasion trajectory and niche trends of a global freshwater invader  #                                    
#                                                                                                     #
# Scripts led by: Tommaso Cancellario & Simone Guareschi                                              #
#                                                                                                     #
# Creation: 2022 - 05 - 05                                                                            #
# Last update: 2023 - 05 - 20                                                                         #
#######################################################################################################

# Figure 3 #

# Load libraries
library(sf) # <- GIS
library(rgdal) # <- GIS
library(raster) # <- GIS
library(ggplot2)
library(dplyr)
library(effsize) 

# Set working directory ----
# setwd("./")

# To upload Ensemble models for each period (3 for data up to 1950, 2 for data up to 1975 and 1 for data up to 2000). Here our examples:

map1975_50<-raster("./mod1950.word4.1/proj_to_1975/individual_projections/mod1950.word4.1_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd")

map2000_50<-raster("./mod1950.word4.1/proj_to_2000/individual_projections/mod1950.word4.1_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd")

map2022_50<-raster("./mod1950.word4.1/proj_to_2022/individual_projections/mod1950.word4.1_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd")

map2000_75<-raster("./mod1975.word4.1/proj_to_2000/individual_projections/mod1975.word4.1_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd")

map2022_75<-raster("./mod1975.word4.1/proj_to_2022/individual_projections/mod1975.word4.1_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd")
  
map2022_00<-raster("./mod2000.word4.1/proj_to_2022/individual_projections/mod2000.word4.1_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd")


# Upload all the occurrences (period by period). Data available on OSF, file: ESM_OSF link_Records_Pclarkii_1854_2022.xlsx

before1950<-read.csv("./")
before1975<-read.csv("./")
before2000<-read.csv("./")
before2022<-read.csv("./")


# Approx with 4 digits and deleting duplicates
before1950$Latitude<-round(before1950$Latitude, digits=4);before1950$Longitude<-round(before1950$Longitude, digits=4)
before1975$Latitude<-round(before1975$Latitude, digits=4);before1975$Longitude<-round(before1975$Longitude, digits=4)
before2000$Latitude<-round(before2000$Latitude, digits=4);before2000$Longitude<-round(before2000$Longitude, digits=4)
before2022$Latitude<-round(before2022$Latitude, digits=4);before2022$Longitude<-round(before2022$Longitude, digits=4)

before1950<-before1950[,c("Latitude", "Longitude")]
before1975<-before1975[,c("Latitude", "Longitude")]
before2000<-before2000[,c("Latitude", "Longitude")]
before2022<-before2022[,c("Latitude", "Longitude")]

before1950$species<-"p.clarkii"
before1975$species<-"p.clarkii"
before2000$species<-"p.clarkii"
before2022$species<-"p.clarkii"

duplicated(before1950)
length(which(duplicated(before1950)==TRUE))

before1950<-before1950[!duplicated(before1950), ]
before1975<-before1975[!duplicated(before1975), ]
before2000<-before2000[!duplicated(before2000), ]
before2022<-before2022[!duplicated(before2022), ]


## select un-shared records, using 1950 as starting point.

dif50_75<-setdiff(before1975, before1950)
dif50_00<-setdiff(before2000, before1950)
dif50_22<-setdiff(before2022, before1950)


# Convert dataframes to spatial dataframes. In order to extract the suitability values. 
spatial_dif50_75 <- SpatialPointsDataFrame(coords = dif50_75[,2:1], data = dif50_75, proj4string=map1975_50@crs)
spatial_dif50_00 <- SpatialPointsDataFrame(coords = dif50_00[,2:1], data = dif50_00, proj4string=map2000_50@crs)
spatial_dif50_22 <- SpatialPointsDataFrame(coords = dif50_22[,2:1], data = dif50_22, proj4string=map2022_50@crs)

plot(spatial_dif50_75)

# Extract suitability values for the occurrences not used when modelling.
suit_dif50_75<-extract(map1975_50, spatial_dif50_75)
suit_dif50_00<-extract(map2000_50, spatial_dif50_00)
suit_dif50_22<-extract(map2022_50, spatial_dif50_22)

# Convert to df with a new column (true_data) 
suit_dif50_75df<-data.frame(suitability=suit_dif50_75, records=rep("true_data", length(suit_dif50_75)))
suit_dif50_75df<-suit_dif50_75df[complete.cases(suit_dif50_75df),]
  
suit_dif50_00df<-data.frame(suitability=suit_dif50_00, records=rep("true_data", length(suit_dif50_00)))
suit_dif50_00df<-suit_dif50_00df[complete.cases(suit_dif50_00df),]
  
suit_dif50_22df<-data.frame(suitability=suit_dif50_22, records=rep("true_data", length(suit_dif50_22)))
suit_dif50_22df<-suit_dif50_22df[complete.cases(suit_dif50_22df),]

# Convert to df with a new column (random_data)
map1975_50df<-as.data.frame(map1975_50)
map1975_50df<-map1975_50df[!is.na(map1975_50df), ]
map_dif50_75df<-data.frame(suitability=map1975_50df, records=rep("random_data", length(map1975_50df)))
head(map_dif50_75df)

map2000_50df<-as.data.frame(map2000_50)
map2000_50df<-map2000_50df[!is.na(map2000_50df), ]
map_dif50_00df<-data.frame(suitability=map2000_50df, records=rep("random_data", length(map2000_50df)))
head(map_dif50_00df)

map2022_50df<-as.data.frame(map2022_50)
map2022_50df<-map2022_50df[!is.na(map2022_50df), ]
map_dif50_22df<-data.frame(suitability=map2022_50df, records=rep("random_data", length(map2022_50df)))
head(map_dif50_22df)


######### 1975 projection with data up to 1950 ################### 

### 1000 comparisons with Wilcox test and effect size analysis

testW1000<-data.frame()
recap<-data.frame()
values.plot<-data.frame()

set.seed(1234)

for(i in 1:1000){
  random.df.1 <- as.data.frame(round(map_dif50_75df$suitability[sample(nrow(map_dif50_75df), nrow(suit_dif50_75df))], digits = 0))
  colnames(random.df.1) <- "suitability"
  random.df.1$records<-rep("random", nrow(random.df.1))
  values.plot<-rbind(values.plot, random.df.1)
  random.df.1<-rbind(random.df.1, suit_dif50_75df)
  testW<-wilcox.test(suitability ~ factor(records), paired = FALSE, data=random.df.1)
  delta<-cliff.delta(d = random.df.1$suitability, f = random.df.1$records)
  testW.df<-data.frame(W=testW$statistic, pvalue=testW$p.value, efsize=delta$estimate, ci_u=delta$conf.int[2], ci_l=delta$conf.int[1])
  testW1000<-rbind(testW1000, testW.df)
  recap.1<-summaryBy(suitability~ factor(records), data=random.df.1, na.rm=TRUE, FUN=c(mean, median, sd, max))
  recap<-rbind(recap, recap.1)
  
    }

values.plot<-rbind(values.plot, suit_dif50_75df)

write.csv(testW1000,"G:/", row.names = FALSE)
write.csv(recap,"G:/", row.names = FALSE)


# summary with boxplot values
sum75_50<-summaryBy(suitability~ factor(records), data=values.plot, na.rm=TRUE, FUN=c(mean, median, sd, max))


# boxplot (Fig. 3)

bp75_dat50<-ggboxplot(values.plot, x = "records", y = "suitability", 
                       outlier.shape = NA, color = "records", palette = c("#00AFBB", "#E7B800"),  
                       ylab = "suitability", xlab = "records") + theme(legend.position="right")



######### 2000 projection with data up to 1950 ################### 

### 1000 comparisons with Wilcox test and effect size analysis

testW1000<-data.frame()
recap<-data.frame()
values.plot<-data.frame()

set.seed(1234)

for(i in 1:1000){
  random.df.1 <- as.data.frame(round(map_dif50_00df$suitability[sample(nrow(map_dif50_00df), nrow(suit_dif50_00df))], digits = 0))
  colnames(random.df.1) <- "suitability"
  random.df.1$records<-rep("random", nrow(random.df.1))
  values.plot<-rbind(values.plot, random.df.1)
  random.df.1<-rbind(random.df.1, suit_dif50_00df)
  testW<-wilcox.test(suitability ~ factor(records), paired = FALSE, data=random.df.1)
  delta<-cliff.delta(d = random.df.1$suitability, f = random.df.1$records)
  testW.df<-data.frame(W=testW$statistic, pvalue=testW$p.value, efsize=delta$estimate, ci_u=delta$conf.int[2], ci_l=delta$conf.int[1])
  testW1000<-rbind(testW1000, testW.df)
  recap.1<-summaryBy(suitability~ factor(records), data=random.df.1, na.rm=TRUE, FUN=c(mean, median, sd, max))
  recap<-rbind(recap, recap.1)
  
}

values.plot50_00<-rbind(values.plot, suit_dif50_00df)

write.csv(testW1000,"G:/", row.names = FALSE)
write.csv(recap,"G:/", row.names = FALSE)

# boxplot (Fig. 3)
bp2000_dat50<-ggboxplot(values.plot50_00, x = "records", y = "suitability", 
                      outlier.shape = NA, color = "records", palette = c("#00AFBB", "#E7B800"),  
                      ylab = "suitability", xlab = "records") + theme(legend.position="right")


######### 2000 projection with data up to 1950 ################### 

### 1000 comparisons with Wilcox test and effect size analysis 

testW1000<-data.frame()
recap<-data.frame()
values.plot<-data.frame()

set.seed(1234)

for(i in 1:1000){
    random.df.1 <- as.data.frame(round(map_dif50_22df$suitability[sample(nrow(map_dif50_22df), nrow(suit_dif50_22df))], digits = 0))
    colnames(random.df.1) <- "suitability"
    random.df.1$records<-rep("random", nrow(random.df.1))
    values.plot<-rbind(values.plot, random.df.1)
    random.df.1<-rbind(random.df.1, suit_dif50_22df)
    testW<-wilcox.test(suitability ~ factor(records), paired = FALSE, data=random.df.1)
    delta<-cliff.delta(d = random.df.1$suitability, f = random.df.1$records)
    testW.df<-data.frame(W=testW$statistic, pvalue=testW$p.value, efsize=delta$estimate, ci_u=delta$conf.int[2], ci_l=delta$conf.int[1])
    testW1000<-rbind(testW1000, testW.df)
    recap.1<-summaryBy(suitability~ factor(records), data=random.df.1, na.rm=TRUE, FUN=c(mean, median, sd, max))
    recap<-rbind(recap, recap.1)
    #print(i)
  }

values.plot50_22<-rbind(values.plot, suit_dif50_22df)

write.csv(testW1000,"G:/", row.names = FALSE)
write.csv(recap,"G:/", row.names = FALSE)

# boxplot (Fig. 3)
bp2022_dat50<-ggboxplot(values.plot50_22, x = "records", y = "suitability", 
                        outlier.shape = NA, color = "records", palette = c("#00AFBB", "#E7B800"),  
                        ylab = "suitability", xlab = "records") + theme(legend.position="right")

ggarrange(bp75_dat50, bp2000_dat50, bp2022_dat50,  labels = c("bp75_dat50", "bp2000_dat50", "bp2022_dat50"), common.legend = TRUE, legend = "bottom", ncol = 3, nrow = 1)



##### Change of dataset: starting point is the amount of information up to 1975. In this case just 2 steps, to 2000 and to 2022 
##################################################################################################################### 

dif75_00<-setdiff(before2000, before1975)
dif75_22<-setdiff(before2022, before1975)

spatial_dif75_00 <- SpatialPointsDataFrame(coords = dif75_00[,2:1], data = dif75_00, proj4string=map2000_75@crs)
spatial_dif75_22 <- SpatialPointsDataFrame(coords = dif75_22[,2:1], data = dif75_22, proj4string=map2022_75@crs)

suit_dif75_00<-extract(map2000_75, spatial_dif75_00)
suit_dif75_22<-extract(map2022_75, spatial_dif75_22)

suit_dif75_00df<-data.frame(suitability=suit_dif75_00, records=rep("true_data", length(suit_dif75_00)))
suit_dif75_00df<-suit_dif75_00df[complete.cases(suit_dif75_00df),]

suit_dif75_22df<-data.frame(suitability=suit_dif75_22, records=rep("true_data", length(suit_dif75_22)))
suit_dif75_22df<-suit_dif75_22df[complete.cases(suit_dif75_22df),]

map2000_75df<-as.data.frame(map2000_75)
map2000_75df<-map2000_75df[!is.na(map2000_75df), ]
map_dif75_00df<-data.frame(suitability=map2000_75df, records=rep("random_data", length(map2000_75df)))
head(map_dif75_00df)

map2022_75df<-as.data.frame(map2022_75)
map2022_75df<-map2022_75df[!is.na(map2022_75df), ]
map_dif75_22df<-data.frame(suitability=map2022_75df, records=rep("random_data", length(map2022_75df)))
head(map_dif75_22df)


### 1000 comparisons with Wilcox test and effect size analysis 
# 2000 with data up to 1975. 
  
testW1000<-data.frame()
recap<-data.frame()
values.plot<-data.frame()

set.seed(1234)

for(i in 1:1000){
  random.df.1 <- as.data.frame(round(map_dif75_00df$suitability[sample(nrow(map_dif75_00df), nrow(suit_dif75_00df))], digits = 0))
  colnames(random.df.1) <- "suitability"
  random.df.1$records<-rep("random", nrow(random.df.1))
  values.plot<-rbind(values.plot, random.df.1)
  random.df.1<-rbind(random.df.1, suit_dif75_00df)
  testW<-wilcox.test(suitability ~ factor(records), paired = FALSE, data=random.df.1)
  delta<-cliff.delta(d = random.df.1$suitability, f = random.df.1$records)
  testW.df<-data.frame(W=testW$statistic, pvalue=testW$p.value, efsize=delta$estimate, ci_u=delta$conf.int[2], ci_l=delta$conf.int[1])
  testW1000<-rbind(testW1000, testW.df)
  recap.1<-summaryBy(suitability~ factor(records), data=random.df.1, na.rm=TRUE, FUN=c(mean, median, sd, max))
  recap<-rbind(recap, recap.1)
  #print(i)
}

values.plot75_00<-rbind(values.plot, suit_dif75_00df)

write.csv(testW1000,"G:/", row.names = FALSE)
write.csv(recap,"G:/", row.names = FALSE)

# boxplot (Fig. 3)
bp2000_dat75<-ggboxplot(values.plot75_00, x = "records", y = "suitability", 
                        outlier.shape = NA, color = "records", palette = c("#00AFBB", "#E7B800"),  
                        ylab = "suitability", xlab = "records") + theme(legend.position="right")



### 2022 with data up to 1975 ##############

testW1000<-data.frame()
recap<-data.frame()
values.plot<-data.frame()

set.seed(1234)

for(i in 1:1000){
  random.df.1 <- as.data.frame(round(map_dif75_22df$suitability[sample(nrow(map_dif75_22df), nrow(suit_dif75_22df))], digits = 0))
  colnames(random.df.1) <- "suitability"
  random.df.1$records<-rep("random", nrow(random.df.1))
  values.plot<-rbind(values.plot, random.df.1)
  random.df.1<-rbind(random.df.1, suit_dif75_22df)
  testW<-wilcox.test(suitability ~ factor(records), paired = FALSE, data=random.df.1)
  delta<-cliff.delta(d = random.df.1$suitability, f = random.df.1$records)
  testW.df<-data.frame(W=testW$statistic, pvalue=testW$p.value, efsize=delta$estimate, ci_u=delta$conf.int[2], ci_l=delta$conf.int[1])
  testW1000<-rbind(testW1000, testW.df)
  recap.1<-summaryBy(suitability~ factor(records), data=random.df.1, na.rm=TRUE, FUN=c(mean, median, sd, max))
  recap<-rbind(recap, recap.1)
  #print(i)
}

values.plot75_22<-rbind(values.plot, suit_dif75_22df)

write.csv(testW1000,"G:/", row.names = FALSE)
write.csv(recap,"G:/", row.names = FALSE)

# boxplot (Fig. 3)
bp2022_dat75<-ggboxplot(values.plot75_22, x = "records", y = "suitability", 
                        outlier.shape = NA, color = "records", palette = c("#00AFBB", "#E7B800"),  
                        ylab = "suitability", xlab = "records") + theme(legend.position="right") 



##### Change of dataset: starting point is the amount of information up to 2000. 


dif00_22<-setdiff(before2022, before2000)

spatial_dif00_22 <- SpatialPointsDataFrame(coords = dif00_22[,2:1], data = dif00_22, proj4string=map2022_00@crs)

suit_dif00_22<-extract(map2022_00, spatial_dif00_22)


suit_dif00_22df<-data.frame(suitability=suit_dif00_22, records=rep("true_data", length(suit_dif00_22)))
suit_dif00_22df<-suit_dif00_22df[complete.cases(suit_dif00_22df),]

map2022_00df<-as.data.frame(map2022_00)
map2022_00df<-map2022_00df[!is.na(map2022_00df), ]
map_dif00_22df<-data.frame(suitability=map2022_00df, records=rep("random_data", length(map2022_00df)))
head(map_dif00_22df)

### 1000 comparisons with Wilcox test and effect size analysis

testW1000<-data.frame()
recap<-data.frame()
values.plot<-data.frame()

set.seed(1234)

for(i in 1:1000){
  random.df.1 <- as.data.frame(round(map_dif00_22df$suitability[sample(nrow(map_dif00_22df), nrow(suit_dif00_22df))], digits = 0))
  colnames(random.df.1) <- "suitability"
  random.df.1$records<-rep("random", nrow(random.df.1))
  values.plot<-rbind(values.plot, random.df.1)
  random.df.1<-rbind(random.df.1, suit_dif00_22df)
  testW<-wilcox.test(suitability ~ factor(records), paired = FALSE, data=random.df.1)
  delta<-cliff.delta(d = random.df.1$suitability, f = random.df.1$records)
  testW.df<-data.frame(W=testW$statistic, pvalue=testW$p.value, efsize=delta$estimate, ci_u=delta$conf.int[2], ci_l=delta$conf.int[1])
  testW1000<-rbind(testW1000, testW.df)
  recap.1<-summaryBy(suitability~ factor(records), data=random.df.1, na.rm=TRUE, FUN=c(mean, median, sd, max))
  recap<-rbind(recap, recap.1)
  print(i)
}

values.plot00_22<-rbind(values.plot, suit_dif00_22df)

write.csv(testW1000,"G:/", row.names = FALSE)
write.csv(recap,"G:/", row.names = FALSE)

# boxplot (Fig. 3)
bp2022_dat00<-ggboxplot(values.plot00_22, x = "records", y = "suitability", 
                        outlier.shape = NA, color = "records", palette = c("#00AFBB", "#E7B800"),  
                        ylab = "suitability", xlab = "records")  + theme(legend.position="right")

ggarrange(bp2000_dat75, bp2022_dat75, bp2022_dat00,  labels = c("bp2000_dat75", "bp2022_dat75", "bp2022_dat00"), common.legend = TRUE, legend = "bottom", ncol = 3, nrow = 1)


