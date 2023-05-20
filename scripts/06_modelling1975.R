##################################################
# Title: XXX                                     #
# Scripts led by:                                #
# Tommaso Cancellario & Simone Guareschi         #
#                                                #
# Creation: 2022 - 05 - 05                       #
# Last update: 2023 - 05 - 20                    #
##################################################

#----------------#
# Modelling 1975 #
#----------------#

# Load libraries ----
library(biomod2) # Ensemble SDM package
library(dplyr) # Manage data
library(maps) # Easy access to basic map layers
library(parallel)
library(raster) # Gis
library(rgeos) # Gis
library(terra) # requested by biomod2 4.1-2


# Set working directory ----
# setwd("./")


# Load predictors variables ----
variables_sub75 <- raster::stack(c(
      elevation_nopoles = "./variables_model/variables_def_checkNA/66_75/elevation_nopoles_checkNA.tif",
      bio15_bv_66_75 = "./variables_model/variables_def_checkNA/66_75/bio15_bv_66_75_checkNA.tif",
      PETseasonality_env_66_75 = "./variables_model/variables_def_checkNA/66_75/PETseasonality_env_66_75_checkNA.tif",
      bio18_bv_66_75 = "./variables_model/variables_def_checkNA/66_75/bio18_bv_66_75_checkNA.tif",
      bio1_bv_66_75 = "./variables_model/variables_def_checkNA/66_75/bio1_bv_66_75_checkNA.tif",
      embergerQ_env_66_75 = "./variables_model/variables_def_checkNA/66_75/embergerQ_env_66_75_checkNA.tif",
      aridityIndexThornthwaite_env_66_75 = "./variables_model/variables_def_checkNA/66_75/aridityIndexThornthwaite_env_66_75_checkNA.tif"
    )
  )

variables_sub00 <- raster::stack(c(
      elevation_nopoles = "./variables_model/variables_def_checkNA/91_00/elevation_nopoles_checkNA.tif",
      bio15_bv_66_75 = "./variables_model/variables_def_checkNA/91_00/bio15_bv_91_00_checkNA.tif",
      PETseasonality_env_66_75 = "./variables_model/variables_def_checkNA/91_00/PETseasonality_env_91_00_checkNA.tif",
      bio18_bv_66_75 = "./variables_model/variables_def_checkNA/91_00/bio18_bv_91_00_checkNA.tif",
      bio1_bv_66_75 = "./variables_model/variables_def_checkNA/91_00/bio1_bv_91_00_checkNA.tif",
      embergerQ_env_66_75 = "./variables_model/variables_def_checkNA/91_00/embergerQ_env_91_00_checkNA.tif",
      aridityIndexThornthwaite_env_66_75 = "./variables_model/variables_def_checkNA/91_00/aridityIndexThornthwaite_env_91_00_checkNA.tif"
    )
  )

variables_sub22 <- raster::stack(c(
      elevation_nopoles = "./variables_model/variables_def_checkNA/09_18/elevation_nopoles_checkNA.tif",
      bio15_bv_66_75 = "./variables_model/variables_def_checkNA/09_18/bio15_bv_09_18_checkNA.tif",
      PETseasonality_env_66_75 = "./variables_model/variables_def_checkNA/09_18/PETseasonality_env_09_18_checkNA.tif",
      bio18_bv_66_75 = "./variables_model/variables_def_checkNA/09_18/bio18_bv_09_18_checkNA.tif",
      bio1_bv_66_75 = "./variables_model/variables_def_checkNA/09_18/bio1_bv_09_18_checkNA.tif",
      embergerQ_env_66_75 = "./variables_model/variables_def_checkNA/09_18/embergerQ_env_09_18_checkNA.tif",
      aridityIndexThornthwaite_env_66_75 = "./variables_model/variables_def_checkNA/09_18/aridityIndexThornthwaite_env_09_18_checkNA.tif"
    )
  )

# Load occurrences 1975 ----
occ_1975 <- read.csv("./occurrences/1975_clean.csv")
occ_1975$presence <- 1

summary(occ_1975)
colnames(occ_1975)[1] <- "P_clarkii"


# Prepare data for biomod2 and modelling ----
set.seed(2222)

clarkii_data75 <- BIOMOD_FormatingData(
    resp.var = occ_1975["presence"],
    resp.xy = occ_1975[, c("x", "y")],
    expl.var = variables_sub75,
    resp.name = "mod1975.word4.1",
    PA.nb.rep = 3,
    PA.nb.absences = 10000,
    PA.strategy = "random"
  )

# biomode2 model options
myBiomodOptions <- BIOMOD_ModelingOptions()

clarkii_models75 <- BIOMOD_Modeling(bm.format = clarkii_data75,
                                    modeling.id = "AllModels",
                                    models = c("GBM", "RF", "GLM", "MAXENT.Phillips.2"),
                                    bm.options = myBiomodOptions,
                                    nb.rep = 3,
                                    data.split.perc = 70,
                                    metric.eval = c("TSS", "ROC"),
                                    var.import = 3,
                                    do.full.models = TRUE,
                                    nb.cpu = 9,
                                    seed.val = 22,
                                    do.progress = TRUE)

# Model evaluation
clarkii_models_scores <- get_evaluations(clarkii_models75)
write.csv(as.data.frame(clarkii_models_scores), "./mod1975.word4.1/single_model_evaluations75.csv", row.names = TRUE)


# Variables importance
clarkii_models_var_import <- get_variables_importance(clarkii_models75, as.data.frame = TRUE)

clarkii_models_var_import <- clarkii_models_var_import %>%
                             group_by(Algo, Run, Dataset, Rand) %>%
                             mutate(tot = sum(Var.imp)) %>%
                             ungroup

clarkii_models_var_import <- clarkii_models_var_import %>% 
                             group_by(Algo, Run, Dataset, Rand) %>%
                             mutate(std_var.imp = Var.imp/tot) %>%
                             ungroup

clarkii_models_var_import <- clarkii_models_var_import %>% 
                             group_by(Algo, Expl.var) %>%
                             mutate(mean = mean(std_var.imp)) %>%
                             ungroup

clarkii_models_var_import <- clarkii_models_var_import %>%
                             group_by(Algo, Expl.var) %>%
                             mutate(sd = sd(std_var.imp)) %>%
                             ungroup

models_var_import_no_dup <- clarkii_models_var_import[ ,c(2, 5, 10, 11)]
models_var_import_no_dup <- distinct(models_var_import_no_dup)

write.csv(as.data.frame(clarkii_models_var_import), "./mod1975.word4.1/mean_var_importance.csv", row.names = TRUE)
write.csv(as.data.frame(models_var_import_no_dup), "./mod1975.word4.1/models_var_import_stand_no_dup.csv", row.names = TRUE)


# Ensemble model
clarkiiEM75 <- BIOMOD_EnsembleModeling(bm.mod = clarkii_models75,
                                       models.chosen = "all",
                                       em.by = "all",
                                       metric.select = "TSS",
                                       metric.select.thresh = 0.7,
                                       metric.eval = c("TSS", "ROC"),
                                       var.import = 3,
                                       prob.mean = TRUE,
                                       prob.cv = FALSE,
                                       prob.ci = FALSE,
                                       prob.ci.alpha = 0.05,
                                       committee.averaging = FALSE,
                                       prob.mean.weight = TRUE,
                                       prob.mean.weight.decay = "proportional",
                                       seed.val = 42)

# Ensemble model evaluation
eva_EM75 <- get_evaluations(clarkiiEM75, as.data.frame = TRUE)
bm_PlotEvalMean(bm.out = clarkiiEM75, group.by = "model")
bm_PlotEvalBoxplot(bm.out =clarkiiEM75, group.by = c("model", "model"))
write.csv(as.data.frame(eva_EM75), "./mod1975.word4.1/eva_EM75.csv", row.names = TRUE)

# Ensemble variable importance
clarkii_models_var_importEM75 <- get_variables_importance(clarkiiEM75, as.data.frame = TRUE)
write.csv(as.data.frame(clarkii_models_var_importEM75), "./mod1975.word4.1/mean_var_importanceEM.csv", row.names = TRUE)

# Projections ----

# 1975
clarkii_models_proj_current <- BIOMOD_Projection(bm.mod = clarkii_models75,
                                                 proj.name = "Current",
                                                 new.env = variables_sub75,
                                                 models.chosen = "all",
                                                 metric.binary = "TSS",
                                                 metric.filter = "TSS",
                                                 nb.cpu = 9,
                                                 build.clamping.mask = TRUE,
                                                 output.format = ".grd")

clarkiiEM_proj_current <- BIOMOD_EnsembleForecasting(bm.em = clarkiiEM75,
                                                     bm.proj = clarkii_models_proj_current,
                                                     models.chosen = "all",
                                                     metric.binary = "TSS",
                                                     metric.filter = "TSS")

# Save projections 1975
dir.create("./mod1975.word4.1/raster_each_model75/")
for(i in 1:48){
  writeRaster(clarkii_models_proj_current@proj.out@val@layers[[i]], 
              filename = paste0("./mod1975.word4.1/raster_each_model75/", names(clarkii_models_proj_current@proj.out@val@layers[[i]]), ".tif"),
              format = "GTiff", overwrite = FALSE)
}; rm(i)


# 1975 to 2000
clarkii_models_proj_to2000 <- BIOMOD_Projection(bm.mod = clarkii_models75,
                                                proj.name = "to_2000",
                                                new.env = variables_sub00,
                                                models.chosen = "all",
                                                metric.binary = "TSS",
                                                metric.filter = "TSS",
                                                nb.cpu = 9,
                                                build.clamping.mask = TRUE,
                                                output.format = ".grd")

clarkiiEM_proj_to2000 <- BIOMOD_EnsembleForecasting(bm.em = clarkiiEM75,
                                                    bm.proj = clarkii_models_proj_to2000,
                                                    models.chosen = "all",
                                                    metric.binary = "TSS",
                                                    metric.filter = "TSS")

# Save projections 1975 to 2000
dir.create("./mod1975.word4.1/raster_each_model75to00/")
for(i in 1:48){
  writeRaster(clarkii_models_proj_to2000@proj.out@val@layers[[i]],
              filename = paste0("./mod1975.word4.1/raster_each_model75to00/", names(clarkii_models_proj_to2000@proj.out@val@layers[[i]]), "_00.tif"),
              format = "GTiff", overwrite = FALSE)
}; rm(i)


# 1975 to 2022
clarkii_models_proj_to2022 <- BIOMOD_Projection(bm.mod = clarkii_models75,
                                                proj.name = "to_2022",
                                                new.env = variables_sub22,
                                                models.chosen = "all",
                                                metric.binary = "TSS",
                                                metric.filter = "TSS",
                                                nb.cpu = 9,
                                                build.clamping.mask = TRUE,
                                                output.format = ".grd")

clarkiiEM_proj_to2022 <- BIOMOD_EnsembleForecasting(bm.em = clarkiiEM75,
                                                    bm.proj = clarkii_models_proj_to2022,
                                                    models.chosen = "all",
                                                    metric.binary = "TSS",
                                                    metric.filter = "TSS")

# Save projections 1975 to 2022
dir.create("./mod1975.word4.1/raster_each_model75to22/")
for(i in 1:48){
  writeRaster(clarkii_models_proj_to2022@proj.out@val@layers[[i]], 
  filename = paste0("./mod1975.word4.1/raster_each_model75to22/", names(clarkii_models_proj_to2022@proj.out@val@layers[[i]]), "_22.tif"),
              format = "GTiff", overwrite = FALSE)
}; rm(i)