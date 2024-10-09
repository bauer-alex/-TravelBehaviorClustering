### This is the main code file containing the analyses conducted for the publication 
### "Understanding travel behaviour patterns and their dynamics"

# Loading of necessary packages and sourcing of self-defined functions:
library(mgcv)
library(fuzzyclara)
library(APCtools)
library(tidyverse)
library(dplyr)
library(Rfast)
library(ggpubr)
library(stringr)
library(readxl)
library(pROC)
library(cvAUC)
library(colorspace)
source("Code/Functions.R")

# Data preparation --------------------------------------------------------

# Raw dataset
data = readRDS("Data/220607_dat_ftf.rds")

# Full dataset used for the study:
data_all <- read_and_prepare_data(data)

# Structured dataset for static clustering:
data_clustering_all <- prepare_clustering_data(data_all)

# Sample used for clustering analysis:
data_clustering_sample <- filter_travel_year(data_clustering_all)

# Load clustering information about clustering variables in global environment:
assign_clustering_variables(data_clustering_all)
  

# Number Clusters ---------------------------------------------------------

# Find optimal number of clusters:

cluster_results_list <- evaluate_cluster_numbers(data = data_clustering_sample,
                                                clusters_range = 1:8,
                                                metric = dist_tourist,
                                                algorithm = "clara",
                                                scale = FALSE, samples = 20,
                                                sample_size = 5000,
                                                type = "fuzzy", seed = 240822,
                                                cores = 10, verbose = 2,
                                                nstart = 50, build = TRUE,
                                                plot = FALSE,
                                                return_results = TRUE)
saveRDS(object = cluster_results_list,
	file = "Results/Static_Clustering/cluster_results_list.rds")

# Elbow plot:
## ggplot theme:
theme <- theme_minimal() +
  theme(text = element_text(size = 16), axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.key.width = unit(2, "lines"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        strip.text = element_text(size = 18, face = "bold"),
        strip.text.y = element_text(size = 16), legend.text.align = 0,
        strip.placement = "outside", strip.background = element_blank(),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)))
plot_cluster_numbers <- plot_cluster_numbers(cluster_results_list) + theme
ggsave(plot = plot_cluster_numbers, filename = "Graphics/Additionals/Figure_cluster_numbers.jpeg",
       width = 16, height = 8)

# Static clustering -------------------------------------------------------

cluster_results5 <- fuzzyclara(data = data_clustering_sample, clusters = 5,
                             metric = dist_tourist,
                             algorithm = "clara", scale = FALSE, samples = 20,
                             sample_size = 10000, type = "fuzzy",
                             seed = 240822, cores = 10, verbose = 2,
                             nstart = 50, build = TRUE)
saveRDS(object = cluster_results5,
        file = "Results/Static_Clustering/cluster_results5.rds")
cluster_results5$fuzzyness_exponent <- 1.5


# Use clustering results to create data for analysis
analysis_data <- prepare_analysis_data(clustering_result = cluster_results5,
                                       analysis_data = data_all,
                                       clustering_data = data_clustering_all,
                                       all_data = TRUE)
# Save to be used in Tabledata-file
saveRDS(object = analysis_data, file = "Results/Statistical_Modelling/analysis_data.rds")

# Figure 2 in paper:
# Overview of tourist types and relationships between the tourist types
plot_star <- starplot(analysis_data)

ggsave(plot = plot_star, filename = "Graphics/Figure2_Starplot.jpeg", width = 8,
       height = 8)


# Figure 3 in paper:
# Relative frequencies of destinations of main trip per general cluster (left), core and fuzzy cluster (right)
plot_static_hard    <- plot_destination_per_cluster(analysis_data, 
                                                    type = "hard")
plot_static_cluster <- plot_destination_per_cluster(analysis_data, 
                                                    type = "fuzzy")
plot_static         <- ggarrange(plot_static_hard, plot_static_cluster, common.legend = TRUE)

ggsave(plot = plot_static, filename = "Graphics/Additionals/Figure_static.jpeg", width = 16,
       height = 8)

# Dynamic clustering ------------------------------------------------------

for (i in 1:5) {
  plot_time_overall <- plot_cluster_over_time(analysis_data, type = "hard",
                                              cluster = i)
  plot_time_cluster <- plot_cluster_over_time(analysis_data, type = "fuzzy",
                                              cluster = i)
  plot_time <- ggarrange(plot_time_overall, plot_time_cluster)
  ggsave(plot = plot_time,
         filename = paste0("Graphics/Additionals/Figure_time_", i, ".jpeg"), width = 16,
         height = 8)
}


# Statistical modelling ---------------------------------------------------

# Hard clusters:

# Model estimation:
models <- fit_models(data = analysis_data, type = "hard")

saveRDS(object = models,
        file = "Results/Statistical_Modelling/Models_hard.rds")

# Model performance:
auc <- calculate_auc(data = analysis_data, type = "hard")

saveRDS(object = auc, file = "Results/Statistical_Modelling/auc_hard.rds")

# APC effects:
plot_APC <- plot_models_APC(analysis_data, models)

ggsave(plot = plot_APC, filename = "Graphics/Figure3_APC_hard.jpeg", width = 12,
       height = 6)

# Linear effects:
plot_linEffects <- plot_models_linear(models)

ggsave(plot = plot_linEffects, filename = "Graphics/Additionals/Figure_linEffects_hard.jpeg",
       width = 12, height = 12)

# Smooth effect of income:
plot_incEffects <- plot_income_effects(model = models, plot_ci = TRUE,
                                       select = 2, alpha = 0.05) #? ggf. Farbe für Cluster 3 ändern

ggsave(plot = plot_incEffects, filename = "Graphics/Additionals/Figure_incEffects_hard.jpeg",
       width = 12, height = 6)


# Fuzzy clusters:
for (i in 1:5) {
 
  # Model estimation:
  models <- fit_models(data = analysis_data, type = "fuzzy", cluster = i)
  saveRDS(object = models,
          file = paste0("Results/Statistical_Modelling/Models_fuzzy_", i, ".rds"))
  
  # Model performance:
  auc <- calculate_auc(data = analysis_data, type = "fuzzy", cluster = i,
                       n_cluster = length(models))
  saveRDS(object = auc,
          file = paste0("Results/Statistical_Modelling/auc_fuzzy_", i, ".rds"))
  
  # APC effects:
  plot_APC <- plot_models_APC(analysis_data, models, type = "fuzzy",
                              cluster = i)
  ggsave(plot = plot_APC, filename = paste0("Graphics/Additionals/Figure_APC_", i, ".jpeg"),
         width = 12, height = 6)
  
  # Linear effects:
  height_lin <- case_when(length(models) == 2 ~ 6,
                          length(models) == 3 ~ 8,
                          length(models) == 4 ~ 10,
                          length(models) == 5 ~ 12)
  plot_linEffects <- plot_models_linear(models, type = "fuzzy", cluster = i)
  ggsave(plot = plot_linEffects,
         filename = paste0("Graphics/Additionals/Figure_linEffects_", i, ".jpeg"),
         width = 12, height = height_lin)
  
  # Smooth effect of income:
  plot_incEffects <- plot_income_effects(model = models, plot_ci = TRUE,
                                         select = 2, alpha = 0.05,
                                         type = "fuzzy", cluster = i)
  ggsave(plot = plot_incEffects,
         filename = paste0("Graphics/Additionals/Figure_incEffects_", i, ".jpeg"),
         width = 12, height = 6)
}

