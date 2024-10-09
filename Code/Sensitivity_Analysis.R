### This file contains the code for the sensitivity analyses conducted for the
### publication "Tourist Types in a spatio-temporal context"."

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

# Load raw data
raw_data = readRDS("Data/220607_dat_ftf.rds")

# Data 1983-1990  ---------------------------------------------------------

# data for 1983 - 1990:
data <- raw_data |> filter(travel_year <= 1990)

# Full dataset used for the study:
data_all <- read_and_prepare_data(data)

# Structured dataset for static clustering:
data_clustering_all <- prepare_clustering_data(data_all)

# Load clustering information about clustering variables in global environment:
assign_clustering_variables(data_clustering_all)

# Sample used for clustering analysis:
data_clustering_sample <- filter_travel_year(data_clustering_all)

# Clustering 1983-1990 ----------------------------------------------------

cluster_results5 <- fuzzyclara(data = data, clusters = 5, metric = dist_tourist,
                             algorithm = "clara", scale = FALSE, samples = 20,
                             sample_size = 10000, type = "fuzzy",
                             seed = 240822, cores = 1, verbose = 2,
                             nstart = 50, build = TRUE)
cluster_results5$fuzzyness_exponent <- cluster_results5$fuzzyness

# Full dataset:
analysis_data <- prepare_analysis_data(clustering_result = cluster_results5,
                                       analysis_data = data_all,
                                      clustering_data = data_clustering_all,
                                       all_data = TRUE, years = "198390")

# Graphical visualizations:
plot_static_hard <- plot_destination_per_cluster(analysis_data, type = "hard")
plot_static_cluster <- plot_destination_per_cluster(analysis_data,
                                                    type = "fuzzy")
plot_static <- ggarrange(plot_static_hard, plot_static_cluster,
                         common.legend = TRUE)
plot_static
ggsave(plot = plot_static, filename = "Graphics/Additionals/Figure_static_19831990.jpeg",
       width = 16, height = 8)
plot_star <- starplot(analysis_data)
plot_star
ggsave(plot = plot_star, filename = "Graphics/Additionals/Figure_star_19831990.jpeg", width = 8,
       height = 8)



# Data 2011-2018 ----------------------------------------------------------

# Filter travel years:
data <- raw_data %>% filter(travel_year >= 2011)

data_all <- read_and_prepare_data(data)

# Structured dataset for static clustering:
data_clustering_all <- prepare_clustering_data(data_all)

# Load clustering information about clustering variables in global environment:
assign_clustering_variables(data_clustering_all)

# Sample used for clustering analysis:
data_clustering_sample <- filter_travel_year(data_clustering_all)

# Clustering 2011-2018 ----------------------------------------------------

cluster_results5 <- fuzzyclara(data = data, clusters = 5, metric = dist_tourist,
                             algorithm = "clara", scale = FALSE, samples = 20,
                             sample_size = 10000, type = "fuzzy",
                             seed = 240822, cores = 20, verbose = 2,
                             nstart = 50, build = TRUE)

cluster_results5$fuzzyness_exponent <- cluster_results5$fuzzyness

# Full dataset:
analysis_data <- prepare_analysis_data(clustering_result = cluster_results5,
                                      analysis_data = data_all,
                                      clustering_data = data_clustering_all,
                                      all_data = TRUE, years = "201118")

# Graphical visualizations:
plot_static_hard <- plot_destination_per_cluster(analysis_data, type = "hard")
plot_static_cluster <- plot_destination_per_cluster(analysis_data,
                                                    type = "fuzzy")
plot_static <- ggarrange(plot_static_hard, plot_static_cluster,
                         common.legend = TRUE)
ggsave(plot = plot_static, filename = "Graphics/Additionals/Figure_static_20112018.jpeg",
       width = 16, height = 8)
plot_star <- starplot(analysis_data)
plot_star
ggsave(plot = plot_star, filename = "Graphics/Additionals/Figure_star_20112018.jpeg", width = 8,
       height = 8)

################################################################################