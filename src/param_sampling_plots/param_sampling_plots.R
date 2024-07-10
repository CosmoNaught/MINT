library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)

source("plots.R")

set.seed(123)

orderly2::orderly_dependency("param_sampling", "latest()",
                             c("lhs_scenarios_sample.csv" = "lhs_scenarios_sample.csv",
                             "grid_scenarios_sample.csv" = "grid_scenarios_sample.csv"))

# THIS IS FOR LATER!
# orderly2::orderly_dependency("random", "latest(parameter:n_samples > 10)",
#                              c("randm.rds" = "data.rds"))

grid_scenarios_sample <- read.csv("grid_scenarios_sample.csv")
lhs_scenarios_sample <- read.csv("lhs_scenarios_sample.csv")

# Subset for debug output
grid_sample <- grid_scenarios_sample# %>% sample_n(5000)
lhs_sample <- lhs_scenarios_sample# %>% sample_n(5000)

pdf("plots.pdf")

param_histogram_plot(grid_sample, lhs_sample)
param_scatter_plot(grid_sample, lhs_sample)
param_pair_scatter_plot(grid_sample, lhs_sample, "eir", "dn0")
param_pair_scatter_plot(grid_sample, lhs_sample, "itn_future", "irs_future")
param_pair_scatter_plot(grid_sample, lhs_sample, "anthropophagy", "biting_inbed_indoors")

dev.off()