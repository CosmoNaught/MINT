library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)

source("plots.R")

set.seed(123)

orderly2::orderly_parameters(run = NULL)

if(!run %in% c("short_run", "long_run")) {
    stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

orderly2::orderly_dependency("param_sampling", "latest(parameter:run == this:run)",
                             c("lhs_scenarios_sample.csv" = "lhs_scenarios_sample.csv",
                             "grid_scenarios_sample.csv" = "grid_scenarios_sample.csv"))

grid_sample <- read.csv("grid_scenarios_sample.csv")
lhs_sample <- read.csv("lhs_scenarios_sample.csv")

pdf("plots.pdf")

param_histogram_plot(grid_sample, lhs_sample)
param_scatter_plot(grid_sample, lhs_sample)
param_pair_scatter_plot(grid_sample, lhs_sample, "eir", "dn0")
param_pair_scatter_plot(grid_sample, lhs_sample, "itn_future", "irs_future")
param_pair_scatter_plot(grid_sample, lhs_sample, "Q0", "phi_bednets")

dev.off()