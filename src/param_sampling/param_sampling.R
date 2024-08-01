library(dplyr)
library(tidyr)
library(lhs)
library(scales)
library(ggplot2)
library(GGally)
library(foreach)

if (local_cluster == TRUE){
  library(doParallel)
}

set.seed(123)

orderly2::orderly_parameters(run = NULL, local_cluster = NULL)

if(run == "short_run"){
  total_samples = 10000
  sample_prop = 0.1
  subset_samples = 1000
} else if(run == "long_run"){
  total_samples = 1000000
  sample_prop = 0.1
  subset_samples = 10000
} else {
  stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: "), run)
}

orderly2::orderly_dependency("collate_bednet_param", "latest()", c("bednet_params_raw.RDS" = "bednet_params_raw.RDS"))

# Load bednet parameters from CSV
bednet_params_raw <- readRDS("bednet_params_raw.RDS")

# Define parameter ranges
eir_range <- sort(10^(runif(16, log10(0.0001), log10(300))))
dn0_min_non_zero <- min(bednet_params_raw$dn0[bednet_params_raw$dn0 > 0]) * 0.9
dn0_max <- max(bednet_params_raw$dn0) * 1.2
dn0_range <- sort(runif(10, dn0_min_non_zero, dn0_max))
Q0_range <- c(0.6, 1)
phi_bednets_range <- c(0.4, 0.95)
seasonal_range <- c(0, 1)
itn_use_range <- seq(0, 1, by = 0.2)
irs_use_range <- seq(0, 1, by = 0.2)
itn_future_range <- seq(0, 1, length.out = 5)
irs_future_range <- seq(0, 1, length.out = 5)
lsm_range <- c(seq(0, 1, length.out = 5), 0, 0.9)

# Collect all parameter ranges in a list for LHS
all_ranges <- list(eir = eir_range, dn0_use = dn0_range, dn0_future = dn0_range, Q0 = Q0_range,
                   phi_bednets = phi_bednets_range, seasonal = seasonal_range, 
                   itn_use = itn_use_range, irs_use = irs_use_range, itn_future = itn_future_range,
                   irs_future = irs_future_range, lsm = lsm_range)

num_corner_samples <- ceiling(sample_prop * total_samples)
corner_samples <- expand.grid(lapply(all_ranges, function(r) r))[1:num_corner_samples, ]
n_lhs <- total_samples - num_corner_samples

if (local_cluster == TRUE) {
  # Set up parallel backend to use multiple cores
  registerDoParallel(cores = detectCores() - 2)
}

# Generate LHS samples using parallel foreach
lhs_samples <- foreach(i = 1:n_lhs, .combine = rbind) %dopar% {
  maximinLHS(1, length(all_ranges))
}
colnames(lhs_samples) <- names(all_ranges)

# Scale the LHS samples to the desired ranges
scaled_lhs_samples <- sapply(names(all_ranges), function(var) {
  range_values <- unlist(all_ranges[[var]])
  if (var == "eir") {
    # For EIR, use log scale
    lhs_samples[, var] <- 10^(scales::rescale(lhs_samples[, var], to = range(log10(range_values))))
  } else if (length(range_values) == 2 && all(range_values %in% c(0, 1))) {
    # For binary variables, round to 0 or 1
    lhs_samples[, var] <- round(scales::rescale(lhs_samples[, var], to = range(range_values)))
  } else if (is.numeric(range_values)) {
    # For continuous variables, use regular scaling
    lhs_samples[, var] <- scales::rescale(lhs_samples[, var], to = range(range_values))
  } else {
    # For discrete variables, round to nearest discrete value
    lhs_samples[, var] <- round(scales::rescale(lhs_samples[, var], to = c(1, length(range_values))))
    lhs_samples[, var] <- range_values[lhs_samples[, var]]
  }
  return(lhs_samples[, var])
}, simplify = "data.frame")

lhs_scenarios <- as.data.frame(scaled_lhs_samples)
lhs_scenarios <- rbind(lhs_scenarios, corner_samples)

# Base expand grid scenario
grid_scenarios <- expand.grid(eir = eir_range, Q0 = Q0_range,
                              phi_bednets = phi_bednets_range, seasonal = seasonal_range, 
                              dn0_use = dn0_range, dn0_future = dn0_range, itn_use = itn_use_range, irs_use = irs_use_range,
                              itn_future = itn_future_range, irs_future = irs_future_range,
                              lsm = lsm_range, stringsAsFactors = FALSE)

# # Write to output
if(local_cluster == TRUE) {
  print(dim(grid_scenarios))
  print(dim(lhs_scenarios))
}

# This is silly so need to stop saving it.
#write.csv(grid_scenarios, "grid_scenarios.csv", row.names = FALSE)
write.csv(lhs_scenarios, "lhs_scenarios.csv", row.names = FALSE)

# Write subset to output for debugging
grid_sample <- grid_scenarios %>% sample_n(subset_samples)
lhs_sample <- lhs_scenarios %>% sample_n(subset_samples)

if(local_cluster == TRUE) {
  print(dim(grid_sample))
  print(dim(lhs_sample))
}

write.csv(grid_sample, "grid_scenarios_sample.csv", row.names = FALSE)
write.csv(lhs_sample, "lhs_scenarios_sample.csv", row.names = FALSE)

if (local_cluster == TRUE) {
  # Stop the parallel backend
  stopImplicitCluster()
}