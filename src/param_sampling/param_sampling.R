# library(dplyr)
# library(tidyr)
# library(lhs)
# library(scales)
# library(ggplot2)
# library(GGally)

# set.seed(123)

# orderly2::orderly_dependency("bednet_param_gen", "latest()",
#                              c("bednet_params_raw.RDS" = "bednet_params_raw.RDS"))

# # Load bednet parameters from CSV
# bednet_params_raw <- readRDS("bednet_params_raw.RDS")

# # Define parameter ranges
# eir_range <- sort(10^(runif(12, log10(0.5), log10(300)))) # Continuous on log scale

# dn0_min_non_zero <- min(bednet_params_raw$dn0[bednet_params_raw$dn0 > 0]) * 0.9 # - 10% padding towards 0, non-zero cap
# dn0_max <- max(bednet_params_raw$dn0) * 1.1 # + padding 10%

# dn0_range <- sort(runif(10, dn0_min_non_zero, dn0_max)) # Continuous
# anthropophagy_range <- c(70, 98) # Discrete (specific level) 
# biting_inbed_indoors_range <- c(70, 98) # Discrete (specific level)
# seasonal_range <- c(0, 1) # Binary
# itn_use_range <- c(seq(0, 1, by = 0.2), 0.05, 0.95) # Discrete (cap 1)
# irs_use_range <- c(seq(0, 1, by = 0.2), 0.05, 0.95) # Discrete
# itn_future_range <- seq(0, 1, length.out = 5) # Discrete (cap 1)
# irs_future_range <- seq(0, 1, length.out = 5) # Discrete
# lsm_range <- seq(0, 0.9, length.out = 5) # Discrete cap (0.9)

# # Collect all parameter ranges in a list for LHS
# all_ranges <- list(eir = eir_range, dn0 = dn0_range, anthropophagy = anthropophagy_range,
#                    biting_inbed_indoors = biting_inbed_indoors_range, seasonal = seasonal_range,
#                    itn_use = itn_use_range, irs_use = irs_use_range, itn_future = itn_future_range,
#                    irs_future = irs_future_range, lsm = lsm_range)

# # Generate LHS samples for all variables
# n_lhs <- 1000  # Number of LHS samples
# lhs_samples <- maximinLHS(n_lhs, length(all_ranges))
# colnames(lhs_samples) <- names(all_ranges)

# # Scale the LHS samples to the desired ranges
# scaled_lhs_samples <- sapply(names(all_ranges), function(var) {
#   range_values <- unlist(all_ranges[[var]])
#   if (var == "eir") {
#     # For EIR, use log scale
#     lhs_samples[, var] <- 10^(scales::rescale(lhs_samples[, var], to = range(log10(range_values))))
#   } else if (length(range_values) == 2 && all(range_values %in% c(0, 1))) {
#     # For binary variables, round to 0 or 1
#     lhs_samples[, var] <- round(scales::rescale(lhs_samples[, var], to = range(range_values)))
#   } else if (is.numeric(range_values)) {
#     # For continuous variables, use regular scaling
#     lhs_samples[, var] <- scales::rescale(lhs_samples[, var], to = range(range_values))
#   } else {
#     # For discrete variables, round to nearest discrete value
#     lhs_samples[, var] <- round(scales::rescale(lhs_samples[, var], to = c(1, length(range_values))))
#     lhs_samples[, var] <- range_values[lhs_samples[, var]]
#   }
#   return(lhs_samples[, var])
# })

# # Convert scaled samples to data frame
# lhs_scenarios <- as.data.frame(scaled_lhs_samples)

# # Combine LHS samples with a grid approach for complete scenario generation
# grid_scenarios <- expand.grid(
#   eir = eir_range,
#   anthropophagy = anthropophagy_range,
#   biting_inbed_indoors = biting_inbed_indoors_range,
#   seasonal = seasonal_range,
#   dn0 = dn0_range,
#   itn_use = itn_use_range,
#   irs_use = irs_use_range,
#   itn_future = itn_future_range,
#   irs_future = irs_future_range,
#   lsm = lsm_range,
#   stringsAsFactors = FALSE
# )

# # Save the generated scenarios
# write.csv(grid_scenarios, "grid_scenarios.csv", row.names = FALSE)
# write.csv(lhs_scenarios, "lhs_scenarios.csv", row.names = FALSE)

# Sample 500 entries from each scenario for plotting

library(dplyr)
library(tidyr)
library(lhs)
library(scales)
library(ggplot2)
library(GGally)

set.seed(123)

orderly2::orderly_dependency("bednet_param_gen", "latest()",
                             c("bednet_params_raw.RDS" = "bednet_params_raw.RDS"))

# Load bednet parameters from CSV
bednet_params_raw <- readRDS("bednet_params_raw.RDS")

# Define parameter ranges
eir_range <- sort(10^(runif(12, log10(0.5), log10(300)))) # Continuous on log scale

dn0_min_non_zero <- min(bednet_params_raw$dn0[bednet_params_raw$dn0 > 0]) * 0.9 # - 10% padding towards 0, non-zero cap
dn0_max <- max(bednet_params_raw$dn0) * 1.1 # + padding 10%
dn0_range <- sort(runif(10, dn0_min_non_zero, dn0_max)) # Continuous

anthropophagy_range <- c(70, 98) # Discrete (specific level)
biting_inbed_indoors_range <- c(70, 98) # Discrete (specific level)
seasonal_range <- c(0, 1) # Binary
itn_use_range <- c(seq(0, 1, by = 0.2), 0.05, 0.95) # Discrete (cap 1)
irs_use_range <- c(seq(0, 1, by = 0.2), 0.05, 0.95) # Discrete
itn_future_range <- seq(0, 1, length.out = 5) # Discrete
irs_future_range <- seq(0, 1, length.out = 5) # Discrete
lsm_range <- c(seq(0, 1, length.out = 5),0, 0.9) # Discrete cap (0.9)

# Collect all parameter ranges in a list for LHS
all_ranges <- list(eir = eir_range, dn0 = dn0_range, anthropophagy = anthropophagy_range,
                   biting_inbed_indoors = biting_inbed_indoors_range, seasonal = seasonal_range,
                   itn_use = itn_use_range, irs_use = irs_use_range, itn_future = itn_future_range,
                   irs_future = irs_future_range, lsm = lsm_range)

# Define total number of samples
total_samples <- 1000

# Calculate number of corner samples (10% of total - can vary)
num_corner_samples <- ceiling(0.1 * total_samples)

# Generate Corner Samples
corner_samples <- expand.grid(lapply(all_ranges, function(r) r))

# Take only required number of corner samples
corner_samples <- corner_samples[1:num_corner_samples, ]

# Generate LHS samples for remaining variables
# Number of LHS samples to make up 90% of the total
n_lhs <- total_samples - num_corner_samples
lhs_samples <- maximinLHS(n_lhs, length(all_ranges))
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

# Convert scaled samples to data frame
lhs_scenarios <- as.data.frame(scaled_lhs_samples)

# Combine LHS samples with corner samples
lhs_scenarios <- rbind(lhs_scenarios, corner_samples)

# Base expand grid scenario
grid_scenarios <- expand.grid(
  eir = eir_range,
  anthropophagy = anthropophagy_range,
  biting_inbed_indoors = biting_inbed_indoors_range,
  seasonal = seasonal_range,
  dn0 = dn0_range,
  itn_use = itn_use_range,
  irs_use = irs_use_range,
  itn_future = itn_future_range,
  irs_future = irs_future_range,
  lsm = lsm_range,
  stringsAsFactors = FALSE
)

# Write to output
write.csv(grid_scenarios, "grid_scenarios.csv", row.names = FALSE)
write.csv(lhs_scenarios, "lhs_scenarios.csv", row.names = FALSE)

# Write subset to output for debugging
grid_sample <- grid_scenarios %>% sample_n(1000)
lhs_sample <- lhs_scenarios %>% sample_n(1000)

write.csv(grid_sample, "grid_scenarios_sample.csv", row.names = FALSE)
write.csv(lhs_sample, "lhs_scenarios_sample.csv", row.names = FALSE)