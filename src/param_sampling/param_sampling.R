library(dplyr)
library(tidyr)
library(lhs)
library(scales)
library(ggplot2)
library(GGally)

# Set a seed for reproducibility
set.seed(123)

# Define parameters for the 'orderly' workflow management system
orderly2::orderly_parameters(run = NULL, gen_grid = NULL)

# Determine sample sizes based on the 'run' parameter
if (run == "short_run") {
  total_samples <- 2^10          # For a short run: 2^10 = 1024
} else if (run == "long_run") {
  total_samples <- 2^14          # For a long run: 2^20 = 1,048,576
} else {
  # If 'run' parameter is invalid, stop execution with an error message
  stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

# Define the proportion for corner samples as 10% of total samples
corner_prop <- 0.1
num_corner_samples <- floor(corner_prop * total_samples)
subset_samples <- 128  # number of samples to subset for grid sampling (used when output_grid=TRUE)

orderly2::orderly_dependency("collate_bednet_param", "latest()", c("bednet_params_raw.RDS" = "bednet_params_raw.RDS"))

# Load bednet parameters from CSV
bednet_params_raw <- readRDS("bednet_params_raw.RDS")

# Define parameter ranges
eir_range <- sort(10^(runif(16, log10(0.5), log10(500))))
dn0_min_non_zero <- min(bednet_params_raw$dn0[bednet_params_raw$dn0 > 0]) * 0.9
dn0_max <- max(bednet_params_raw$dn0) * 1.2
dn0_range <- sort(runif(10, dn0_min_non_zero, dn0_max))
Q0_range <- c(0.6, 1)
phi_bednets_range <- c(0.4, 0.95)
seasonal_range <- c(0, 1)
routine_range <- c(0, 1)
itn_use_range <- seq(0, 1, by = 0.2)
irs_use_range <- seq(0, 1, by = 0.2)
itn_future_range <- seq(0, 1, length.out = 5)
irs_future_range <- seq(0, 1, length.out = 5)
lsm_range <- c(seq(0, 1, length.out = 5), 0, 0.9)

# Collect all parameter ranges in a list for LHS
all_ranges <- list(
  eir = eir_range, 
  dn0_use = dn0_range, 
  dn0_future = dn0_range, 
  Q0 = Q0_range,
  phi_bednets = phi_bednets_range, 
  seasonal = seasonal_range, 
  routine = routine_range,
  itn_use = itn_use_range, 
  irs_use = irs_use_range, 
  itn_future = itn_future_range,
  irs_future = irs_future_range, 
  lsm = lsm_range
)

# Generate corner samples by taking only min and max values of each parameter
corner_values <- lapply(all_ranges, function(r) {
  if (is.numeric(r)) {
    c(min(r), max(r))
  } else {
    # If discrete range, just take the first and last values
    c(r[1], r[length(r)])
  }
})

all_corners <- do.call(expand.grid, c(corner_values, stringsAsFactors = FALSE))

# If there are more corners than needed, take a subset, otherwise use all
if (nrow(all_corners) > num_corner_samples) {
  corner_samples <- all_corners[1:num_corner_samples, ]
} else {
  corner_samples <- all_corners
}

n_lhs <- total_samples - nrow(corner_samples)

# Generate LHS samples in one go
lhs_raw <- randomLHS(n_lhs, length(all_ranges))
colnames(lhs_raw) <- names(all_ranges)

# Scale the LHS samples to the desired ranges
scaled_lhs_samples <- sapply(names(all_ranges), function(var) {
  range_values <- unlist(all_ranges[[var]])
  values <- lhs_raw[, var]
  
  # Define which parameters should be treated as binary
  binary_params <- c("seasonal", "routine")
  
  if (var %in% binary_params) {
    # For binary variables, round to 0 or 1
    values <- round(values)
  } else if (var == "eir") {
    # For EIR, use log scale
    values <- 10^(scales::rescale(values, to = range(log10(range_values))))
  } else if (length(range_values) == 2 && all(range_values %in% c(0, 1))) {
    # For [0,1] continuous ranges, just rescale
    values <- scales::rescale(values, to = range(range_values))
  } else if (is.numeric(range_values)) {
    # For continuous variables, use regular scaling
    values <- scales::rescale(values, to = range(range_values))
  } else {
    # For discrete variables, map to discrete indices
    indices <- round(scales::rescale(values, to = c(1, length(range_values))))
    values <- range_values[indices]
  }
  return(values)
}, simplify = "data.frame")

lhs_scenarios <- as.data.frame(scaled_lhs_samples)

# Prepend corner samples so that they appear first
lhs_scenarios <- rbind(corner_samples, lhs_scenarios)

output_grid = FALSE
if (output_grid) {
  # Base expand grid scenario
  grid_scenarios <- expand.grid(
    eir = eir_range, Q0 = Q0_range,
    phi_bednets = phi_bednets_range, seasonal = seasonal_range, routine = routine_range,
    dn0_use = dn0_range, dn0_future = dn0_range, itn_use = itn_use_range, irs_use = irs_use_range,
    itn_future = itn_future_range, irs_future = irs_future_range,
    lsm = lsm_range, stringsAsFactors = FALSE
  )
  write.csv(grid_scenarios, "grid_scenarios.csv", row.names = FALSE)
  grid_sample <- grid_scenarios %>% sample_n(subset_samples)
  write.csv(grid_sample, "grid_scenarios_sample.csv", row.names = FALSE)
}

# Write out the full LHS scenarios (with corners prepended)
write.csv(lhs_scenarios, "lhs_scenarios.csv", row.names = FALSE)

# Write out a sample of the LHS scenarios for debugging
lhs_sample <- lhs_scenarios %>% sample_n(subset_samples)
write.csv(lhs_sample, "lhs_scenarios_sample.csv", row.names = FALSE)

# Ensure the 'figs' folder exists
if(!dir.exists("figs")) {
  dir.create("figs")
}

# Take a subset of 2^10 (1024) samples for plotting
plot_subset_size <- 2^10
if (nrow(lhs_scenarios) >= plot_subset_size) {
  lhs_scenarios_plot <- lhs_scenarios %>% sample_n(plot_subset_size)
} else {
  lhs_scenarios_plot <- lhs_scenarios
}

pdf("figs/param_distributions.pdf", onefile = TRUE)
for (param in names(lhs_scenarios_plot)) {
  # Set up the plotting area for two plots per page
  par(mfrow = c(2, 1))
  
  # 1. Histogram (Distribution) plot
  hist(lhs_scenarios_plot[[param]], 
       main = paste("Distribution of", param),
       xlab = param,
       col = "skyblue",
       border = "white")

  # 2. Scatter plot of parameter values vs. index with transparency
  plot(lhs_scenarios_plot[[param]], 
       main = paste("Scatter of", param, "values"),
       xlab = "Sample Index",
       ylab = param,
       pch = 19, 
       col = adjustcolor("darkblue", alpha.f = 0.05))

  # Plot corner samples in red
  points(1:nrow(corner_samples), corner_samples[[param]], col = adjustcolor("darkred", alpha.f = 0.05), pch = 19)
}
dev.off()