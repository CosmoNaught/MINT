# Load necessary libraries
library(malariasimulation)
library(tibble)

# Source additional scripts
source("debug_plot.R")
source("resources.R")

# Set seed for reproducibility
set.seed(123)

# Constants
YEAR <- 365
SIM_LENGTH <- 6 * YEAR
HUMAN_POPULATION <- 1000

# Load dependencies
orderly2::orderly_parameters(run = NULL,
num_sample = NULL,
run_control = NULL,
plot = NULL)

if(!run %in% c("short_run", "long_run")) {
  stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

orderly2::orderly_dependency("bednet_param_gen", "latest()", 
                              c("bednet_params.csv" = "bednet_params.csv"))
orderly2::orderly_dependency("param_sampling", "latest(parameter:run == this:run)",
                              c("lhs_scenarios_sample.csv" = "lhs_scenarios_sample.csv"))


bednet_params <- read.csv("bednet_params.csv")
lhs_samples <- read.csv("lhs_scenarios_sample.csv")
lhs_sample <- lhs_samples[num_sample, ]

selected_seasonality <- set_seasonality(lhs_sample)
simparams <- initialize_simulation_parameters(lhs_sample, selected_seasonality)

baseline_simparams <- simparams
treatment_simparams <- simparams

# Initialize parameters and lists to store timesteps
bednet_baseline_result <- set_bednet_parameters(baseline_simparams, lhs_sample, bednet_params, baseline = TRUE)
baseline_simparams <- bednet_baseline_result$simparams
bednet_baseline_timesteps <- bednet_baseline_result$timesteps

bednet_treatment_result <- set_bednet_parameters(treatment_simparams, lhs_sample, bednet_params, baseline = FALSE)
treatment_simparams <- bednet_treatment_result$simparams
bednet_treatment_timesteps <- bednet_treatment_result$timesteps

irs_baseline_result <- set_irs_parameters(baseline_simparams, lhs_sample, baseline = TRUE)
baseline_simparams <- irs_baseline_result$simparams
irs_baseline_timesteps <- irs_baseline_result$timesteps

irs_treatment_result <- set_irs_parameters(treatment_simparams, lhs_sample, baseline = FALSE)
treatment_simparams <- irs_treatment_result$simparams
irs_treatment_timesteps <- irs_treatment_result$timesteps

lsm_baseline_result <- set_lsm_parameters(baseline_simparams, lhs_sample, baseline = TRUE)
baseline_simparams <- lsm_baseline_result$simparams
lsm_baseline_timesteps <- lsm_baseline_result$timesteps

lsm_treatment_result <- set_lsm_parameters(treatment_simparams, lhs_sample, baseline = FALSE)
treatment_simparams <- lsm_treatment_result$simparams
lsm_treatment_timesteps <- lsm_treatment_result$timesteps

unique_bednet_timesteps <- unique(c(bednet_baseline_timesteps, bednet_treatment_timesteps))
unique_irs_timesteps <- unique(c(irs_baseline_timesteps, irs_treatment_timesteps))
unique_lsm_timesteps <- unique(c(lsm_baseline_timesteps, lsm_treatment_timesteps))

output <- list()

# Create a list object called 'timesteps' with unique timesteps for each treatment
output$treatment_timesteps <- list(
  bednet = unique_bednet_timesteps,
  irs = unique_irs_timesteps,
  lsm = unique_lsm_timesteps
)

if (run_control) {
    control_simulation_results <- run_control_sim(SIM_LENGTH, baseline_simparams)
    output$control_simulation_results <- control_simulation_results
}

treatment_simulation_results <- run_treatment_sim(SIM_LENGTH, treatment_simparams)
output$treatment_simulation_results <- treatment_simulation_results

if (plot) {
  plot_simulations(output = output$treatment_simulation_results,
  output_control =  output$control_simulation_results,
  timesteps = output$treatment_timesteps)
}

output$lhs_sample <- lhs_sample

saveRDS(output, "output.RDS")