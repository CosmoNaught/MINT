# Load necessary libraries
library(malariasimulation)
library(tibble)
library(spearMINT)
# devtools::load_all("/home/ye120/net/malaria/Cosmo/spearMINT")

# Source additional scripts
source("set_inits.R")
source("set_species.R")
source("set_seasonality.R")
source("set_bednets.R")
source("set_irs.R")
source("set_lsm.R")
source("launch_sim.R")
source("debug_plot.R")

# Set seed for reproducibility
set.seed(123)

# Constants
YEAR <- 365

SIM_LENGTH <- 12 * YEAR
HUMAN_POPULATION <- 100000

# Load dependencies
orderly2::orderly_parameters(run = NULL,
    num_sample = NULL,
    repetitions = NULL,
    parallel = NULL,
    workers_override = NULL,
    plot = NULL)

if(!run %in% c("short_run", "long_run")) {
  stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

orderly2::orderly_dependency("collate_bednet_param", "latest()", 
                              c("bednet_params_raw.RDS" = "bednet_params_raw.RDS"))

orderly2::orderly_dependency("param_sampling", "latest(parameter:run == this:run)",
                              c("lhs_scenarios.csv" = "lhs_scenarios.csv"))

bednet_params <- readRDS("bednet_params_raw.RDS")
#lhs_samples <- read.csv("lhs_scenarios.csv")

# Load the first 100k rows
chunk_size <- 100000
data_chunk <- data.table::fread("lhs_scenarios.csv", nrows = chunk_size)

lhs_sample <- data_chunk[num_sample, ]

selected_seasonality <- set_seasonality(lhs_sample)
simparams <- initialize_simulation_parameters(lhs_sample, HUMAN_POPULATION, selected_seasonality)

treatment_simparams <- simparams

# Initialize parameters and lists to store timesteps

bednet_treatment_result <- set_bednet_parameters(treatment_simparams, lhs_sample, bednet_params)
treatment_simparams <- bednet_treatment_result$simparams
bednet_treatment_timesteps <- bednet_treatment_result$timesteps

irs_treatment_result <- set_irs_parameters(treatment_simparams, lhs_sample)
treatment_simparams <- irs_treatment_result$simparams
irs_treatment_timesteps <- irs_treatment_result$timesteps

lsm_treatment_result <- set_lsm_parameters(treatment_simparams, lhs_sample)
treatment_simparams <- lsm_treatment_result$simparams
lsm_treatment_timesteps <- lsm_treatment_result$timesteps

unique_bednet_timesteps <- unique(bednet_treatment_timesteps)
unique_irs_timesteps <- unique(irs_treatment_timesteps)
unique_lsm_timesteps <- unique(lsm_treatment_timesteps)

output <- list()

# Create a list object called 'timesteps' with unique timesteps for each treatment
output$treatment_timesteps <- list(
  mass_bednet = c(0, 3, 6, 9) * 365,
  irs = unique_irs_timesteps,
  lsm = unique_lsm_timesteps
)

treatment_simulation_results <- run_sim_with_reps(
    timesteps = SIM_LENGTH, 
    parameters = treatment_simparams, 
    repetitions = repetitions, 
    parallel = parallel, 
    workers_override = workers_override
)

output$treatment_simulation_results <- treatment_simulation_results

if (plot) {
  plot_simulations(output = output$treatment_simulation_results,
  timesteps = output$treatment_timesteps)
}

output$lhs_sample <- lhs_sample

saveRDS(output, "output.RDS")