library(data.table)
library(parallel)
library(malariasimulation)
library(tibble)
library(spearMINT)
#library(arrow)
library(dplyr)

# Source additional scripts
source("set_inits.R")
source("set_species.R")
source("set_seasonality.R")
source("set_bednets.R")
source("set_irs.R")
source("set_lsm.R")
source("get_runtime_parameters.R")
source("execution_controller.R")
# Set seed for reproducibility
set.seed(123)

# Constants
YEAR <- 365
SIM_LENGTH <- 12 * YEAR
HUMAN_POPULATION <- 1000#00

# Load dependencies
orderly2::orderly_parameters(run = NULL,
                             param_index = NULL,
                             reps = NULL,
                             rrq = NULL)

if (!run %in% c("short_run", "long_run")) {
  stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

orderly2::orderly_dependency("collate_bednet_param", "latest()", 
                             c("bednet_params_raw.RDS" = "bednet_params_raw.RDS"))

bednet_params <- readRDS("bednet_params_raw.RDS")

orderly2::orderly_dependency("param_sampling", "latest(parameter:run == this:run)",
                             c("lhs_scenarios.csv" = "lhs_scenarios.csv"))

lhs_data <- data.table::fread("lhs_scenarios.csv")

input <- lapply(1:param_index, function(parameter_set) {
  get_runtime_parameters(
    parameter_set,
    lhs_data,
    HUMAN_POPULATION,
    bednet_params,
    SIM_LENGTH
  )
})

# List of vector of size reps of rrq task IDs
all_ids <- lapply(seq_along(input), function(i) {
  rrq_malariasim_controller(input[[i]], reps)
})

for (i in seq_along(all_ids)) {
  # Collect results
  rrq::rrq_task_wait(all_ids[[i]])
  results <- rrq::rrq_task_results(all_ids[[i]])
  
  parameter_set_output <- list(input = input[[i]], outputs = results)
  parameter_set_output$input$parameters <- NULL

  # Save results
  saveRDS(parameter_set_output, file = sprintf("simulation_results_%d.rds", i))
}