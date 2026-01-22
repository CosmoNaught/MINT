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
HUMAN_POPULATION <- 100000

# Load dependencies
orderly::orderly_parameters(run = NULL,
                             param_index = NULL,
                             reps = NULL,
                             grid = NULL,
                             rrq = NULL)

if (!run %in% c("short_run", "long_run")) {
  stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

orderly::orderly_dependency("collate_bednet_param", "latest()", 
                             c("bednet_params_raw.RDS" = "bednet_params_raw.RDS"))

bednet_params <- readRDS("bednet_params_raw.RDS")

if (grid) {
  print("NOTE: parameters will be chosen according to grid sampling")
  orderly::orderly_dependency("param_sampling", "latest(parameter:run == this:run)",
                             c("grid_scenarios.csv" = "grid_scenarios.csv"))
  lhs_data <- data.table::fread("grid_scenarios.csv")
} else {
    print("NOTE: parameters will be chosen according to LHS sampling")
  orderly::orderly_dependency("param_sampling", "latest(parameter:run == this:run)",
                             c("lhs_scenarios.csv" = "lhs_scenarios.csv"))
  lhs_data <- data.table::fread("lhs_scenarios.csv")
}


## Set timeout 

bar_id <- cli::cli_progress_bar("Generating parameters", total=param_index)
input <- lapply(1:param_index, function(parameter_set) {
  params <- get_runtime_parameters(
    parameter_set,
    lhs_data,
    HUMAN_POPULATION,
    bednet_params,
    SIM_LENGTH
  )
  cli::cli_progress_update(inc = 1, id = bar_id)
  params
})
cli::cli_progress_done(bar_id)

# List of vector of size reps of rrq task IDs
bar_id <- cli::cli_progress_bar("Submitting tasks", total=param_index)
all_ids <- lapply(seq_along(input), function(i) {
  id <- rrq_malariasim_controller(input[[i]], reps)
  cli::cli_progress_update(inc = 1, id = bar_id)
  id
})
cli::cli_progress_done(bar_id)

saveRDS(all_ids, "all_ids")

cli::cli_progress_bar("Saving results", total=param_index)
failed <- data.frame(parameter_set=numeric(0), rep=numeric(0))
for (i in seq_along(all_ids)) {
  # Collect results
  rrq::rrq_task_wait(all_ids[[i]])
  results <- rrq::rrq_task_results(all_ids[[i]])

  # result: list of length reps, where each entry is either data.frame or a rrq_task_error
  for (j in seq_along(results)) {
    if (inherits(results[[j]], "rrq_task_error")) {
      cli::cli_alert_warning("Parameter set {i} rep {j} failed")
      failed <- rbind(failed, list(parameter_set=i, rep=j))
    }
  }
  
  parameter_set_output <- list(input = input[[i]], outputs = results)
  parameter_set_output$input$parameters <- NULL

  # Save results
  saveRDS(parameter_set_output, file = sprintf("simulation_results_%d.rds", i))
  cli::cli_progress_update(inc = 1)
}
cli::cli_progress_done()

# return empty if all successfully executed
write.csv2(failed, "failed.csv", row.names = FALSE)