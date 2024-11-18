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

# Set seed for reproducibility
set.seed(123)

# Constants
YEAR <- 365
SIM_LENGTH <- 12 * YEAR
HUMAN_POPULATION <- 100000

# Load dependencies
orderly2::orderly_parameters(run = NULL,
                             parameter_set = NULL,
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

output <- list()
task_ids <- list()

input <- get_runtime_parameters(
  parameter_set,
  lhs_data,
  HUMAN_POPULATION,
  bednet_params,
  SIM_LENGTH
)

parameter_set_output <- list()
parameter_set_output$input <- input
parameter_set_output$input$parameters <- NULL

if (rrq) {
  ids <- rrq::rrq_task_create_bulk_call(
    function(k, input) {
      
      result <- malariasimulation::run_simulation(
        input$timesteps,
        input$parameters
      )
      return(result)
    },
    seq_len(reps),
    args = list(input = input)
  )
  
  task_ids <- ids
  
} else {
  cl <- parallel::makeCluster(max(1, reps - 1))
  
  parallel::clusterEvalQ(cl, {
    library(malariasimulation)
  })
  
  parallel::clusterExport(cl, c("input"))
  
  task_fun <- function(k, input) {
    result <- malariasimulation::run_simulation(
      input$timesteps,
      input$parameters
    )
    return(list(result = result))
  }
  
  results <- parallel::parLapply(cl, seq_len(reps), task_fun, input = input)
  parallel::stopCluster(cl)
  
  for (j in seq_len(reps)) {
    parameter_set_output[[paste0("rep_", j)]] <- list(
      result = results[[j]]$result
    )
  }
  
  output <- parameter_set_output
}

if (rrq) {
  all_ids <- unlist(task_ids)
  
  rrq::rrq_task_wait(all_ids)
  all_results <- rrq::rrq_task_results(all_ids)
  
  parameter_set_output <- list()
  parameter_set_output$input <- input
  parameter_set_output$input$parameters <- NULL
  
  for (j in seq_len(reps)) {
    parameter_set_output[[paste0("rep_", j)]] <- list(
      result = all_results[[j]]
    )
  }
  
  output <- parameter_set_output
}

saveRDS(output, file = "simulation_results.rds")
