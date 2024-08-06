# Load necessary libraries
library(malariasimulation)
library(tibble)
library(spearMINT)

source("launch_support.R")
source("debug_plot.R")

# Load dependencies
orderly2::orderly_parameters(run = NULL,
    parameter_set = NULL,
    repetitions = NULL,
    parallelism = NULL,
    workers_override = NULL,
    plot = NULL)

if(!run %in% c("short_run", "long_run")) {
  stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

orderly2::orderly_dependency("simulation_prep", "latest()", 
                              c("input.RDS" = "input.RDS"))

input <- readRDS("input.RDS")

input <- input[[parameter_set]]

output <- list()

treatment_simulation_results <- run_sim_with_reps(
    timesteps = input$timesteps, 
    parameters = input$parameters, 
    repetitions = repetitions, 
    parallelism = parallelism, 
    workers_override = workers_override
)

output$treatment_simulation_results <- treatment_simulation_results

if (plot) {
  plot_simulations(output = output$treatment_simulation_results,
  timesteps = input$treatment_timesteps)
}

output$lhs_sample <- input$MINT_parameters

saveRDS(output, "output.RDS")