## cluster setup
hipercow::hipercow_init(driver = "windows")
hipercow::hipercow_provision()
hipercow::hipercow_configuration()

############################ Launch bednet parameter collator ############################

id1 <- hipercow::task_create_expr(orderly2::orderly_run("collate_bednet_param"))
hipercow::task_log_watch(id1)

############################ Launch parameter space explorer ############################

id2 <- hipercow::task_create_expr(orderly2::orderly_run("param_sampling", 
list(run = "long_run",
local_cluster = FALSE)),
resources = hipercow::hipercow_resources(cores = 32))
hipercow::task_log_watch(id2)

############################ Plot parameter space ############################

# DISABLED FOR NOW TO BE FIXED LATER
# id3 <- hipercow::task_create_expr(orderly2::orderly_run("param_sampling_plots", list(run = "short_run")))

############################ Launch simulation setup ############################

library(tibble)
library(dplyr)

## Local
parameter_set <- 2
reps <- 2
task_id <- tibble()

for (i in seq(1, parameter_set)){
    simulation_prep  <- orderly2::orderly_run("simulation_controller",
    list(run = "long_run",
                    parameter_set = parameter_set,
                    reps = reps,
                    rrq = FALSE))
    temp_tibble <- tibble(parameter_set = i, output = simulation_prep)
    task_id <- bind_rows(task_id, temp_tibble)
}

# Cluster
library(tibble)
library(dplyr)
parameter_set <- 1
reps <- 8

r <- hipercow::hipercow_rrq_controller()

for (i in seq(1, parameter_set)){
    hipercow::task_create_expr({
            orderly2::orderly_run(
            "simulation_controller",
            list(
                run = "long_run",
                parameter_set = parameter_set,
                reps = reps,
                rrq = TRUE
            )
        )
    },
    parallel = hipercow::hipercow_parallel(use_rrq = TRUE)
    )
}

info <- hipercow::hipercow_rrq_workers_submit(8)

############################ Plotting Simulation ##################

orderly2::orderly_run(
        "simulation_plots"
    )


####################################################################

load_simulation_results <- function(task_id) {
  base_dir <- "/home/ye120/net/malaria/Cosmo/MINT/archive/simulation_controller"
  file_path <- file.path(base_dir, task_id, "simulation_results.rds")
  
  if (!file.exists(file_path)) {
    stop("The specified task ID does not exist or the file could not be found: ", file_path)
  }
  
  simulation_results <- readRDS(file_path)
  
  return(simulation_results)
}

###################################################################

 library(tibble)
 
    # Example input data
    input_data <- tst$input
 
    # Function to dynamically collapse nested lists into a single tibble row
    collapse_to_tibble <- function(data) {
      data$treatment_timesteps$mass_bednet <- list(data$treatment_timesteps$mass_bednet)
      data$treatment_timesteps$irs <- list(data$treatment_timesteps$irs)
      
      flattened <- unlist(data, recursive = FALSE)
      names(flattened) <- gsub("\\.", "_", names(flattened))
      as_tibble(flattened)
    }
 
    # Apply function to input data
    result <- collapse_to_tibble(input_data)
 
    # Print the resulting tibble
    print(result)