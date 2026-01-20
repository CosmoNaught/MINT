## cluster setup
hipercow::hipercow_init(driver = "windows")
hipercow::hipercow_provision()
hipercow::hipercow_configuration()

############################ Launch bednet parameter collator ############################

id1 <- hipercow::task_create_expr(orderly2::orderly_run("collate_bednet_param"))
hipercow::task_log_watch(id1)

############################ Launch parameter space explorer ############################

# Local

id2 <- orderly2::orderly_run("param_sampling",
list(run = "long_run", output_grid = FALSE))

############################ Launch simulation setup ############################

## Minimal Test (Debug)
library(tibble)
library(dplyr)
param_index <- 7
reps <- 8

r <- hipercow::hipercow_rrq_controller()

  tid <- hipercow::task_create_expr({
          orderly2::orderly_run(
          "simulation_controller",
          list(
              run = "long_run",
              param_index = param_index,
              grid = TRUE,
              reps = reps,
              rrq = TRUE
          )
      )
  },
  parallel = hipercow::hipercow_parallel(use_rrq = TRUE),
  resources = hipercow::hipercow_resources(queue = "AllNodes")
  )

info <- hipercow::hipercow_rrq_workers_submit(64,
resources = hipercow::hipercow_resources(queue = "AllNodes"))
hipercow::task_log_watch(tid)

# Cluster Testing
library(tibble)
library(dplyr)
param_index <- 4#2^14
reps <- 2
making
r <- hipercow::hipercow_rrq_controller()

  tid <- hipercow::task_create_expr({
          orderly2::orderly_run(
          "simulation_controller",
          list(
              run = "long_run",
              param_index = param_index,
              reps = reps,
              grid = TRUE,
              rrq = TRUE
          )
      )
  },
  parallel = hipercow::hipercow_parallel(use_rrq = TRUE),
  resources = hipercow::hipercow_resources(queue = "Testing")
  )

info <- hipercow::hipercow_rrq_workers_submit(n = 4, 
resources = hipercow::hipercow_resources(queue = "Testing"))
hipercow::task_log_watch(tid)


# Cluster FullDeployment
library(tibble)
library(dplyr)
param_index <- 2^14
reps <- 8

r <- hipercow::hipercow_rrq_controller()

  tid <- hipercow::task_create_expr({
          orderly2::orderly_run(
          "simulation_controller",
          list(
              run = "long_run",
              param_index = param_index,
              reps = reps,
              grid = FALSE,
              rrq = TRUE
          )
      )
  },
  parallel = hipercow::hipercow_parallel(use_rrq = TRUE),
  resources = hipercow::hipercow_resources(queue = "AllNodes", requested_nodes = "wpia-009")
  )

info <- hipercow::hipercow_rrq_workers_submit(1)
hipercow::task_log_watch(tid)

####################################################################
# benchmark
    library(tibble) 
    library(dplyr) 

for(ii in 1:10) {
    param_index <- ii
    reps <- 8 
     
    r <- hipercow::hipercow_rrq_controller() 
     
      tid <- hipercow::task_create_expr({ 
              orderly2::orderly_run( 
              "simulation_controller", 
              list( 
                  run = "long_run", 
                  param_index = param_index, 
                  reps = reps, 
                  grid = FALSE, 
                  rrq = TRUE 
              ) 
          ) 
      }, 
      parallel = hipercow::hipercow_parallel(use_rrq = TRUE), 
      resources = hipercow::hipercow_resources(queue = "AllNodes") 
      ) 
     
    info <- hipercow::hipercow_rrq_workers_submit(1, 
    resources = hipercow::hipercow_resources(queue = )) 
    hipercow::task_log_watch(tid)  
  
}
