## cluster setup
hipercow::hipercow_init(driver = "dide-windows")
hipercow::hipercow_provision()
hipercow::hipercow_configuration()

############################ Launch bednet parameter collator ############################

id1 <- hipercow::task_create_expr(orderly::orderly_run("collate_bednet_param"))
hipercow::task_log_watch(id1)

############################ Launch parameter space explorer ############################

# Local

id2 <- orderly::orderly_run("param_sampling",
list(run = "long_run"))

############################ Launch simulation setup ############################

## Minimal Test (Debug)
library(tibble)
library(dplyr)
param_index <- 7
reps <- 8

r <- hipercow::hipercow_rrq_controller()

  tid <- hipercow::task_create_expr({
          orderly::orderly_run(
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

# Cluster
library(tibble)
library(dplyr)
param_index <- 2^12
reps <- 4

r <- hipercow::hipercow_rrq_controller(driver = "dide-windows")

tid <- hipercow::task_create_expr({
        orderly::orderly_run(
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
print(tid)
info <- hipercow::hipercow_rrq_workers_submit(n = 512, 
resources = hipercow::hipercow_resources(queue = "AllNodes"))
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
              orderly::orderly_run( 
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
