## cluster setup
hipercow::hipercow_init(driver = "windows")
hipercow::hipercow_provision()
hipercow::hipercow_configuration()

############################ Launch bednet parameter collator ############################

id1 <- hipercow::task_create_expr(orderly2::orderly_run("collate_bednet_param"))

############################ Launch parameter space explorer ############################

id2 <- hipercow::task_create_expr(orderly2::orderly_run("param_sampling", 
list(run = "long_run",
local_cluster = FALSE)),
parallel = hipercow::hipercow_parallel("parallel"),
resources = hipercow::hipercow_resources(cores = 32))


############################ Plot parameter space ############################

# DISABLED FOR NOW TO BE FIXED LATER
# id3 <- hipercow::task_create_expr(orderly2::orderly_run("param_sampling_plots", list(run = "short_run")))

############################ Launch simulation setup ############################

## Local
simulation_prep  <- orderly2::orderly_run("simulation_prep",
list(run = "long_run", 
chunk_size = 1000,
subset_override = 10000),
echo = FALSE)

## Cluster
id4 <- hipercow::task_create_expr(
    orderly2::orderly_run("simulation_prep",
    list(run = "long_run", 
    chunk_size = 1000,
    subset_override = 10000),
    echo = FALSE),
    parallel = parallel,
    resources = resources
)

############################ Launch simulations ############################

## Local
parameter_set_indices <- 10000

for (i in parameter_set_indices) {
  simulation_launch <- orderly2::orderly_run(
    "simulation_launch",
    list(
      run = "long_run",
      parameter_set = i,
      repetitions = 5,
      parallelism = TRUE,
      workers_override = FALSE,
      plot = TRUE
    ),
    echo = FALSE
  )
}

## Cluster
parameter_set_indices <- 1:10000
for (i in parameter_set_indices) {
    hipercow::task_create_expr(
        orderly2::orderly_run(
        "simulation_launch",
        list(
        run = "long_run",
        parameter_set = i,
        repetitions = 5,
        parallelism = TRUE,
        workers_override = FALSE,
        plot = TRUE
        ),
        echo = FALSE
    ),
        #parallel = parallel,
        resources = hipercow::hipercow_resources(cores = 5)
    )
}

############################ Gather simulation IDs ############################

parameter_set_indices <- 1:2
parameter_set_indices <- paste(parameter_set_indices, collapse = ",")

id6 <- hipercow::task_create_expr(
    orderly2::orderly_run(
        "pre_collate",
        list(
            indices = parameter_set_indices,
            verbose = TRUE,
            parallel = TRUE,
            store_output = TRUE
        ),
        echo = FALSE
    ),
    resources = hipercow::hipercow_resources(cores = 2)
)

hipercow::task_log_watch(id6)
