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

## Local
init_param_idx <- 1
parameter_set <- 2
reps <- 4 

simulation_prep  <- orderly2::orderly_run("simulation_controller",
list(run = "long_run",init_param_idx = init_param_idx,
                parameter_set = parameter_set,
                reps = reps,
                rrq = FALSE))
# Cluster
init_param_idx <- 1
parameter_set <- 2
reps <- 8

r <- hipercow::hipercow_rrq_controller()
    
launch_id <- hipercow::task_create_expr({
    orderly2::orderly_run(
        "simulation_controller",
        list(
            run = "long_run",
            init_param_idx = init_param_idx,
            parameter_set = parameter_set,
            reps = reps,
            rrq = TRUE
        )
    )
},
parallel = hipercow::hipercow_parallel(use_rrq = TRUE),
envvars = hipercow::hipercow_envvars(HIPERCOW_RRQ_QUEUE_ID = r$queue_id)
)

info <- hipercow::hipercow_rrq_workers_submit(4)

############################ Plotting Simulation ##################

orderly2::orderly_run(
        "simulation_plots"
    )
