## cluster setup
hipercow::hipercow_init(driver = "windows")
hipercow::hipercow_provision()
hipercow::hipercow_configuration()

id1 <- hipercow::task_create_expr(orderly2::orderly_run("collate_bednet_param"))

id2 <- hipercow::task_create_expr(orderly2::orderly_run("param_sampling", 
list(run = "long_run",
local_cluster = FALSE)),
parallel = hipercow::hipercow_parallel("parallel"),
resources = hipercow::hipercow_resources(cores = 32))

# DISABLED FOR NOW TO BE FIXED LATER
# id3 <- hipercow::task_create_expr(orderly2::orderly_run("param_sampling_plots", list(run = "short_run")))

parallel <- hipercow::hipercow_parallel("parallel")
resources <- hipercow::hipercow_resources(cores = 5)

id4 <- hipercow::task_create_expr(orderly2::orderly_run("simulation_prep",
list(run = "long_run", 
num_sample = 7008, 
run_control = FALSE,
repetitions = 5, 
parallel = TRUE,
plot = TRUE)),
parallel = parallel,
resources = resources)