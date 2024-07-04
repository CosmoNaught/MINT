## cluster setup
hipercow::hipercow_init(driver = "windows")
hipercow::hipercow_provision()
hipercow::hipercow_configuration()

task <- hipercow::task_create_expr(orderly2::orderly_run(
    'bednet_param_gen',
    resources = hipercow::hipercow_resources(cores = 2)))

hipercow::task_log_watch(task)

task <- hipercow::task_create_expr(orderly2::orderly_run(
    'param_sampling',
    resources = hipercow::hipercow_resources(cores = 32)))

hipercow::task_log_watch(task)