## non-cluster setup

t0 <- Sys.time()
id <- orderly2::orderly_run("bednet_param_gen")
id <- orderly2::orderly_run("param_sampling")
id <- orderly2::orderly_run("param_sampling_plots")
print(Sys.time() - t0)
## Main functions for execution can be found here
## See cluster.R for running on cluster
