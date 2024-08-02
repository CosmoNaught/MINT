## non-cluster setup
## can't load paramter specific dependency
devtools::install_local("/home/ye120/net/malaria/Cosmo/spearMINT", force = TRUE)

id <- orderly2::orderly_run("collate_bednet_param")
id <- orderly2::orderly_run("param_sampling", list(run = "long_run", local_cluster = TRUE))

# DISABLED FOR NOW TO BE FIXED LATER
#id <- orderly2::orderly_run("param_sampling_plots", list(run = "short_run"))

id <- orderly2::orderly_run("simulation_prep", list(run = "long_run", num_sample = 7008, run_control = FALSE, plot = TRUE))


## Main functions for execution can be found here
## See cluster.R for running on cluster
## cluster-integration