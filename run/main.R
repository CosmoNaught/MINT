## non-cluster setup
## can't load paramter specific dependency
devtools::install_local("~/Documents/Imperial/PhD/Projects/spearMINT", force = TRUE)
devtools::load_all("~/Documents/Imperial/PhD/Projects/spearMINT")

t0 <- Sys.time()
# id <- orderly2::orderly_run("bednet_param_gen")
id <- orderly2::orderly_run("collate_bednet_param")
id <- orderly2::orderly_run("param_sampling", list(run = "short_run"))
id <- orderly2::orderly_run("param_sampling_plots", list(run = "short_run"))
id <- orderly2::orderly_run("simulation_prep", list(run = "short_run", num_sample = 69, run_control = TRUE, plot = TRUE))
print(Sys.time() - t0)

## Main functions for execution can be found here
## See cluster.R for running on cluster
## cluster-integration