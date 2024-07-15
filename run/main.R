## non-cluster setup
## can't load paramter specific dependency
t0 <- Sys.time()
id <- orderly2::orderly_run("bednet_param_gen")
id <- orderly2::orderly_run("param_sampling", list(run = "short_run"))
id <- orderly2::orderly_run("param_sampling_plots", list(run = "short_run"))
id <- orderly2::orderly_run("simulation_prep", list(run = "short_run", num_sample = 5))
print(Sys.time() - t0)

## Main functions for execution can be found here
## See cluster.R for running on cluster


tst1 <- lhs_samples[4,]

lhs_samples2 <- lhs_samples[4,]
lhs_samples2$seasonal <- 1
tst2 <- lhs_samples2

tst3 <- lhs_samples[64, ]

tst1_tibble <- as_tibble(tst1)
tst2_tibble <- as_tibble(tst2)
tst3_tibble <- as_tibble(tst3)

combined_tibble <- dplyr::bind_rows(tst1_tibble, tst2_tibble, tst3_tibble)
combined_tibble


