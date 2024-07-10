## Load data from either a long or short run

set.seed(123)

orderly2::orderly_parameters(run = NULL)

if(!run %in% c("short_run", "long_run")) {
    stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

orderly2::orderly_dependency("param_sampling", "latest(parameter:run == this:run)",
                             c("lhs_scenarios_sample.csv" = "lhs_scenarios_sample.csv",
                             "grid_scenarios_sample.csv" = "grid_scenarios_sample.csv"))

## process data for malariasim