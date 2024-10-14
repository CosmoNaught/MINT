library(data.table)
library(parallel)
library(malariasimulation)
library(tibble)
library(spearMINT)

# Source additional scripts
source("set_inits.R")
source("set_species.R")
source("set_seasonality.R")
source("set_bednets.R")
source("set_irs.R")
source("set_lsm.R")
source("generate_parameters.R")

# Set seed for reproducibility
set.seed(123)

# Constants
YEAR <- 365
SIM_LENGTH <- 12 * YEAR
HUMAN_POPULATION <- 100000

# Load dependencies
orderly2::orderly_parameters(run = NULL, i = NULL)

if (!run %in% c("short_run", "long_run")) {
  stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

orderly2::orderly_dependency("collate_bednet_param", "latest()", 
                             c("bednet_params_raw.RDS" = "bednet_params_raw.RDS"))

bednet_params <- readRDS("bednet_params_raw.RDS")

orderly2::orderly_dependency("param_sampling", "latest(parameter:run == this:run)",
                             c("lhs_scenarios.csv" = "lhs_scenarios.csv"))

# Load the entire dataset before parallel processing

lhs_data <- data.table::fread("lhs_scenarios.csv")

# Call the function
input_entry <- generate_parameters(
                                   i,
                                   lhs_data,
                                   HUMAN_POPULATION,
                                   bednet_params,
                                   SIM_LENGTH)

# Save the result outside the function
saveRDS(input_entry, file = "input.RDS")