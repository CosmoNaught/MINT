# Function to set LSM parameters and return timesteps
set_lsm_parameters <- function(simparams, lhs_sample, baseline = TRUE) {
  if (baseline) {
    lsm_coverage <- 0
    cc <- get_init_carrying_capacity(simparams)
    lsmtimesteps <- c(1e-9) * YEAR
    
    simparams <- simparams |> set_carrying_capacity(
      carrying_capacity = matrix(cc * (1 - lsm_coverage), ncol = 1),
      timesteps = lsmtimesteps
    )
  } else {
    lsm_coverage <- lhs_sample$lsm
    cc <- get_init_carrying_capacity(simparams)
    lsmtimesteps <- 9 * YEAR
    
    simparams <- simparams |> set_carrying_capacity(
      carrying_capacity = matrix(cc * (1 - lsm_coverage), ncol = 1),
      timesteps = lsmtimesteps
    )
  }
  
  list(simparams = simparams, timesteps = lsmtimesteps)
}