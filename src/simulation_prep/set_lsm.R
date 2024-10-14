# Function to set LSM parameters and return timesteps
set_lsm_parameters <- function(simparams, lhs_sample) {
    lsm_coverage <- lhs_sample$lsm
    lsmtimesteps <- 9 * YEAR
    
    simparams <- simparams |> set_carrying_capacity(
      carrying_capacity_scalers = matrix((1 - lsm_coverage), ncol = 1),
      timesteps = lsmtimesteps
    )
  
  list(simparams = simparams, timesteps = lsmtimesteps)
}