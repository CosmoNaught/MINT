# Function to set LSM parameters and return timesteps
set_lsm_parameters <- function(simparams, lhs_sample) {
    lsm_coverage <- lhs_sample$lsm
    cc <- get_init_carrying_capacity(simparams)
    lsmtimesteps <- 9 * YEAR
    
    simparams <- simparams |> set_carrying_capacity(
      carrying_capacity = matrix(cc * (1 - lsm_coverage), ncol = 1),
      timesteps = lsmtimesteps
    )
  
  list(simparams = simparams, timesteps = lsmtimesteps)
}