# Function to set IRS parameters and return timesteps
set_irs_parameters <- function(simparams, lhs_sample, baseline = TRUE) {
  peak <- peak_season_offset(simparams)
  month <- 30
  
  if (baseline) {
    sprayingtimesteps <- seq(0, 8) * YEAR + peak - 3 * month
    simparams <- set_spraying(
      simparams,
      timesteps = sprayingtimesteps,
      coverages = c(rep(lhs_sample$irs_use, length(sprayingtimesteps))),
      ls_theta = matrix(2.025, nrow = length(sprayingtimesteps), ncol = 1),
      ls_gamma = matrix(-0.009, nrow = length(sprayingtimesteps), ncol = 1),
      ks_theta = matrix(-2.222, nrow = length(sprayingtimesteps), ncol = 1),
      ks_gamma = matrix(0.008, nrow = length(sprayingtimesteps), ncol = 1),
      ms_theta = matrix(-1.232, nrow = length(sprayingtimesteps), ncol = 1),
      ms_gamma = matrix(-0.009, nrow = length(sprayingtimesteps), ncol = 1)
    )
  } else {
    sprayingtimesteps <- seq(0,12) * YEAR + peak - 3 * month
    simparams <- set_spraying(
      simparams,
      timesteps = sprayingtimesteps,
      coverages = c(rep(lhs_sample$irs_use, 9), rep(lhs_sample$irs_future, 4)),
      ls_theta = matrix(2.025, nrow = length(sprayingtimesteps), ncol = 1),
      ls_gamma = matrix(-0.009, nrow = length(sprayingtimesteps), ncol = 1),
      ks_theta = matrix(-2.222, nrow = length(sprayingtimesteps), ncol = 1),
      ks_gamma = matrix(0.008, nrow = length(sprayingtimesteps), ncol = 1),
      ms_theta = matrix(-1.232, nrow = length(sprayingtimesteps), ncol = 1),
      ms_gamma = matrix(-0.009, nrow = length(sprayingtimesteps), ncol = 1)
    )
  }
  
  list(simparams = simparams, timesteps = sprayingtimesteps)
}