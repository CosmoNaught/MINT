# Function to set IRS parameters and return timesteps
set_irs_parameters <- function(simparams, lhs_sample) {
  peak <- peak_season_offset(simparams)
  month <- 30

  peak_season_offset_override <- TRUE

  if (peak_season_offset_override){
    peak_season_time <- 0
  } else {
    peak_season_time <- peak - 3 * month
  }
  sprayingtimesteps <- seq(0,12) * YEAR + peak_season_time
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

  list(simparams = simparams, timesteps = sprayingtimesteps)
}