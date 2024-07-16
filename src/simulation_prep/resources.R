# Function to set seasonality
set_seasonality <- function(lhs_sample) {
  seasonal <- tibble(seasonality = list(c(0.285505, -0.325352, -0.0109352, 0.0779865, -0.132815, 0.104675, -0.013919)), seas_name = 'seasonal')
  perennial <- tibble(seasonality = list(c(0.2852770, -0.0248801, -0.0529426, -0.0168910, -0.0216681, -0.0242904, -0.0073646)), seas_name = 'perennial')
  
  if (lhs_sample$seasonal == 1) {
    selected_seasonality <- seasonal$seasonality[[1]]
  } else {
    selected_seasonality <- perennial$seasonality[[1]]
  }
  
  list(g0 = selected_seasonality[1], g = selected_seasonality[2:4], h = selected_seasonality[5:7])
}

# Function to set mosquito parameters
set_mosquito_parameters <- function(lhs_sample) {
  amphiphilic_mosquito_params <- fun_params
  amphiphilic_mosquito_params$phi_indoors <- lhs_sample$biting_inbed_indoors / 100
  amphiphilic_mosquito_params$Q0 <- lhs_sample$anthropophagy / 100
  amphiphilic_mosquito_params$species <- 'amphiphilic'
  
  amphiphilic_mosquito_params
}

# Function to initialize simulation parameters
initialize_simulation_parameters <- function(lhs_sample, selected_seasonality) {
  simparams <- get_parameters(
    list(
      human_population = HUMAN_POPULATION,
      model_seasonality = TRUE,
      g0 = selected_seasonality$g0,
      g = selected_seasonality$g,
      h = selected_seasonality$h
    )
  )
  
  amphiphilic_mosquito_params <- set_mosquito_parameters(lhs_sample)
  simparams <- set_species(
    simparams, 
    species = list(amphiphilic_mosquito_params),
    proportions = c(1)
  )
  
  simparams <- set_equilibrium(
    parameters = simparams,
    init_EIR = lhs_sample$eir
  )
  
  simparams
}

# Function to set bednet parameters and return timesteps
set_bednet_parameters <- function(simparams, lhs_sample, bednet_params, baseline = TRUE) {
  target_dn0 <- round(lhs_sample$dn0, digits = 3)
  differences <- abs(bednet_params$dn0 - target_dn0)
  closest_index <- which.min(differences)
  selected_net_params <- bednet_params[closest_index,]
  
  if (baseline) {
    bednetstimesteps <- 0
    simparams <- set_bednets(
      simparams,
      timesteps = bednetstimesteps,
      coverages = lhs_sample$itn_use,
      retention = 5 * YEAR,
      dn0 = matrix(c(lhs_sample$dn0), nrow = length(bednetstimesteps), ncol = 1),
      rn = matrix(c(selected_net_params$rn0), nrow = length(bednetstimesteps), ncol = 1),
      rnm = matrix(c(.24), nrow = length(bednetstimesteps), ncol = 1),
      gamman = rep(selected_net_params$gamman * 365, length(bednetstimesteps))
    )
  } else {
    bednetstimesteps <- c(0, 3) * YEAR
    simparams <- set_bednets(
      simparams,
      timesteps = bednetstimesteps,
      coverages = c(lhs_sample$itn_use, lhs_sample$itn_future),
      retention = 5 * YEAR,
      dn0 = matrix(c(lhs_sample$dn0), nrow = length(bednetstimesteps), ncol = 1),
      rn = matrix(c(selected_net_params$rn0), nrow = length(bednetstimesteps), ncol = 1),
      rnm = matrix(c(.24), nrow = length(bednetstimesteps), ncol = 1),
      gamman = rep(selected_net_params$gamman * 365, length(bednetstimesteps))
    )
  }
  
  list(simparams = simparams, timesteps = bednetstimesteps)
}

# Function to set IRS parameters and return timesteps
set_irs_parameters <- function(simparams, lhs_sample, baseline = TRUE) {
  peak <- peak_season_offset(simparams)
  month <- 30
  
  if (baseline) {
    sprayingtimesteps <- c(0, 1, 2) * YEAR + peak - 3 * month
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
    sprayingtimesteps <- c(0, 1, 2, 3, 4, 5, 6) * YEAR + peak - 3 * month
    simparams <- set_spraying(
      simparams,
      timesteps = sprayingtimesteps,
      coverages = c(rep(lhs_sample$irs_use, 3), rep(lhs_sample$irs_future, 4)),
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
    lsmtimesteps <- c(3) * YEAR
    
    simparams <- simparams |> set_carrying_capacity(
      carrying_capacity = matrix(cc * (1 - lsm_coverage), ncol = 1),
      timesteps = lsmtimesteps
    )
  }
  
  list(simparams = simparams, timesteps = lsmtimesteps)
}

# Function to run simulations and plot results
run_simulations <- function(sim_length, baseline_simparams, treatment_simparams) {
  output_control <- run_simulation(timesteps = sim_length, parameters = baseline_simparams)
  output <- run_simulation(timesteps = sim_length, parameters = treatment_simparams)
  return(list(output = output, output_control = output_control))
}


run_control_sim <- function(sim_length, baseline_simparams) {
    output_control <- run_simulation(timesteps = sim_length, parameters = baseline_simparams)
}

run_treatment_sim <- function(sim_length, treatment_simparams) {
    output <- run_simulation(timesteps = sim_length, parameters = treatment_simparams)
}

# Function to plot simulation results
plot_simulations <- function(output, output_control = NULL, timesteps) {
  pdf("prpf210.pdf")
  plot_prev(output, output_control, timesteps)
  dev.off()
}