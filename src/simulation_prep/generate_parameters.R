
generate_parameters <- function(i, lhs_data, HUMAN_POPULATION, bednet_params, SIM_LENGTH) {
  
  # Extract lhs_sample for the given index
  lhs_sample <- lhs_data[i, ]
  
  # Initialize simulation parameters for each row independently
  selected_seasonality <- set_seasonality(lhs_sample)
  simparams <- initialize_simulation_parameters(lhs_sample, HUMAN_POPULATION, selected_seasonality)
  
  # Assign the simulation parameters for treatment
  treatment_simparams <- simparams
  
  # Initialize parameters and lists to store timesteps
  bednet_treatment_result <- set_bednet_parameters(treatment_simparams, lhs_sample, bednet_params)
  treatment_simparams <- bednet_treatment_result$simparams
  bednet_treatment_timesteps <- bednet_treatment_result$timesteps
  
  irs_treatment_result <- set_irs_parameters(treatment_simparams, lhs_sample)
  treatment_simparams <- irs_treatment_result$simparams
  irs_treatment_timesteps <- irs_treatment_result$timesteps
  
  lsm_treatment_result <- set_lsm_parameters(treatment_simparams, lhs_sample)
  treatment_simparams <- lsm_treatment_result$simparams
  lsm_treatment_timesteps <- lsm_treatment_result$timesteps
  
  unique_bednet_timesteps <- unique(bednet_treatment_timesteps)
  unique_irs_timesteps <- unique(irs_treatment_timesteps)
  unique_lsm_timesteps <- unique(lsm_treatment_timesteps)
  
  # Generate the parameter name
  param_name <- spearMINT::generate_param_name(i)
  
  # Create input with the specific param_name
  input_entry <- list()
  
  input_entry[[paste0("input", param_name)]] <- list(
    MINT_parameters = lhs_sample,
    timesteps = SIM_LENGTH,
    parameters = treatment_simparams,
    treatment_timesteps = list(
      mass_bednet = c(0, 3, 6, 9) * 365,
      irs = unique_irs_timesteps,
      lsm = unique_lsm_timesteps
    )
  )
  
  return(input_entry)
}