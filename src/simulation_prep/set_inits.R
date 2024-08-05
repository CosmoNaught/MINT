# Function to initialize simulation parameters
initialize_simulation_parameters <- function(lhs_sample, HUMAN_POPULATION, selected_seasonality) {
  simparams <- get_parameters(
    list(
      human_population = HUMAN_POPULATION,
      prevalence_rendering_min_ages = c(0) * 365,
      prevalence_rendering_max_ages = c(5) * 365,
      clinical_incidence_rendering_min_ages = c(0) * 365,
      clinical_incidence_rendering_max_ages = c(100) * 365,
      model_seasonality = TRUE,
      g0 = selected_seasonality$g0,
      g = selected_seasonality$g,
      h = selected_seasonality$h
    )
  )
  
  Anopheles_mosquito_params <- set_mosquito_parameters(lhs_sample)
  simparams <- set_species(
    simparams, 
    species = list(Anopheles_mosquito_params),
    proportions = c(1)
  )
  
  simparams <- set_equilibrium(
    parameters = simparams,
    init_EIR = lhs_sample$eir
  )
  
  simparams
}