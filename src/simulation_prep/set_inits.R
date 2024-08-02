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