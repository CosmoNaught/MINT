# Function to set mosquito parameters
set_mosquito_parameters <- function(lhs_sample) {
  Anopheles_mosquito_params <- fun_params
  Anopheles_mosquito_params$blood_meal_rates <- 1/3
  Anopheles_mosquito_params$foraging_time <- 0.69
  Anopheles_mosquito_params$mum <- 0.132
  Anopheles_mosquito_params$phi_bednets <- lhs_sample$phi_bednets ## THIS MIGHT BREAK
  Anopheles_mosquito_params$phi_indoors <- lhs_sample$phi_bednets + 0.05 ## THIS MIGHT BREAK
  Anopheles_mosquito_params$Q0 <- lhs_sample$Q0
  Anopheles_mosquito_params$species <- 'Anopheles'
  
  Anopheles_mosquito_params
}