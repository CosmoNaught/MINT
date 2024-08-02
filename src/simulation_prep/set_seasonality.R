# Function to set seasonality
set_seasonality <- function(lhs_sample) {
  seasonal <- tibble(seasonality = list(c(0.285505, -0.1328150,  0.0109352,  0.0139190, 0.3253520, -0.1046750,  0.0779865)), seas_name = 'seasonal')
  perennial <- tibble(seasonality = list(c(0.285277, -0.0216681,  0.0529426,  0.0073646, 0.0248801,  0.0242904, -0.0168910)), seas_name = 'perennial')
  if (lhs_sample$seasonal == 1) {
    selected_seasonality <- seasonal$seasonality[[1]]
  } else {
    selected_seasonality <- perennial$seasonality[[1]]
  }
  
  list(g0 = selected_seasonality[1], g = selected_seasonality[2:4], h = selected_seasonality[5:7])
}
