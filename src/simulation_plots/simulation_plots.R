source("plot_support.R")

orderly2::orderly_dependency("simulation_prep", "latest()", 
                              c("input.RDS" = "input.RDS"))

orderly2::orderly_dependency("simulation_launch", "latest()", 
                              c("simulation_results.rds" = "simulation_results.rds"))

input <- readRDS("input.RDS")
output <- readRDS("simulation_results.rds")

lapply(seq_along(output), function(i) {
  # Create a dynamic filename for each output
  pdf_filename <- paste0("output_plots_", i, ".pdf")
  
  # Call the function for each element in the output list
  generate_plots_to_pdf(output[[i]], input[[i]]$treatment_timesteps, pdf_filename)
})
