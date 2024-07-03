library(dplyr)
library(tidyr)

# Assume orderly2 and other necessary data loading routines are in place
orderly2::orderly_dependency("bednet_param_gen", "latest()",
                             c("bednet_params.csv" = "bednet_params.csv"))

# Load bednet parameters from CSV
bednet_params <- read.csv("bednet_params.csv")

# Define the initial scenario grid
initial_scenarios <- expand.grid(
  prevalence = seq(0.05, 0.6, by = 0.05),
  anthropophagy = c("low", "high"),
  biting_inbed_indoors = c("low", "high"),
  seasonal = 1:2,
  resistance = seq(0, 100, by = 20),
  itn_use = seq(0, 1, by = 0.2),
  irs_use = c(0, 0.6, 0.8),
  itn_future = seq(0, 1.0, by = 0.1),
  irs_future = seq(0, 1.0, by = 0.1),
  uncertainty_draw = c("median"),
  lsm = seq(0, 0.8, by = 0.1),
  stringsAsFactors = FALSE
)

# Create bednet combinations
bednet_combinations <- bednet_params %>%
  transmute(bednet_prop = paste(prop_standard, prop_PBO, prop_pyrrole, sep = ", "))

# Define batch size and calculate number of batches
batch_size <- 4096  # Adjust as needed based on your system's memory capacity
n_batches <- ceiling(nrow(initial_scenarios) / batch_size)

# Batch processing
for (i in 1:n_batches) {
  # Define the range for the current batch
  start_row <- (i - 1) * batch_size + 1
  end_row <- min(i * batch_size, nrow(initial_scenarios))
  
  # Extract the current batch
  scenario_chunk <- initial_scenarios[start_row:end_row, ]
  
  # Expand scenarios by crossing them with bednet_combinations
  expanded_scenarios <- crossing(scenario_chunk, bednet_combinations)
  
  # Output to console which batch is currently being processed
  message(sprintf("Processing batch %d of %d", i, n_batches))
  
  # Optionally, save each batch to a CSV file
  filepath <- paste0("output_batch_", i, ".csv")
  write.csv(expanded_scenarios, filepath, row.names = FALSE)
}

# Optional message to indicate completion
message("All batches processed.")
