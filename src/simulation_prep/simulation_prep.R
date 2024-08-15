library(data.table)
library(parallel)
library(malariasimulation)
library(tibble)
library(spearMINT)

# Source additional scripts
source("set_inits.R")
source("set_species.R")
source("set_seasonality.R")
source("set_bednets.R")
source("set_irs.R")
source("set_lsm.R")

# Set seed for reproducibility
set.seed(123)

# Constants
YEAR <- 365
SIM_LENGTH <- 12 * YEAR
HUMAN_POPULATION <- 100000

# Load dependencies
orderly2::orderly_parameters(run = NULL, chunk_size = NULL, subset_override = NULL)

if (!run %in% c("short_run", "long_run")) {
  stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

orderly2::orderly_dependency("collate_bednet_param", "latest()", 
                             c("bednet_params_raw.RDS" = "bednet_params_raw.RDS"))

bednet_params <- readRDS("bednet_params_raw.RDS")

orderly2::orderly_dependency("param_sampling", "latest(parameter:run == this:run)",
                             c("lhs_scenarios.csv" = "lhs_scenarios.csv"))

# Load the entire dataset before parallel processing
lhs_data <- data.table::fread("lhs_scenarios.csv")[1:subset_override, ]
TOTAL_ROWS <- nrow(lhs_data)

# Efficiently determine the number of rows
cat("Total number of rows:", TOTAL_ROWS, "\n") # Debug statement

# Calculate the number of chunks based on the chunk size
TOTAL_CHUNKS <- ceiling(TOTAL_ROWS / chunk_size)
cat("Total number of chunks:", TOTAL_CHUNKS, "\n") # Debug statement

# Function to process each chunk
process_chunk <- function(chunk_index) {
  # Capture messages in a vector
  debug_messages <- c()
  
  # Calculate start and end row for the current chunk
  start_row <- (chunk_index - 1) * chunk_size + 1
  end_row <- min(chunk_index * chunk_size, TOTAL_ROWS)
  
  # Add debug information
  debug_messages <- c(debug_messages, paste("Processing chunk", chunk_index, "of", TOTAL_CHUNKS))
  debug_messages <- c(debug_messages, paste("Reading rows", start_row, "to", end_row))
  cat(debug_messages, sep = "\n")
  flush.console()  # Ensure output is flushed to the console
  
  # Read the specific chunk from the pre-loaded data
  data_chunk <- lhs_data[start_row:end_row, ]
  
  debug_messages <- c(debug_messages, paste("Rows read:", nrow(data_chunk)))
  if (nrow(data_chunk) == 0) {
    error_message <- paste("Error: Data chunk is empty for chunk", chunk_index)
    debug_messages <- c(debug_messages, error_message)
    cat(error_message, "\n")
    flush.console()
    return(list(results = NULL, messages = debug_messages))
  }
  
  # Process each row within the chunk
  inputs <- lapply(1:nrow(data_chunk), function(i) {
    debug_messages <- c(debug_messages, paste("Processing row", i, "of chunk", chunk_index))
    cat("Processing row", i, "of chunk", chunk_index, "\n")
    flush.console()
    
    # Read the lhs_sample
    lhs_sample <- data_chunk[i, ]
    
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
    param_name <- spearMINT::generate_param_name((chunk_index - 1) * chunk_size + i)
    debug_messages <- c(debug_messages, paste("Generated parameter name:", param_name))
    cat("Generated parameter name:", param_name, "\n")
    flush.console()
    
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
  })
  
  # Combine all inputs from this chunk into a single list
  combined_inputs <- do.call(c, inputs)
  debug_messages <- c(debug_messages, paste("Chunk", chunk_index, "processed successfully"))
  cat("Chunk", chunk_index, "processed successfully\n")
  flush.console()
  
  list(results = combined_inputs, messages = debug_messages)  # Return both results and messages
}

# Parallel processing setup
num_cores <- min(detectCores(), 10)  # Use up to 10 cores or the number of available cores
cat("Using", num_cores, "cores for parallel processing\n") # Debug statement
flush.console()
cl <- makeCluster(num_cores)

# Export necessary variables and functions to the cluster
clusterExport(cl, c("YEAR", "SIM_LENGTH", "HUMAN_POPULATION", "chunk_size", "TOTAL_CHUNKS",
                    "TOTAL_ROWS", "bednet_params", "generate_param_name", 
                    "set_seasonality", "initialize_simulation_parameters", 
                    "set_bednet_parameters", "set_irs_parameters", "set_lsm_parameters", 
                    "lhs_data"))  # Ensure lhs_data is exported

# Export required libraries to the cluster
clusterEvalQ(cl, {
  library(tibble)
  library(malariasimulation)
  library(spearMINT)
  source("set_inits.R")
  source("set_species.R")
  source("set_seasonality.R")
  source("set_bednets.R")
  source("set_irs.R")
  source("set_lsm.R")
  cat("Worker ready for processing\n") # Debug statement
  flush.console()
})

cat("Beginning Processing using parLapply\n")
flush.console()

# Process all chunks in parallel
results <- parLapply(cl, 1:TOTAL_CHUNKS, process_chunk)

# Stop the cluster
stopCluster(cl)
cat("Parallel processing completed\n") # Debug statement
flush.console()

# Combine results and messages
all_inputs <- lapply(results, function(res) res$results)
all_messages <- unlist(lapply(results, function(res) res$messages))

# Print all debug messages
cat(all_messages, sep = "\n")
flush.console()

# Combine all results into a single list
final_input <- do.call(c, all_inputs)

# Assign the combined results to input
input <- final_input

# Save final input as RDS
cat("Saving final input to input.RDS\n") # Debug statement
flush.console()
saveRDS(input, "input.RDS")

cat("Script execution completed\n") # Debug statement
flush.console()
