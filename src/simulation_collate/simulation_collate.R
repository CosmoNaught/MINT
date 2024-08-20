library(orderly2)
library(parallel)
library(pbapply)

# Load the dependencies for the script
load_dependencies <- function() {
  suppressMessages({
    orderly2::orderly_dependency("pre_collate", "latest()", c("ids.RDS" = "ids.RDS"))
  })
  readRDS("ids.RDS")
}

# Function to get the base path up to 'MINT'
get_mint_base_path <- function() {
  # Get the current working directory
  wd <- getwd()
  
  # Find the position of 'MINT' in the path
  mint_position <- regexpr("/MINT", wd)
  
  # Extract the path up to and including 'MINT'
  if (mint_position != -1) {
    base_path <- substr(wd, 1, mint_position + attr(mint_position, "match.length") - 1)
    return(base_path)
  } else {
    stop("MINT directory not found in the current working directory.")
  }
}

# Function to process each reportID
process_report <- function(i, reports) {
  # Save the current working directory
  original_wd <- getwd()
  
  # Ensure the working directory is reset on exit, even if an error occurs
  on.exit(setwd(original_wd))
  
  # Access the parameter_set using the index 'i'
  parameter_set <- reports$report_IDs[[i]]$parameter_set
  
  # Generate the name using spearMINT::generate_param_name(parameter_set)
  param_name <- spearMINT::generate_param_name(parameter_set)
  
  # Construct the full path to the output file in the archive
  base_path <- get_mint_base_path()
  report_id <- reports$report_IDs[[i]]$id
  output_path <- file.path(base_path, "archive/simulation_launch", report_id, "output.RDS")
  
  # Change to the MINT directory to access the file
  setwd(file.path(base_path, "archive/simulation_launch", report_id))
  
  # Check if the file exists
  if (!file.exists("output.RDS")) {
    stop(paste("File", output_path, "does not exist."))
  }
  
  # Read the output file
  output <- readRDS("output.RDS")
  
  # Return to the original working directory
  setwd(original_wd)
  
  # Return the results with the new name based on param_name
  list(
    param_name = param_name,
    treatment_simulation_results = output$treatment_simulation_results,
    lhs_sample = output$lhs_sample
  )
}

# Main function to run the parallelized process with progress
run_parallel_process <- function() {
  reports <- load_dependencies()
  
  # Extract the first 10 reportIDs for testing
  reportIDs <- sapply(seq_along(reports$report_IDs), function(x) reports$report_IDs[[x]]$id)
  reportIDs <- reportIDs[5001:10000]
  # Use pblapply from pbapply to show a progress bar
  results <- pblapply(seq_along(reportIDs), process_report, reports, cl = 10)
  
  # Combine results into a list
  outputs <- setNames(lapply(results, function(res) list(
    treatment_simulation_results = res$treatment_simulation_results,
    lhs_sample = res$lhs_sample
  )), sapply(results, `[[`, "param_name"))
  
  return(outputs)
}

# Execute the parallel process
processed_outputs <- run_parallel_process()
saveRDS(processed_outputs, "processed_outputs.RDS")
