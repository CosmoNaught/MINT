library(orderly2)
library(parallel)
library(pbapply)

# Load the dependencies for the script
load_dependencies <- function() {
  orderly2::orderly_dependency("pre_collate", "latest()", c("ids.RDS" = "ids.RDS"))
  readRDS("ids.RDS")
}

# Function to process each reportID
process_report <- function(i, reports) {
  # Access the parameter_set using the index 'i'
  parameter_set <- reports$report_IDs[[i]]$parameter_set
  
  # Generate the name using spearMINT::generate_param_name(parameter_set)
  param_name <- spearMINT::generate_param_name(parameter_set)
  
  # Construct the unique file name using param_name
  output_file <- paste0(param_name, "output.RDS")
  
  # Dynamically create the c() argument
  dependency_arg <- setNames("output.RDS", output_file)
  
  # Load the dependencies and read the output file using dynamic file name
  orderly2::orderly_dependency("simulation_launch", reports$report_IDs[[i]]$id, dependency_arg)
  output <- readRDS(output_file)
  
  # Return the results
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
  reportIDs <- reportIDs[1:10]
  
  # Use pblapply from pbapply to show a progress bar
  results <- pblapply(seq_along(reportIDs), process_report, reports, cl = detectCores() - 1)
  
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

