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
  wd <- getwd()
  mint_position <- regexpr("/MINT", wd)
  if (mint_position != -1) {
    base_path <- substr(wd, 1, mint_position + attr(mint_position, "match.length") - 1)
    return(base_path)
  } else {
    stop("MINT directory not found in the current working directory.")
  }
}

# Function to process each reportID and extract required information
process_report <- function(i, reports) {
  original_wd <- getwd()
  on.exit(setwd(original_wd))
  
  parameter_set <- reports$report_IDs[[i]]$parameter_set
  param_name <- spearMINT::generate_param_name(parameter_set)
  
  base_path <- get_mint_base_path()
  report_id <- reports$report_IDs[[i]]$id
  output_path <- file.path(base_path, "archive/simulation_launch", report_id, "output.RDS")
  
  setwd(file.path(base_path, "archive/simulation_launch", report_id))
  
  if (!file.exists("output.RDS")) {
    stop(paste("File", output_path, "does not exist."))
  }
  
  output <- readRDS("output.RDS")
  
  setwd(original_wd)
  
  # Extract the required data
  treatment_simulation_results <- output$treatment_simulation_results
  extracted_data <- lapply(treatment_simulation_results, function(repetition) {
    timestep <- repetition$timestep
    prevalence <- repetition$n_detect_0_1825 / repetition$n_0_1825
    incidence <- repetition$n_inc_clinical_0_36500 / repetition$n_0_36500
    list(
      timestep = timestep,
      prevalence = prevalence,
      incidence = incidence
    )
  })
  
  list(
    param_name = param_name,
    extracted_data = extracted_data
  )
}

# Main function to run the parallelized process with progress
run_parallel_process <- function() {
  reports <- load_dependencies()
  
  reportIDs <- sapply(seq_along(reports$report_IDs), function(x) reports$report_IDs[[x]]$id)
  
  results <- pblapply(seq_along(reportIDs), process_report, reports, cl = 10)
  
  outputs <- setNames(lapply(results, function(res) res$extracted_data), sapply(results, `[[`, "param_name"))
  
  return(outputs)
}

# Execute the parallel process
processed_outputs <- run_parallel_process()
saveRDS(processed_outputs, "processed_outputs.RDS")
