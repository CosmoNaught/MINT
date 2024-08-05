library(malariasimulation)

# Define the simulation function outside of the main function
simulation_function <- function(repetition, timesteps, parameters) {
    # Run the simulation and append the repetition number to the dataframe
    df <- malariasimulation::run_simulation(timesteps, parameters)
    df$repetition <- repetition
    return(df)
}

# Main function
run_sim_with_reps <- function(
    timesteps,
    parameters = list(),
    repetitions,
    parallel = FALSE,
    workers_override = FALSE
) {
    if (parallel) {
        print("Repetition sequence: parallel")

        # Determine the number of workers to use
        if (isFALSE(workers_override)) {
            # Default to using as many workers as repetitions if workers_override is FALSE
            cores <- repetitions
        } else {
            # Use the specified number of workers if workers_override is set
            cores <- as.integer(workers_override)
        }

        # Create a cluster with the specified number of cores
        cluster <- parallel::makeCluster(cores, outfile = "worker_log.txt")

        # Export necessary libraries and functions to each cluster worker
        parallel::clusterEvalQ(cluster, {
            library(malariasimulation)
            library(dplyr)
        })

        # Export the simulation function and parameters to the cluster
        parallel::clusterExport(
            cluster,
            varlist = c("simulation_function", "timesteps", "parameters"),
            envir = environment()  # Ensure variables are exported from the correct environment
        )

        # Use parLapply for parallel execution
        list_dfs <- parallel::parLapply(
            cl = cluster,
            X = seq(repetitions),  # Loop over the number of repetitions
            fun = function(repetition) {
                # Use the global variables explicitly inside the worker function
                df <- simulation_function(repetition, timesteps, parameters)
                return(df)
            }
        )

        # Stop the cluster after execution
        parallel::stopCluster(cluster)

    } else {
        print("Repetition sequence: sequential")

        # Use a list to store each repetition's dataframe independently
        list_dfs <- lapply(
            seq(repetitions),
            function(repetition) {
                df <- malariasimulation::run_simulation(timesteps, parameters)
                df$repetition <- repetition
                return(df)
            }
        )
    }

    # Create a named list where each key corresponds to the repetition number
    names(list_dfs) <- paste0("repetition_", seq(length(list_dfs)))
    return(list_dfs)
}