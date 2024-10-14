library(malariasimulation)
library(parallel)

simulation_function <- function(repetition, timesteps, parameters) {
    
    # Run the simulation and append the repetition number to the dataframe
    df <- malariasimulation::run_simulation(timesteps, parameters)
    df$repetition <- repetition

    return(list(data = df))
}

# Main function
run_sim_with_reps <- function(
    timesteps,
    parameters = list(),
    repetitions,
    parallelism = FALSE,
    workers_override = FALSE
) {
    start_total_time <- Sys.time()

    if (parallelism) {
        # Determine the number of workers to use
        if (isFALSE(workers_override)) {
            # Default to using as many workers as repetitions if workers_override is FALSE
            cores <- repetitions
        } else {
            # Use the specified number of workers if workers_override is set
            cores <- as.integer(workers_override)
        }

        cluster <- makeCluster(cores, outfile = "worker_log.txt")
        clusterCall(cluster, ".libPaths", .libPaths())

        # Load necessary libraries on each cluster worker
        load_libraries_start <- Sys.time()

        clusterCall(cluster, function() {
            library(malariasimulation)
            library(spearMINT)
            TRUE  # Return TRUE to indicate successful execution
        })

        clusterExport(
            cluster,
            varlist = c("simulation_function", "timesteps", "parameters"),
            envir = environment()  # Ensure variables are exported from the correct environment
        )
        # Run the simulations in parallel
        results_list <- parLapply(
            cl = cluster,
            X = seq(repetitions),  # Loop over the number of repetitions
            fun = function(repetition) {
                # Run simulation function and capture logs
                result <- simulation_function(repetition, timesteps, parameters)
                # Capture execution time for this task
                execution_times[repetition] <- as.numeric(difftime(Sys.time(), result$data$repetition, units = "secs"))
                return(result)
            }
        )

        # Separate the data and logs
        list_dfs <- lapply(results_list, function(res) res$data)
        stopCluster(cluster)
    } else {
        print("Repetition sequence: sequential")

        # Use a list to store each repetition's dataframe independently
        list_dfs <- lapply(
            seq(repetitions),
            function(repetition) {
                start_rep_time <- Sys.time()
                print(paste("Starting repetition", repetition, "at", start_rep_time))

                df <- malariasimulation::run_simulation(timesteps, parameters)
                df$repetition <- repetition

                end_rep_time <- Sys.time()
                print(paste("Finished repetition", repetition, "at", end_rep_time))
                print(paste("Duration for repetition", repetition, ":", difftime(end_rep_time, start_rep_time, units = "secs")))

                return(df)
            }
        )
    }

    end_total_time <- Sys.time()
    print(paste("Finished total simulation at", end_total_time))
    print(paste("Total duration:", difftime(end_total_time, start_total_time, units = "secs")))

    # Create a named list where each key corresponds to the repetition number
    names(list_dfs) <- paste0("repetition_", seq(length(list_dfs)))
    return(list_dfs)
}
