library(malariasimulation)
library(parallel)

simulation_function <- function(repetition, timesteps, parameters) {
    start_time <- Sys.time()
    message <- paste("Starting simulation for repetition:", repetition, "at", start_time, "\n")
    
    # Run the simulation and append the repetition number to the dataframe
    df <- malariasimulation::run_simulation(timesteps, parameters)
    df$repetition <- repetition

    end_time <- Sys.time()
    message <- paste(message, "Finished simulation for repetition:", repetition, "at", end_time, "\n")
    message <- paste(message, "Duration for repetition", repetition, ":", difftime(end_time, start_time, units = "secs"), "\n")
    
    return(list(data = df, log = message))
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
    print(paste("Starting total simulation at", start_total_time))

    if (parallelism) {
        print("Repetition sequence: parallel")

        # Determine the number of workers to use
        if (isFALSE(workers_override)) {
            # Default to using as many workers as repetitions if workers_override is FALSE
            cores <- repetitions
        } else {
            # Use the specified number of workers if workers_override is set
            cores <- as.integer(workers_override)
        }

        print(paste("Using", cores, "cores for parallel execution"))

        # Create a cluster with the specified number of cores
        cluster_creation_start <- Sys.time()
        print(paste("Creating cluster at", cluster_creation_start))

        cluster <- makeCluster(cores, outfile = "worker_log.txt")

        cluster_creation_end <- Sys.time()
        print(paste("Cluster created at", cluster_creation_end))
        print(paste("Cluster creation duration:", difftime(cluster_creation_end, cluster_creation_start, units = "secs")))

        # Set the library paths on each cluster worker
        print("Setting library paths on cluster workers...")
        lib_paths_start <- Sys.time()

        clusterCall(cluster, ".libPaths", .libPaths())

        lib_paths_end <- Sys.time()
        print(paste("Library paths set at", lib_paths_end))
        print(paste("Library paths duration:", difftime(lib_paths_end, lib_paths_start, units = "secs")))

        # Load necessary libraries on each cluster worker
        print("Loading libraries on cluster workers...")
        load_libraries_start <- Sys.time()

        clusterCall(cluster, function() {
            library(malariasimulation)
            TRUE  # Return TRUE to indicate successful execution
        })

        load_libraries_end <- Sys.time()
        print(paste("Libraries loaded at", load_libraries_end))
        print(paste("Libraries loading duration:", difftime(load_libraries_end, load_libraries_start, units = "secs")))

        # Export the simulation function and parameters to the cluster
        print("Exporting variables to cluster workers...")
        export_vars_start <- Sys.time()

        clusterExport(
            cluster,
            varlist = c("simulation_function", "timesteps", "parameters"),
            envir = environment()  # Ensure variables are exported from the correct environment
        )

        export_vars_end <- Sys.time()
        print(paste("Variables exported at", export_vars_end))
        print(paste("Variable export duration:", difftime(export_vars_end, export_vars_start, units = "secs")))

        # Use parLapply for parallel execution
        print("Starting parallel execution using parLapply...")
        parLapply_start <- Sys.time()

        # Collect execution times for each task
        execution_times <- numeric(repetitions)

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
        logs <- unlist(lapply(results_list, function(res) res$log))

        # Print all logs from workers
        cat("Worker Logs:\n", logs, "\n")

        parLapply_end <- Sys.time()
        print(paste("Parallel execution finished at", parLapply_end))
        print(paste("parLapply total duration:", difftime(parLapply_end, parLapply_start, units = "secs")))

        # Log the execution times for each task
        print("Individual execution times for each repetition (in seconds):")
        print(execution_times)

        # Calculate the expected utilization
        max_exec_time <- max(execution_times)
        total_worker_time <- sum(execution_times)
        theoretical_parallel_time <- total_worker_time / cores
        print(paste("Maximum single task execution time:", max_exec_time, "seconds"))
        print(paste("Total worker time:", total_worker_time, "seconds"))
        print(paste("Theoretical parallel execution time:", theoretical_parallel_time, "seconds"))

        # Calculate actual efficiency
        efficiency <- total_worker_time / as.numeric(difftime(parLapply_end, parLapply_start, units = "secs"))
        print(paste("Actual parallel efficiency (total worker time / parLapply duration):", round(efficiency * 100, 2), "%"))

        # Stop the cluster after execution
        print("Stopping the cluster...")
        stop_cluster_start <- Sys.time()

        stopCluster(cluster)

        stop_cluster_end <- Sys.time()
        print(paste("Cluster stopped at", stop_cluster_end))
        print(paste("Cluster stop duration:", difftime(stop_cluster_end, stop_cluster_start, units = "secs")))

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
