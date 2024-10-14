# # orderly2::orderly_parameters(run = NULL,
# #     parameter_set = NULL,
# #     reps = NULL)

# # if (!run %in% c("short_run", "long_run")) {
# #   stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
# # }

# # orderly2::orderly_dependency("simulation_prep", "latest()", 
# #                               c("input.RDS" = "input.RDS"))

# # loaded_input <- readRDS("input.RDS")
# # output <- list()
# # input <- loaded_input[[1]]

# # # result <- malariasimulation::run_simulation(
# # #   input$timesteps,
# # #   input$parameters
# # # )
# # # browser()

# # # Start the overall timing
# # overall_start_time <- Sys.time()

# # # Outer loop to iterate over each parameter set
# # for (i in seq(parameter_set)) {
# #   input <- loaded_input[[i]]
# #   parameter_set_output <- list()
# #   parameter_set_output$MINT_parameters <- input$MINT_parameters

# #   # Use bulk call to create all repetition tasks at once
# #   ids <- rrq::rrq_task_create_bulk_call(
# #     function(k, input) {
# #       # Start timing for this repetition
# #       rep_start_time <- Sys.time()
      
# #       # Run the simulation
# #       result <- malariasimulation::run_simulation(
# #         input$timesteps,
# #         input$parameters
# #       )
      
# #       # End timing for this repetition
# #       rep_end_time <- Sys.time()
# #       rep_duration <- difftime(rep_end_time, rep_start_time, units = "secs")
      
# #       # Log the timing for this repetition
# #       log_message <- paste0("Repetition ", k, " took ", rep_duration, " seconds.")
      
# #       return(list(result = result, log = log_message))
# #     },
# #     seq_len(reps),
# #     args = list(input = input)
# #   )

# #   rrq::rrq_task_wait(ids)
# #   results <- rrq::rrq_task_results(ids)

# #   for (j in seq_len(reps)) {
# #     parameter_set_output[[paste0("rep_", j)]] <- list(
# #       result = results[[j]]$result,
# #       log = results[[j]]$log
# #     )
# #   }
  
# #   # Store the entire parameter set's results in the output list
# #   output[[paste0("parameter_set_", i)]] <- parameter_set_output
# # }

# # # End the overall timing
# # overall_end_time <- Sys.time()
# # overall_duration <- difftime(overall_end_time, overall_start_time, units = "secs")

# # # Log the overall duration
# # output$log <- paste0("Overall process took ", overall_duration, " seconds.")

# # # Save the output list into a single RDS file
# # saveRDS(output, file = "simulation_results.rds")
# orderly2::orderly_parameters(run = NULL,
#     parameter_set = NULL,
#     reps = NULL,
#     rrq = NULL)

# if (!run %in% c("short_run", "long_run")) {
#   stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
# }

# orderly2::orderly_dependency("simulation_prep", "latest()", 
#                               c("input.RDS" = "input.RDS"))

# loaded_input <- readRDS("input.RDS")
# output <- list()

# for (i in seq(parameter_set)) {
#   input <- loaded_input[[i]]
#   parameter_set_output <- list()
#   parameter_set_output$MINT_parameters <- input$MINT_parameters

#   if (rrq) {
#     ids <- rrq::rrq_task_create_bulk_call(
#       function(k, input) {

#         result <- malariasimulation::run_simulation(
#           input$timesteps,
#           input$parameters
#         )
#         return(list(result = result))
#       },
#       seq_len(reps),
#       args = list(input = input)
#     )

#     rrq::rrq_task_wait(ids)
#     results <- rrq::rrq_task_results(ids)
#   } else {
#     cl <- parallel::makeCluster((parameter_set * reps) - 1)
    
#     parallel::clusterEvalQ(cl, {
#       library(malariasimulation)
#     })
    
#     parallel::clusterExport(cl, c("input"))

#     task_fun <- function(k, input) {

#       result <- malariasimulation::run_simulation(
#         input$timesteps,
#         input$parameters
#       )
#       return(list(result = result))
#     }
#     results <- parallel::parLapply(cl, seq_len(reps), task_fun, input = input)
#     parallel::stopCluster(cl)
#   }

#   for (j in seq_len(reps)) {
#     parameter_set_output[[paste0("rep_", j)]] <- list(
#       result = results[[j]]$result
#     )
#   }

#   output[[paste0("parameter_set_", i)]] <- parameter_set_output
# }
# saveRDS(output, file = "simulation_results.rds")
orderly2::orderly_parameters(run = NULL,
    init_param_idx = NULL,
    parameter_set = NULL,
    reps = NULL,
    rrq = NULL)

if (!run %in% c("short_run", "long_run")) {
  stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

orderly2::orderly_dependency("simulation_prep", "latest()", 
                              c("input.RDS" = "input.RDS"))

loaded_input <- readRDS("input.RDS")
output <- list()
task_ids <- list()

for (i in seq(init_param_idx, parameter_set)) {
  input <- loaded_input[[i]]
  parameter_set_output <- list()
  parameter_set_output$MINT_parameters <- input$MINT_parameters

  if (rrq) {
    ids <- rrq::rrq_task_create_bulk_call(
      function(k, input) {
        result <- malariasimulation::run_simulation(
          input$timesteps,
          input$parameters
        )
        return(list(result = result))
      },
      seq_len(reps),
      args = list(input = input)
    )
    
    task_ids[[i]] <- ids

  } else {
    cl <- parallel::makeCluster((parameter_set * reps) - 1)
    
    parallel::clusterEvalQ(cl, {
      library(malariasimulation)
    })
    
    parallel::clusterExport(cl, c("input"))

    task_fun <- function(k, input) {

      result <- malariasimulation::run_simulation(
        input$timesteps,
        input$parameters
      )
      return(list(result = result))
    }
    results <- parallel::parLapply(cl, seq_len(reps), task_fun, input = input)
    parallel::stopCluster(cl)

    for (j in seq_len(reps)) {
      parameter_set_output[[paste0("rep_", j)]] <- list(
        result = results[[j]]$result
      )
    }

    output[[paste0("parameter_set_", i)]] <- parameter_set_output
  }
}
if (rrq) {
  all_ids <- unlist(task_ids)
  
  rrq::rrq_task_wait(all_ids)
  all_results <- rrq::rrq_task_results(all_ids)
  
  counter <- 1
  for (i in seq(init_param_idx, parameter_set)) {
    parameter_set_output <- list()
    parameter_set_output$MINT_parameters <- loaded_input[[i]]$MINT_parameters
    for (j in seq_len(reps)) {
      parameter_set_output[[paste0("rep_", j)]] <- list(
        result = all_results[[counter]]$result
      )
      counter <- counter + 1
    }
    output[[paste0("parameter_set_", i)]] <- parameter_set_output
  }
}

saveRDS(output, file = "simulation_results.rds")
