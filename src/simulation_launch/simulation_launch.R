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
    cl <- parallel::makeCluster(max(1, (parameter_set * reps) - 1))
    
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
