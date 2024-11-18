rrq_malariasim_controller <- function(input, reps) {
    ids <- rrq::rrq_task_create_bulk_call(
    function(k, input) {
      
      result <- malariasimulation::run_simulation(
        input$timesteps,
        input$parameters
      )
      return(result)
    },
    seq_len(reps),
    args = list(input = input)
  )
  
  task_ids <- ids
  return(task_ids)
}

local_cluster_malariasim_controller <- function(input, reps){
    cl <- parallel::makeCluster(max(1, reps - 1))
  
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
  parameter_set_output <- list()
  parameter_set_output$input <- input
  parameter_set_output$input$parameters <- NULL
  for (j in seq_len(reps)) {
    parameter_set_output[[paste0("rep_", j)]] <- list(
      result = results[[j]]$result
    )
  }
  
  output <- parameter_set_output
  return(output)
}