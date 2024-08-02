run_sim_with_reps <- function(
    timesteps,
    parameters = list(),
    repetitions,
    parallel = FALSE
) {
  if (parallel) {
    fapply <- parallel::mclapply
  } else {
    fapply <- lapply
  }
  
  # Use a list to store each repetition's dataframe independently
  list_dfs <- fapply(
    seq(repetitions),
    function(repetition) {
      df <- run_simulation(timesteps, parameters)
      df$repetition <- repetition
      return(df)
    }
  )
  
  # Create a named list where each key corresponds to the repetition number
  names(list_dfs) <- paste0("repetition_", seq(repetitions))
  return(list_dfs)
}