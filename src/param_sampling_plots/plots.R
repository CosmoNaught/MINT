param_pair_scatter_plot <- function(data_grid, data_lhs, param1, param2) {
  # Creating the ggplot object
  p <- ggplot() +
    geom_point(data = data_grid, aes_string(x = param1, y = param2, color = "'Expand Grid'"), alpha = 0.6) +
    geom_point(data = data_lhs, aes_string(x = param1, y = param2, color = "'LHS'"), alpha = 0.6) +
    scale_color_manual(values = c("Expand Grid" = "blue", "LHS" = "red")) +
    labs(title = paste("Comparison of Parameter Space Coverage:", param1, "vs", param2),
         x = param1, y = param2, color = "Method") +
    theme_minimal()

  # Return the plot
  return(p)
}

param_histogram_plot <- function(data_grid, data_lhs) {
  # Add a Method column to each dataset to label the source
  data_grid$Method <- 'Expand Grid'
  data_lhs$Method <- 'LHS'
  
  # Combine the datasets
  combined_data <- rbind(data_grid, data_lhs)

  # Melt the combined data to use for faceting in ggplot2
  library(reshape2)
  melted_data <- melt(combined_data, id.vars = "Method")

  # Creating the ggplot object with histograms for each parameter
  p <- ggplot(melted_data, aes(x = value, fill = Method)) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    scale_fill_manual(values = c("Expand Grid" = "blue", "LHS" = "red")) +
    facet_wrap(~variable, scales = "free_x") +
    labs(title = "Density Distributions for Each Parameter",
         x = "Value", y = "Count", fill = "Method") +
    theme_minimal()

  # Return the plot
  return(p)
}

param_scatter_plot <- function(data_grid, data_lhs) {
  # Add a Method column to each dataset to label the source
  data_grid$Method <- 'Expand Grid'
  data_lhs$Method <- 'LHS'
  
  # Combine the datasets
  combined_data <- rbind(data_grid, data_lhs)
  
  # Function to create scatter plots for the lower triangle
  lower_fn <- function(data, mapping, ...) {
    ggplot(data = data, mapping = mapping, ...) +
      geom_point(aes(color = Method), alpha = 0.6) +
      scale_color_manual(values = c("Expand Grid" = "blue", "LHS" = "red"))
  }
  
  # Plot the scatterplot matrix using ggpairs
  p <- ggpairs(combined_data, columns = 1:(ncol(combined_data)-1), lower = list(continuous = lower_fn), diag = list(continuous = "blank")) +
    theme_minimal() +
    labs(title = "Scatterplot Matrix for Parameters")
  
  # Return the plot
  return(p)
}