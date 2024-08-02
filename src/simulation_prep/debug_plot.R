# Set colour palette:
get_cols <- function(){
      cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
}

plot_prev <- function(outputs, output_control = NULL, timesteps) {
  cols <- get_cols()  # Retrieve color palette
  # Start the plot with the first dataset to establish plot parameters
  first_output <- outputs[[1]]
  time_in_years <- first_output$timestep / 365
  plot(x = time_in_years, 
       y = first_output$n_detect_730_3650 / first_output$n_730_3650, 
       type = "l", 
       col = cols[1], 
       lwd = 1,
       xlab = "Time (years)", 
       ylab = expression(paste(italic(Pf), "PR"[2-10])),
       xaxs = "i", 
       yaxs = "i", 
       ylim = c(0, 1),
       xaxt = 'n')

  # Loop through each output and plot it with a unique color
  for (i in seq_along(outputs)) {
    output <- outputs[[i]]
    time_in_years <- output$timestep / 365
    lines(x = time_in_years, 
          y = output$n_detect_730_3650 / output$n_730_3650, 
          col = cols[i %% length(cols) + 1], 
          lwd = 1)
  }

  # Add axes, grid, and legend as before
  year_ticks <- seq(from = floor(min(time_in_years)), to = ceiling(max(time_in_years)), by = 1)
  quarter_ticks <- seq(from = min(time_in_years), to = max(time_in_years), by = 1/4)

  axis(1, at = year_ticks, labels = as.character(year_ticks), tck = -0.02)
  axis(1, at = quarter_ticks, labels = FALSE, tck = -0.01)

  if (!is.null(output_control)) {
    lines(x = output_control$timestep / 365, 
          y = output_control$n_detect_730_3650 / output_control$n_730_3650,
          col = cols[length(cols)], 
          lwd = 1)
  }
  
  # Add treatment markers
  abline(v = timesteps$mass_bednet / 365, col = "black", lty = 2, lwd = 1)
  text(x = (timesteps$mass_bednet + 10) / 365, y = 0.95, labels = "Mass ITN.", adj = 0, cex = 0.8)
  
  abline(v = timesteps$irs / 365, lty = 2, lwd = 1, col = "black")
  text(x = (timesteps$irs + 10) / 365, y = 0.9, labels = "Spraying\nint.", adj = 0, cex = 0.8)
  
  abline(v = timesteps$lsm / 365, lty = 2, lwd = 1, col = "black")
  text(x = (timesteps$lsm + 10) / 365, y = 0.85, labels = "LSM\nint.", adj = 0, cex = 0.8)

  grid(lty = 2, col = "grey80", lwd = 0.5)
  legend("bottomleft", 
         box.lty = 0, 
         bg = "white",
         legend = paste("Simulation", seq_along(outputs)),
         col = cols[1:length(outputs)], 
         lty = 1, 
         lwd = 2, 
         cex = 0.6, 
         y.intersp = 1.3)
}

# Function to plot simulation results
plot_simulations <- function(output, output_control = NULL, timesteps) {
  pdf("prpf210.pdf")
  plot_prev(output, output_control, timesteps)
  dev.off()
}

