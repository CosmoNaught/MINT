# Combined plotting function for both bednets and spraying
# Set colour palette:

get_cols <- function(){
      cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
}

plot_prev <- function(cols = cols) {
  # Plot for the scenario with interventions
  cols <- get_cols()

  # Calculate the time in years from days for plotting
  time_in_years <- output$timestep / 365.25

  plot(x = time_in_years, 
       y = output$n_detect_730_3650 / output$n_730_3650, 
       type = "l", 
       col = cols[3], 
       lwd = 1,
       xlab = "Time (years)", 
       ylab = expression(paste(italic(Pf), "PR"[2-10])),
       xaxs = "i", 
       yaxs = "i", 
       ylim = c(0, 1),
       xaxt = 'n')  # Turn off default x-axis to customize

  # Define tick positions and labels
  year_ticks <- seq(from = floor(min(time_in_years)), to = ceiling(max(time_in_years)), by = 1)
  quarter_ticks <- seq(from = min(time_in_years), to = max(time_in_years), by = 1/4)

  # Major ticks every year and minor ticks every 3 months
  axis(1, at = year_ticks, labels = as.character(year_ticks), tck = -0.02)
  axis(1, at = quarter_ticks, labels = FALSE, tck = -0.01)

  # Plot for the control scenario
  lines(x = output_control$timestep / 365.25, 
        y = output_control$n_detect_730_3650 / output_control$n_730_3650,
        col = cols[5], 
        lwd = 1)
  
  # Add intervention markers
  abline(v = bednetstimesteps / 365.25, col = "black", lty = 2, lwd = 1)
  text(x = (bednetstimesteps + 10) / 365.25, y = 0.95, labels = "Bed net int.", adj = 0, cex = 0.8)
  
  abline(v = sprayingtimesteps / 365.25, lty = 2, lwd = 1, col = "black")
  text(x = (sprayingtimesteps + 10) / 365.25, y = 0.9, labels = "Spraying\nint.", adj = 0, cex = 0.8)
  
  # Add grid and legend
  grid(lty = 2, col = "grey80", lwd = 0.5)
  legend("bottomleft", 
         box.lty = 0, 
         bg = "white",
         legend = c("Prevalence for intervention scenario", "Prevalence for control scenario"),
         col = c(cols[3], cols[5]), 
         lty = c(1, 1), 
         lwd = 2, 
         cex = 0.8, 
         y.intersp = 1.3)
}
