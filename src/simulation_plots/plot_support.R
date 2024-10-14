# Set colour palette:
get_cols <- function(){
      cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
}

plot_prevalence_reps <- function(output, timesteps) {
  cols <- get_cols()  # Retrieve color palette

  # Extract all rep names dynamically
  reps <- names(output)[grep("rep", names(output))]
  
  # Start the plot with the first dataset to establish plot parameters
  first_output <- output[[reps[1]]]$result
  time_in_years <- first_output$timestep / 365
  
  plot(x = time_in_years, 
       y = first_output$n_detect_lm_0_1825 / first_output$n_age_0_1825, 
       type = "l", 
       col = cols[1], 
       lwd = 1,
       xlab = "Time (years)", 
       ylab = "PfPR 0-5",
       xaxs = "i", 
       yaxs = "i", 
       ylim = c(0, 1),
       xaxt = 'n')

  # Loop through each output and plot it with a unique color
  for (i in seq_along(reps)) {
    rep_data <- output[[reps[i]]]$result
    time_in_years <- rep_data$timestep / 365
    lines(x = time_in_years, 
          y = rep_data$n_detect_lm_0_1825 / rep_data$n_age_0_1825, 
          col = cols[i %% length(cols) + 1], 
          lwd = 1)
  }

  # Add axes, grid, and legend as before
  year_ticks <- seq(from = floor(min(time_in_years)), to = ceiling(max(time_in_years)), by = 1)
  quarter_ticks <- seq(from = min(time_in_years), to = max(time_in_years), by = 1/4)

  axis(1, at = year_ticks, labels = as.character(year_ticks), tck = -0.02)
  axis(1, at = quarter_ticks, labels = FALSE, tck = -0.01)
  
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
         legend = paste("Simulation", seq_along(reps)),
         col = cols[1:length(reps)], 
         lty = 1, 
         lwd = 2, 
         cex = 0.6, 
         y.intersp = 1.3)
}

plot_clinical_incidence_reps <- function(output, timesteps) {
  cols <- get_cols()  # Retrieve color palette
  
  max_clinical_incidence <- max(sapply(names(output)[grep("rep", names(output))], function(rep) {
    rep_data <- output[[rep]]$result
    max(rep_data$n_inc_clinical_0_36500 / rep_data$n_age_0_36500, na.rm = TRUE)
  }), na.rm = TRUE)

  # Extract all rep names dynamically
  reps <- names(output)[grep("rep", names(output))]
  
  # Plot the first rep to set up the plot parameters
  first_output <- output[[reps[1]]]$result
  time_in_years <- first_output$timestep / 365
  
  plot(x = time_in_years, 
       y = first_output$n_inc_clinical_0_36500 / first_output$n_age_0_36500, 
       type = "l", 
       col = cols[1], 
       lwd = 1,
       xlab = "Time (years)", 
       ylab = "Clinical Incidence 0-100 (Age 0-5)",
       xaxs = "i", 
       yaxs = "i", 
       ylim = c(0, max_clinical_incidence),
       xaxt = 'n')

  # Loop through all reps and plot each
  for (i in seq_along(reps)) {
    rep_data <- output[[reps[i]]]$result
    time_in_years <- rep_data$timestep / 365
    lines(x = time_in_years, 
          y = rep_data$n_inc_clinical_0_36500 / rep_data$n_age_0_36500, 
          col = cols[i %% length(cols) + 1], 
          lwd = 1)
  }

  # Add axes, grid, and legend
  year_ticks <- seq(from = floor(min(time_in_years)), to = ceiling(max(time_in_years)), by = 1)
  quarter_ticks <- seq(from = min(time_in_years), to = max(time_in_years), by = 1/4)

  axis(1, at = year_ticks, labels = as.character(year_ticks), tck = -0.02)
  axis(1, at = quarter_ticks, labels = FALSE, tck = -0.01)

  # Add treatment markers (bednet, IRS, and LSM)
  abline(v = timesteps$mass_bednet / 365, col = "black", lty = 2, lwd = 1)
  text(x = (timesteps$mass_bednet + 10) / 365, y = max_clinical_incidence * 0.95, labels = "Mass ITN.", adj = 0, cex = 0.8)

  abline(v = timesteps$irs / 365, lty = 2, lwd = 1, col = "black")
  text(x = (timesteps$irs + 10) / 365, y = max_clinical_incidence * 0.9, labels = "Spraying\nint.", adj = 0, cex = 0.8)

  abline(v = timesteps$lsm / 365, lty = 2, lwd = 1, col = "black")
  text(x = (timesteps$lsm + 10) / 365, y = max_clinical_incidence * 0.85, labels = "LSM\nint.", adj = 0, cex = 0.8)

  # Add grid and legend
  grid(lty = 2, col = "grey80", lwd = 0.5)
  legend("bottomleft", 
         box.lty = 0, 
         bg = "white",
         legend = paste("Simulation", seq_along(reps)),
         col = cols[1:length(reps)], 
         lty = 1, 
         lwd = 2, 
         cex = 0.6, 
         y.intersp = 1.3)
}

plot_bednet_use_reps <- function(output, timesteps) {
  cols <- get_cols()  # Retrieve color palette

  # Extract all rep names dynamically
  reps <- names(output)[grep("rep", names(output))]
  
  # Start the plot with the first dataset to establish plot parameters
  first_output <- output[[reps[1]]]$result
  time_in_years <- first_output$timestep / 365
  max_bednet_use <- max(sapply(reps, function(rep) max(output[[rep]]$result$n_use_net, na.rm = TRUE)), na.rm = TRUE)
  
  plot(x = time_in_years, 
       y = first_output$n_use_net, 
       type = "l", 
       col = cols[1], 
       lwd = 1,
       xlab = "Time (years)", 
       ylab = "Bednet Usage",
       xaxs = "i", 
       yaxs = "i", 
       ylim = c(0, max_bednet_use),
       xaxt = 'n')

  # Loop through each output and plot it with a unique color
  for (i in seq_along(reps)) {
    rep_data <- output[[reps[i]]]$result
    time_in_years <- rep_data$timestep / 365
    lines(x = time_in_years, 
          y = rep_data$n_use_net, 
          col = cols[i %% length(cols) + 1], 
          lwd = 1)
  }

  # Add axes, grid, and legend as before
  year_ticks <- seq(from = floor(min(time_in_years)), to = ceiling(max(time_in_years)), by = 1)
  quarter_ticks <- seq(from = min(time_in_years), to = max(time_in_years), by = 1/4)

  axis(1, at = year_ticks, labels = as.character(year_ticks), tck = -0.02)
  axis(1, at = quarter_ticks, labels = FALSE, tck = -0.01)
  
  # Add treatment markers
  abline(v = timesteps$mass_bednet / 365, col = "black", lty = 2, lwd = 1)
  text(x = (timesteps$mass_bednet + 10) / 365, y = max_bednet_use * 0.95, labels = "Mass ITN.", adj = 0, cex = 0.8)
  
  abline(v = timesteps$irs / 365, lty = 2, lwd = 1, col = "black")
  text(x = (timesteps$irs + 10) / 365, y = max_bednet_use * 0.9, labels = "Spraying\nint.", adj = 0, cex = 0.8)
  
  abline(v = timesteps$lsm / 365, lty = 2, lwd = 1, col = "black")
  text(x = (timesteps$lsm + 10) / 365, y = max_bednet_use * 0.85, labels = "LSM\nint.", adj = 0, cex = 0.8)

  grid(lty = 2, col = "grey80", lwd = 0.5)
  legend("bottomleft", 
         box.lty = 0, 
         bg = "white",
         legend = paste("Simulation", seq_along(reps)),
         col = cols[1:length(reps)], 
         lty = 1, 
         lwd = 2, 
         cex = 0.6, 
         y.intersp = 1.3)
}


generate_plots_to_pdf <- function(output, timesteps, pdf_filename) {
  # Open a PDF device to save the plots
  pdf(file = pdf_filename, width = 8, height = 6)
  
  # Set color palette
  cols <- get_cols()

  # Plot clinical incidence
  plot_clinical_incidence_reps(output, timesteps)
  
  # Plot prevalence
  plot_prevalence_reps(output, timesteps)
  
  # Plot bednet usage
  plot_bednet_use_reps(output, timesteps)
  
  # Close the PDF device
  dev.off()
}