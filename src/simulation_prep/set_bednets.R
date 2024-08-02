set_bednet_parameters <- function(simparams, lhs_sample, bednet_params, baseline = TRUE) {
  # Sample value for dn0_use and dn0_future (replace lhs_sample$dn0_use and lhs_sample$dn0_future with actual data)
  target_dn0_use <- round(lhs_sample$dn0_use, digits = 3)
  target_dn0_future <- round(lhs_sample$dn0_future, digits = 3)

  # Calculate differences and find the closest index for dn0_use
  differences_use <- abs(bednet_params$dn0 - target_dn0_use)
  closest_index_use <- which.min(differences_use)
  selected_net_params_use <- bednet_params[closest_index_use, ]

  # Calculate differences and find the closest index for dn0_future
  differences_future <- abs(bednet_params$dn0 - target_dn0_future)
  closest_index_future <- which.min(differences_future)
  selected_net_params_future <- bednet_params[closest_index_future, ]

  # Generate ITN distribution timings
  fetch_net_distribution_times <- function() {
    routine_topups_DOY <- round(seq(DOY_mass_campaigns, by = routine_interval,
                                    length.out = routine_topups_per_year))
    routine_topups_DOY <- sort(routine_topups_DOY %% year)
    rep_years <- rep(seq(0, ceiling(sim_length/year)),
                    each = routine_topups_per_year)
    net_times <- routine_topups_DOY + rep_years * year
    N_dist <- sum(net_times < sim_length)
    net_times <- net_times[1:N_dist]
    return(net_times)
  }

  # Identify ITN distributions for mass campaigns
  fetch_mass_campaign_times <- function(first_year = 0,
                                        final_year = 12,
                                        interval = 3) {
    mass_campaign_times <- DOY_mass_campaigns + year * seq(
      first_year, final_year, interval
    )
  }

  # Distribute enough nets to achieve target given current use
  targetted_distribution <- function(current_use = lhs_sample$itn_use,
                                    target_use = lhs_sample$itn_future) {
    
    # Fail-safes that shouldn't be triggered
    if (current_use > 1) {current_use <- 1}
    if (current_use < 0) {current_use <- 0}
    if (target_use > 1) {target_use <- 1}
    
    # Fail-safe that may be triggered
    # (e.g. for some situations where future use < historic use)
    if (target_use < current_use) {target_use <- current_use}
    
    # Calculate number of nets needed to reach target (assuming random allocation)
    prop_uncovered <- 1 - current_use
    target_difference <- target_use - current_use
    nets_distributed <- target_difference / prop_uncovered
    
    return(nets_distributed)
  }

  # Generate number of nets to be distributed at each distribution event
  fetch_nets_distributed <- function() {
    
    # Historic and future mass campaign timings
    historic_mass_campaigns <<- fetch_mass_campaign_times(
      first_year = first_historic_campaign,
      final_year = last_historic_campaign,
      interval = historic_campaign_interval
    )
    future_mass_campaigns <<- fetch_mass_campaign_times(
      first_year = first_future_campaign,
      final_year = last_future_campaign,
      interval = future_campaign_interval
    )
    
    # Calculate routine top-ups
    if (historic_routine) {
      historic_monthly_routine_loss <- historic_routine_usage * (1 - exp(
        -(year/routine_topups_per_year) * lambda)
      )
      historic_routine_top_up <- historic_monthly_routine_loss / (
        1-historic_routine_usage-historic_monthly_routine_loss)
    } else {
      historic_monthly_routine_loss <- 0
      historic_routine_top_up <- 0
    }
    if (future_routine) {
      future_monthly_routine_loss <- future_routine_usage * (1 - exp(
        -(year/routine_topups_per_year) * lambda)
      )
      future_routine_top_up <- future_monthly_routine_loss / (
        1-future_routine_usage-future_monthly_routine_loss)
    } else {
      future_monthly_routine_loss <- 0
      future_routine_top_up <- 0
    }
    
    # Initialise expected proportion using nets and nets distributed to reach this
    E_nets <- rep(0, N_dist)
    nets_distributed <- rep(0, N_dist)
    
    # Calculate expected proportion with nets and the number of nets to be
    # distributed in each distribution event
    for (i in 1:N_dist) {
      if (net_times[i] %in% historic_mass_campaigns) {
        # Net distribution is a historical mass campaigns
        if (i == 1) {
          pre_distribution <- 0
        } else {
          pre_distribution <- E_nets[i-1] * exp(-routine_interval*lambda)
        }
        nets_distributed[i] <- targetted_distribution(
          current_use = pre_distribution,
          target_use = historic_max_usage
        )
        E_nets[i] <- historic_max_usage
      } else if (net_times[i] %in% future_mass_campaigns) {
        # Net distribution is a future mass campaigns
        pre_distribution <- E_nets[i-1] * exp(-routine_interval*lambda)
        nets_distributed[i] <- targetted_distribution(current_use = pre_distribution,
                                                      target_use = future_max_usage)
        E_nets[i] <- future_max_usage
      } else if (net_times[i] < future_mass_campaigns[1]) {
        # Net distribution is a historical routine top-up
        if (i == 1) {
          E_nets[i] <- 0
        } else {
          nets_distributed[i] <- historic_routine_top_up
          pre_distribution <- E_nets[i-1] * exp(-routine_interval*lambda)
          E_nets[i] <- pre_distribution + (1 - pre_distribution) * historic_routine_top_up
        }
      } else {
        # Net distribution is a future routine top-up
        nets_distributed[i] <- future_routine_top_up
        pre_distribution <- E_nets[i-1] * exp(-routine_interval*lambda)
        E_nets[i] <- pre_distribution + (1 - pre_distribution) * future_routine_top_up
      }
    }
    
    return(nets_distributed)
  }

  # Input parameters

  year <- 365

  # Routine nets
  routine_topups_per_year <- 26 # 0 off. anything else on i.e 26 every 2 weeks try 12
  historic_routine <- TRUE  # Assumed to start at t = 1

  if (lhs_sample$routine == 0) {
    future_routine <- FALSE    # Assumed to start with first future mass campaign
  } else {
    future_routine <- TRUE
  }

  prop_historic_routine <- function(prop_any_itn) {0.1562572 * prop_any_itn}
  prop_future_routine <- function(prop_any_itn) {0.1562572 * prop_any_itn}

  # Day of year of mass campaigns
  DOY_mass_campaigns <- 1 # Set to 1 if fourier series is shifted

  # Historic mass campaigns
  historic_max_usage <- lhs_sample$itn_use
  first_historic_campaign <- 0
  last_historic_campaign <- 9
  historic_campaign_interval <- 3

  # Future mass campaigns
  future_max_usage <- lhs_sample$itn_future
  first_future_campaign <- 9
  last_future_campaign <- 9
  future_campaign_interval <- 3

  # Simulation parameters
  sim_length <- 12 * year
  N_species <- 1
  invlambda <- 2.1 * year       # mean duration of ITN use

  # Historic net parameters
  dn0_historic <- selected_net_params_use$dn0
  rn0_historic <- selected_net_params_use$rn0
  rnm_historic <- 0.24
  gamman_historic <- selected_net_params_use$gamman

  # Future net parameters
  dn0_future <- selected_net_params_future$dn0
  rn0_future <- selected_net_params_future$rn0
  rnm_future <- 0.24
  gamman_future <- selected_net_params_future$gamman

  #-------------------------------------------------------------------------------
  # Calculate dependent parameters

  lambda <- 1 / invlambda
  routine_interval <- year / routine_topups_per_year
  if (historic_routine) {
    historic_routine_usage <- prop_historic_routine(historic_max_usage)
  } else {
    historic_routine_usage <- 0
  }
  if (future_routine) {
    future_routine_usage <- prop_future_routine(future_max_usage)
  } else {
    future_routine_usage <- 0
  }

  #-------------------------------------------------------------------------------
  # Generate numbers of nets distributed and timings

  net_times <- fetch_net_distribution_times()
  N_dist <- length(net_times)
  nets_distributed <- fetch_nets_distributed()
  N_historic_dist <- which(net_times == future_mass_campaigns[1]) - 1
  N_future_dist <- N_dist - N_historic_dist

  # Prepare matrices of parameters for historic and future bednets
  dn0_mat <- matrix(rep(c(rep(dn0_historic, N_historic_dist),
                          rep(dn0_future, N_future_dist)),
                        N_species),
                    nrow = N_dist, ncol = N_species)
  rn0_mat <- matrix(rep(c(rep(rn0_historic, N_historic_dist),
                          rep(rn0_future, N_future_dist)),
                        N_species),
                    nrow = N_dist, ncol = N_species)
  rnm_mat <- matrix(rep(c(rep(rnm_historic, N_historic_dist),
                          rep(rnm_future, N_future_dist)),
                        N_species),
                    nrow = N_dist, ncol = N_species)
  gamman_vec <- c(rep(gamman_historic, N_historic_dist),
                  rep(gamman_future, N_future_dist))

  # Set net parameters
  simparams <- malariasimulation::set_bednets(
    simparams,
    timesteps = net_times, 
    coverages = nets_distributed,
    retention = invlambda,
    dn0 = dn0_mat,
    rn = rn0_mat,
    rnm = rnm_mat,
    gamman = gamman_vec
  )
  
  list(simparams = simparams, timesteps = net_times)

}