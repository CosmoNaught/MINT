D1load = readRDS("pyrethroid_uncertainty.RDS")
D2load = readRDS("pbo_uncertainty_using_pyrethroid_dn0_for_mn_durability.RDS")
D3load = readRDS("pyrrole_uncertainty_using_pyrethroid_dn0_for_mn_durability.RDS")

itn_standard_params = function(how_many_draws,  ## First choose the number of uncertainty runs you need
                               resistance_level## Next select the relevant level of resistance you need
){
  
  rng = sample(1:1000,how_many_draws,replace = FALSE)
  
  uncertainty_draws = D1load[which(D1load$resistance == resistance_level),]
  
  ## And translate mn_dur to gamman
  uncertainty_draws$gamman = uncertainty_draws$mean_duration/log(2)
  
  key_data = data.frame(dn0    = uncertainty_draws$dn0,
                        rn0    = uncertainty_draws$rn_pyr,
                        gamman = uncertainty_draws$gamman)
  
  return(key_data[rng,])
}

itn_pbo_params = function(how_many_draws,  ## First choose the number of uncertainty runs you need
                          resistance_level## Next select the relevant level of resistance you need
){
  
  rng = sample(1:1000,how_many_draws,replace = FALSE)
  
  relevant_data = D2load[which(D2load$resistance.x == resistance_level),]
  uncertainty_draws = relevant_data[rng,c(1,2,3,4,10)]
  
  ## And translate mn_dur to gamman
  uncertainty_draws$gamman_pbo = uncertainty_draws$mn_dur/log(2)
  
  key_data = data.frame(dn0    = uncertainty_draws$dn0,
                        rn0    = uncertainty_draws$rn_pbo,
                        gamman = uncertainty_draws$gamman_pbo)
  
  return(key_data)
}


itn_pyrrole_params = function(how_many_draws,  ## First choose the number of uncertainty runs you need
                          resistance_level## Next select the relevant level of resistance you need
){
  
  rng = sample(1:1000,how_many_draws,replace = FALSE)
  
  relevant_data = D3load[which(D3load$resistance.x == resistance_level),]
  uncertainty_draws = relevant_data[rng,c(1,2,3,4,10)]
  
  ## And translate mn_dur to gamman
  uncertainty_draws$gamman_pbo = uncertainty_draws$mn_dur/log(2)
  
  key_data = data.frame(dn0    = uncertainty_draws$dn0,
                        rn0    = uncertainty_draws$rn_pbo,
                        gamman = uncertainty_draws$gamman_pbo)
  
  return(key_data)
}

###########################################################
##
## For median parameter estimates
##
#############################################################

median_ITN_params = function(which_ITNs,
                             resistance_level){
  
  median_ITN_parameters = expand.grid(nets = c("standard","PBO","pyrrole"))
  
  median_ITN_parameters$dn0 = c(median(itn_standard_params(1000,resistance_level)$dn0),
                                median(itn_pbo_params(1000,resistance_level)$dn0),
                                median(itn_pyrrole_params(1000,resistance_level)$dn0))
  
  median_ITN_parameters$rn0 = c(median(itn_standard_params(1000,resistance_level)$rn0),
                                median(itn_pbo_params(1000,resistance_level)$rn0),
                                median(itn_pyrrole_params(1000,resistance_level)$rn0))
  
  median_ITN_parameters$gamman = c(median(itn_standard_params(1000,resistance_level)$gamman),
                                   median(itn_pbo_params(1000,resistance_level)$gamman),
                                   median(itn_pyrrole_params(1000,resistance_level)$gamman))
  
  if(which_ITNs == c("standard"))
    return(c(median_ITN_parameters[1,])) else if(which_ITNs == c("PBO"))
      return(c(median_ITN_parameters[2,])) else if(which_ITNs == c("pyrrole"))
        return(c(median_ITN_parameters[3,])) else if (which_ITNs == "all")
          return(median_ITN_parameters)
}

# Define the sequence of resistance levels
resistance_levels = seq(0, 1, by = 0.01)

# Use lapply to get median ITN parameters for each resistance level
median_ITN_grid = do.call(rbind, lapply(resistance_levels, function(level) {
  params = median_ITN_params("all", level)
  params$resistance_level = level
  return(params)
}))

# Convert the list to a data frame
median_ITN_grid = as.data.frame(median_ITN_grid)

# View the results
print(median_ITN_grid)

saveRDS(median_ITN_grid, "bednet_params_raw.RDS")

bednet_params <- median_ITN_grid

library(dplyr)
library(tidyr)

# Generate all combinations of proportions where sum equals 1
proportions <- expand.grid(prop_standard = seq(0, 1, by = 0.1),
                           prop_PBO = seq(0, 1, by = 0.1),
                           prop_pyrrole = seq(0, 1, by = 0.1)) %>%
  filter(prop_standard + prop_PBO + prop_pyrrole == 1)

# Create a list to hold the results for each resistance level
results <- list()

# Iterate over each unique resistance level
for (level in unique(bednet_params$resistance_level)) {
  sub_data <- bednet_params %>%
    filter(resistance_level == level)
  
  # Calculate weighted averages for each combination
  expanded_data <- proportions %>%
    rowwise() %>%
    mutate(dn0 = sum(sub_data$dn0 * c(prop_standard, prop_PBO, prop_pyrrole)),
           rn0 = sum(sub_data$rn0 * c(prop_standard, prop_PBO, prop_pyrrole)),
           gamman = sum(sub_data$gamman * c(prop_standard, prop_PBO, prop_pyrrole)),
           resistance_level = level) # Include resistance level directly
  
  # Append the results to the list
  results[[as.character(level)]] <- expanded_data
}

# Combine all results into one dataframe
final_data <- bind_rows(results)

write.csv(final_data, "bednet_params.csv", row.names = FALSE)