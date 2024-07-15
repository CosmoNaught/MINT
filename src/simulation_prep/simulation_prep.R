# Preamble

library(malariasimulation)
library(tibble)

source("debug_plot.R")

set.seed(123)

## Load data from either a long or short run

orderly2::orderly_parameters(run = NULL)

if(!run %in% c("short_run", "long_run")) {
    stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

orderly2::orderly_dependency("bednet_param_gen", "latest()",
							 c("bednet_params.csv" = "bednet_params.csv"))

# Load bednet parameters from CSV
bednet_params <- read.csv("bednet_params.csv")

orderly2::orderly_dependency("param_sampling", "latest(parameter:run == this:run)",
                             c("lhs_scenarios_sample.csv" = "lhs_scenarios_sample.csv"))

lhs_sample <- read.csv("lhs_scenarios_sample.csv")

lhs_sample <- lhs_sample[4,]

# Set base sim params

year <- 365
sim_length <- 6 * year
human_population <- 10000


## Set seasonality - CHECK VALUES WITH KELLY!

seas_name <- 'seasonal'
seasonality <- list(c(0.285505,-0.325352,-0.0109352,0.0779865,-0.132815,0.104675,-0.013919))
s2 <- tibble(seasonality, seas_name)

seas_name <- 'perennial'
seasonality <- list(c(0.2852770,-0.0248801,-0.0529426,-0.0168910,-0.0216681,-0.0242904,-0.0073646))
s3 <- tibble(seasonality, seas_name)

# Check lhs_sample$seasonality and select appropriate seasonality
if (lhs_sample$seasonal == 1) {
  selected_seasonality <- s2$seasonality[[1]]
} else {
  selected_seasonality <- s3$seasonality[[1]]
}

# Extract g0, g, and h
g0 <- selected_seasonality[1]
g <- selected_seasonality[2:4]
h <- selected_seasonality[5:7]

simparams <- get_parameters(
  list(
    human_population = human_population,
    # seasonality parameters
    model_seasonality = TRUE, 
    g0 = g0,
    g = g,
    h = h
  )
)

## Set mosquito species

amphiphilic_mosquito_params <- fun_params
amphiphilic_mosquito_params$phi_indoors <- lhs_sample$biting_inbed_indoors / 100
amphiphilic_mosquito_params$Q0 <- lhs_sample$anthropophagy / 100

amphiphilic_mosquito_params$species <- 'amphiphilic'

simparams <- set_species(
  simparams, 
  species = list(amphiphilic_mosquito_params),
  proportions = c(1)
)

## Set EIR

starting_EIR <- lhs_sample$eir

simparams <- set_equilibrium(
	parameters = simparams,
    init_EIR = starting_EIR
)

# Set bednet parameters

bednetstimesteps <- c(0, 3) * year # The bed nets will be distributed at the end of the first and the 4th year. 

### CONFUSED ON HOW TO TREAT MULTIPLE DN0 ROWS

target_dn0 <- round(lhs_sample$dn0, digits = 3)
 
matching_rows <- which(round(bednet_params$dn0, 3) == target_dn0)
 
## FOR NOW EXTRACT THE LAST VALUE
selected_net_params <- bednet_params[matching_rows,][length(bednet_params[matching_rows,]),]

## what is rnm?

## THIS DOES NOT ALLOW FOR VARYING TYPES OF NETS PRIOR TO SIMULATION START

bednetparams <- set_bednets(
  simparams,
  timesteps = bednetstimesteps,
  coverages = c(lhs_sample$itn_use, lhs_sample$itn_future),  # Each round is distributed to 50% of the population.
  retention = 5 * year, # Nets are kept on average 5 years
  dn0 = matrix(c(lhs_sample$dn0), nrow = length(bednetstimesteps), ncol = 1), # Matrix of death probabilities for each mosquito species over time
  rn = matrix(c(selected_net_params$rn0), nrow = length(bednetstimesteps), ncol = 1), # Matrix of repelling probabilities for each mosquito species over time
  rnm = matrix(c(.24), nrow = length(bednetstimesteps), ncol = 1), # Matrix of minimum repelling probabilities for each mosquito species over time
  gamman = rep(selected_net_params$gamman * 365, 2) # Vector of bed net half-lives for each distribution timestep
)

# Set irs parameters

peak <- peak_season_offset(simparams)
month <- 30

# sprayingtimesteps <- c(0, 3) * year # UNCLEAR + peak - 3 * month # A round of IRS is implemented in the 1st and second year 3 months prior to peak transmission.
sprayingtimesteps <- c(0, 3) * year + peak - 3 * month # A round of IRS is implemented in the 1st and second year 3 months prior to peak transmission.
sprayingtimesteps[1] <- 0

sprayingparams <- set_spraying(
  simparams,
  timesteps = sprayingtimesteps,
  coverages = c(lhs_sample$irs_use,lhs_sample$irs_future), # # The first round covers 30% of the population and the second covers 80%. 
  ls_theta = matrix(2.025, nrow=length(sprayingtimesteps), ncol=1), # Matrix of mortality parameters; nrows=length(timesteps), ncols=length(species) 
  ls_gamma = matrix(-0.009, nrow=length(sprayingtimesteps), ncol=1), # Matrix of mortality parameters per round of IRS and per species
  ks_theta = matrix(-2.222, nrow=length(sprayingtimesteps), ncol=1), # Matrix of feeding success parameters per round of IRS and per species
  ks_gamma = matrix(0.008, nrow=length(sprayingtimesteps), ncol=1), # Matrix of feeding success parameters per round of IRS and per species
  ms_theta = matrix(-1.232, nrow=length(sprayingtimesteps), ncol=1), # Matrix of deterrence parameters per round of IRS and per species
  ms_gamma = matrix(-0.009, nrow=length(sprayingtimesteps), ncol=1) # Matrix of deterrence parameters per round of IRS and per species
)

# Set LSM

# Specify the LSM coverage
lsm_coverage <- lhs_sample$lsm

cc <- get_init_carrying_capacity(simparams)

# Set LSM by reducing the carrying capacity by (1 - coverage)
p_lsm <- simparams |>
  set_carrying_capacity(
    carrying_capacity = matrix(cc * (1 - lsm_coverage), ncol = 1),
    timesteps = 365
  )

# Set defaults for comparison

output_control <- run_simulation(timesteps = sim_length, parameters = simparams)


# Call simulation

output <- run_simulation(
    timesteps = sim_length,
    parameters = c(bednetparams, sprayingparams, p_lsm))

# Plot prevalence with both bednet and spraying interventions

pdf("prpf210.pdf")
plot_prev()
dev.off()