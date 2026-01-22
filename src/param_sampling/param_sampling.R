library(dplyr)

set.seed(42)

# =============================================================================
# orderly CONFIGURATION
# =============================================================================

o_pam <- orderly::orderly_parameters(run = NULL)

if (run == "short_run") {
  total_samples <- 2^10  # 1,024
} else if (run == "long_run") {
  total_samples <- 2^12  # 4,096
} else {
  stop(paste0("Please provide either 'short_run' or 'long_run', provided: ", run))
}

orderly::orderly_dependency(
  "collate_bednet_param",
  "latest()",
  c("bednet_params_raw.RDS" = "bednet_params_raw.RDS")
)

bednet_params_raw <- readRDS("bednet_params_raw.RDS")

# =============================================================================
# PARAMETER RANGES
# =============================================================================

# dn0 bounds from bednet data
dn0_min <- min(bednet_params_raw$dn0[bednet_params_raw$dn0 > 0]) * 0.9
dn0_max <- max(bednet_params_raw$dn0) * 1.2

# Continuous ranges
Q0_RANGE <- c(0.6, 1.0)
PHI_BEDNETS_RANGE <- c(0.4, 0.95)
COVERAGE_RANGE <- c(0, 1)

# =============================================================================
# STRATIFICATION STRUCTURE
# =============================================================================

# EIR bins (log-uniform sampling within each)
EIR_BINS <- list(
  `0-1`    = c(0.5, 1),
  `1-3`    = c(1, 3),
  `3-5`    = c(3, 5),
  `5-10`   = c(5, 10),
  `10-25`  = c(10, 25),
  `25-100` = c(25, 100),
  `100+`   = c(100, 500)
)

# dn0 bins (uniform sampling within each)
DN0_BINS <- list(
  low    = c(dn0_min, 0.2),
  medium = c(0.2, 0.4),
  high   = c(0.4, dn0_max)
)

# Intervention types: which interventions are active (non-zero)
INTERVENTION_TYPES <- list(
  none       = list(itn = FALSE, irs = FALSE, lsm = FALSE),
  itn_only   = list(itn = TRUE,  irs = FALSE, lsm = FALSE),
  irs_only   = list(itn = FALSE, irs = TRUE,  lsm = FALSE),
  lsm_only   = list(itn = FALSE, irs = FALSE, lsm = TRUE),
  itn_irs    = list(itn = TRUE,  irs = TRUE,  lsm = FALSE),
  itn_lsm    = list(itn = TRUE,  irs = FALSE, lsm = TRUE),
  irs_lsm    = list(itn = FALSE, irs = TRUE,  lsm = TRUE),
  triple     = list(itn = TRUE,  irs = TRUE,  lsm = TRUE)
)

# =============================================================================
# SAMPLING FUNCTIONS
# =============================================================================

sample_uniform <- function(n, range) runif(n, range[1], range[2])
sample_log_uniform <- function(n, range) 10^runif(n, log10(range[1]), log10(range[2]))

# =============================================================================
# GENERATE STRATIFIED SCENARIOS
# =============================================================================

n_cells <- length(EIR_BINS) * length(DN0_BINS) * length(INTERVENTION_TYPES)
samples_per_cell <- floor(total_samples / n_cells)
remainder <- total_samples - (samples_per_cell * n_cells)

cat(sprintf("Generating %d scenarios across %d cells (~%d per cell)\n",
            total_samples, n_cells, samples_per_cell))

all_scenarios <- list()
cell_idx <- 1

for (eir_name in names(EIR_BINS)) {
  for (dn0_name in names(DN0_BINS)) {
    for (int_name in names(INTERVENTION_TYPES)) {

      n_this_cell <- samples_per_cell + (if (cell_idx <= remainder) 1 else 0)
      int_type <- INTERVENTION_TYPES[[int_name]]

      # EIR: log-uniform within bin
      eir <- sample_log_uniform(n_this_cell, EIR_BINS[[eir_name]])

      # dn0_use and dn0_future: sampled INDEPENDENTLY within bin
      dn0_use <- sample_uniform(n_this_cell, DN0_BINS[[dn0_name]])
      dn0_future <- sample_uniform(n_this_cell, DN0_BINS[[dn0_name]])

      # Intervention coverages: sampled INDEPENDENTLY for _use and _future
      # Active interventions get U[0,1], inactive get 0
      if (int_type$itn) {
        itn_use <- sample_uniform(n_this_cell, COVERAGE_RANGE)
        itn_future <- sample_uniform(n_this_cell, COVERAGE_RANGE)
      } else {
        itn_use <- rep(0, n_this_cell)
        itn_future <- rep(0, n_this_cell)
      }

      if (int_type$irs) {
        irs_use <- sample_uniform(n_this_cell, COVERAGE_RANGE)
        irs_future <- sample_uniform(n_this_cell, COVERAGE_RANGE)
      } else {
        irs_use <- rep(0, n_this_cell)
        irs_future <- rep(0, n_this_cell)
      }

      # LSM: only has future (starts at year 9)
      if (int_type$lsm) {
        lsm <- sample_uniform(n_this_cell, COVERAGE_RANGE)
      } else {
        lsm <- rep(0, n_this_cell)
      }

      scenarios <- data.frame(
        eir = eir,
        dn0_use = dn0_use,
        dn0_future = dn0_future,
        Q0 = sample_uniform(n_this_cell, Q0_RANGE),
        phi_bednets = sample_uniform(n_this_cell, PHI_BEDNETS_RANGE),
        seasonal = sample(0:1, n_this_cell, replace = TRUE),
        routine = sample(0:1, n_this_cell, replace = TRUE),
        itn_use = itn_use,
        irs_use = irs_use,
        itn_future = itn_future,
        irs_future = irs_future,
        lsm = lsm
      )

      all_scenarios[[cell_idx]] <- scenarios
      cell_idx <- cell_idx + 1
    }
  }
}

lhs_scenarios <- bind_rows(all_scenarios)

# Shuffle to avoid any ordering artifacts
lhs_scenarios <- lhs_scenarios[sample(nrow(lhs_scenarios)), ]

# =============================================================================
# OUTPUT
# =============================================================================

write.csv(lhs_scenarios, "lhs_scenarios.csv", row.names = FALSE)

cat(sprintf("\nGenerated: lhs_scenarios.csv (%d scenarios)\n", nrow(lhs_scenarios)))
