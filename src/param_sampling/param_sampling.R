library(dplyr)
library(tidyr)
library(lhs)
library(scales)
library(ggplot2)
library(GGally)

# Set a seed for reproducibility
set.seed(123)

# Define parameters for the 'orderly' workflow management system
orderly2::orderly_parameters(run = NULL, output_grid = NULL)

# Determine sample sizes based on the 'run' parameter
# NOTE: we will ALWAYS include ALL corner permutations;
# LHS will fill the remainder up to the chosen total_samples.
if (run == "short_run") {
  total_samples <- 2^10          # 1,024 (will error if corners exceed this)
} else if (run == "long_run") {
  total_samples <- 2^14          # 65,536
} else {
  stop(paste0("Please provide either 'short_run' or 'long_run' as query, provided: ", run))
}

subset_samples <- 128  # number of samples to subset for grid sampling (used when output_grid=TRUE)

orderly2::orderly_dependency("collate_bednet_param", "latest()", c("bednet_params_raw.RDS" = "bednet_params_raw.RDS"))

# Load bednet parameters
bednet_params_raw <- readRDS("bednet_params_raw.RDS")

# Define parameter ranges
eir_range <- sort(10^(runif(16, log10(0.5), log10(500))))
dn0_min_non_zero <- min(bednet_params_raw$dn0[bednet_params_raw$dn0 > 0]) * 0.9
dn0_max <- max(bednet_params_raw$dn0) * 1.2
dn0_range <- sort(runif(10, dn0_min_non_zero, dn0_max))
Q0_range <- c(0.6, 1)
phi_bednets_range <- c(0.4, 0.95)
seasonal_range <- c(0, 1)
routine_range <- c(0, 1)
itn_use_range <- seq(0, 1, by = 0.2)
irs_use_range <- seq(0, 1, by = 0.2)
itn_future_range <- seq(0, 1, length.out = 5)
irs_future_range <- seq(0, 1, length.out = 5)
lsm_range <- c(seq(0, 1, length.out = 5), 0, 0.9)

# Collect all parameter ranges in a list for LHS
all_ranges <- list(
  eir = eir_range, 
  dn0_use = dn0_range, 
  dn0_future = dn0_range, 
  Q0 = Q0_range,
  phi_bednets = phi_bednets_range, 
  seasonal = seasonal_range, 
  routine = routine_range,
  itn_use = itn_use_range, 
  irs_use = irs_use_range, 
  itn_future = itn_future_range,
  irs_future = irs_future_range, 
  lsm = lsm_range
)

# ---------- CORNERS: include EVERY min/max permutation (no subsetting) ----------
corner_values <- lapply(all_ranges, function(r) c(min(r), max(r)))
all_corners <- do.call(expand.grid, c(corner_values, stringsAsFactors = FALSE))
corner_samples <- dplyr::as_tibble(all_corners)
n_corners <- nrow(corner_samples)

# Sanity check: ensure total_samples can accommodate corners
if (total_samples < n_corners) {
  stop(paste0(
    "Requested total_samples (", total_samples, 
    ") is less than number of corner permutations (", n_corners, "). ",
    "Increase total_samples or reduce dimensionality."
  ))
}

# ---------- LHS: fill the remainder up to total_samples ----------
n_lhs <- total_samples - n_corners

# Generate LHS only if needed
if (n_lhs > 0) {
  lhs_raw <- randomLHS(n_lhs, length(all_ranges))
  colnames(lhs_raw) <- names(all_ranges)

  # Scale the LHS samples to the desired ranges
  scaled_lhs_samples <- sapply(names(all_ranges), function(var) {
    range_values <- unlist(all_ranges[[var]])
    values <- lhs_raw[, var]

    # Binary variables
    binary_params <- c("seasonal", "routine")

    if (var %in% binary_params) {
      values <- round(values)
    } else if (var == "eir") {
      # LOG-UNIFORM for EIR across [min,max] of eir_range
      values <- 10^(scales::rescale(values, to = range(log10(range_values))))
    } else if (length(range_values) == 2 && all(range_values %in% c(0, 1))) {
      # Continuous [0,1]
      values <- scales::rescale(values, to = range(range_values))
    } else if (is.numeric(range_values)) {
      # Treat as continuous over [min,max]
      values <- scales::rescale(values, to = range(range_values))
    } else {
      # Fallback: map to discrete indices (not expected here)
      indices <- round(scales::rescale(values, to = c(1, length(range_values))))
      values <- range_values[indices]
    }
    return(values)
  }, simplify = "data.frame")

  lhs_scenarios <- dplyr::bind_rows(
    corner_samples,
    as.data.frame(scaled_lhs_samples)
  )
} else {
  # No LHS needed; only corners
  lhs_scenarios <- corner_samples
}

# Optional balanced half-size grid output
if (output_grid) {
  set.seed(1234)

  grid_cols <- c("eir","Q0","phi_bednets","seasonal","routine",
                 "dn0_use","dn0_future","itn_use","irs_use","itn_future","irs_future","lsm")
  corners <- dplyr::select(corner_samples, dplyr::all_of(grid_cols))
  n_corners_grid <- nrow(corners)

  # keep only half the corners
  keep_corners_n <- max(1, floor(n_corners_grid / 2))
  corners_half <- dplyr::sample_n(corners, keep_corners_n)

  # value sets
  V <- list(
    eir = sort(unique(eir_range)), Q0 = sort(unique(Q0_range)),
    phi_bednets = sort(unique(phi_bednets_range)),
    seasonal = sort(unique(seasonal_range)), routine = sort(unique(routine_range)),
    dn0_use = sort(unique(dn0_range)), dn0_future = sort(unique(dn0_range)),
    itn_use = sort(unique(itn_use_range)), irs_use = sort(unique(irs_use_range)),
    itn_future = sort(unique(itn_future_range)), irs_future = sort(unique(irs_future_range)),
    lsm = sort(unique(lsm_range))
  )

  # split EIR into low/medium/high (log scale bins)
  br <- 10^seq(log10(min(V$eir)), log10(max(V$eir)), length.out = 4)
  bins <- list(
    low    = V$eir[V$eir >= br[1] & V$eir <  br[2]],
    medium = V$eir[V$eir >= br[2] & V$eir <  br[3]],
    high   = V$eir[V$eir >= br[3] & V$eir <= br[4]]
  )
  if (length(bins$low)==0)    bins$low <- V$eir
  if (length(bins$medium)==0) bins$medium <- V$eir
  if (length(bins$high)==0)   bins$high <- V$eir

  # target the SAME number of non-corners as kept corners
  tgt_total <- keep_corners_n
  tgt <- c(low = floor(tgt_total/3), medium = floor(tgt_total/3))
  tgt <- c(tgt, high = tgt_total - sum(tgt))

  # sampler
  draw_block <- function(eir_vals, n) data.frame(
    eir = sample(eir_vals, n, TRUE),
    Q0 = sample(V$Q0, n, TRUE),
    phi_bednets = sample(V$phi_bednets, n, TRUE),
    seasonal = sample(V$seasonal, n, TRUE),
    routine  = sample(V$routine,  n, TRUE),
    dn0_use    = sample(V$dn0_use,    n, TRUE),
    dn0_future = sample(V$dn0_future, n, TRUE),
    itn_use    = sample(V$itn_use,    n, TRUE),
    irs_use    = sample(V$irs_use,    n, TRUE),
    itn_future = sample(V$itn_future, n, TRUE),
    irs_future = sample(V$irs_future, n, TRUE),
    lsm        = sample(V$lsm,        n, TRUE),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  # build non-corner sample without duplicates
  noncorners <- list()
  for (b in names(tgt)) {
    need <- tgt[[b]]; acc <- data.frame()
    while (nrow(acc) < need) {
      cand <- draw_block(bins[[b]], max(need - nrow(acc), 500))
      cand <- dplyr::anti_join(cand, corners, by = grid_cols)
      if (nrow(acc)) cand <- dplyr::anti_join(cand, acc, by = grid_cols)
      if (nrow(cand)) acc <- dplyr::bind_rows(acc, head(cand, need - nrow(acc)))
    }
    noncorners[[b]] <- acc
  }
  grid_noncorner <- dplyr::bind_rows(noncorners)

  # balanced half-size grid = half corners + same count of non-corners
  grid_scenarios <- dplyr::bind_rows(corners_half, grid_noncorner)
  write.csv(grid_scenarios, "grid_scenarios.csv", row.names = FALSE)

  # keep sampling step safe if subset_samples is larger than new grid
  subset_samples_grid <- min(subset_samples, nrow(grid_scenarios))
  grid_sample <- grid_scenarios %>% dplyr::sample_n(subset_samples_grid)
  write.csv(grid_sample, "grid_scenarios_sample.csv", row.names = FALSE)
}

# Write out the full scenarios (corners first, then LHS fill)
write.csv(lhs_scenarios, "lhs_scenarios.csv", row.names = FALSE)

# Write out a sample of the scenarios for debugging
subset_samples_lhs <- min(subset_samples, nrow(lhs_scenarios))
lhs_sample <- lhs_scenarios %>% dplyr::sample_n(subset_samples_lhs)
write.csv(lhs_sample, "lhs_scenarios_sample.csv", row.names = FALSE)

# Ensure the 'figs' folder exists
if(!dir.exists("figs")) {
  dir.create("figs")
}

# Take a subset of 2^10 (1024) samples for plotting (or fewer if not available)
plot_subset_size <- 2^10
if (nrow(lhs_scenarios) >= plot_subset_size) {
  lhs_scenarios_plot <- lhs_scenarios %>% dplyr::sample_n(plot_subset_size)
} else {
  lhs_scenarios_plot <- lhs_scenarios
}

pdf("figs/param_distributions.pdf", onefile = TRUE)
for (param in names(lhs_scenarios_plot)) {

  ## --- prep data: treat corners separately from LHS ----
  # indices: corners are first n_corners rows in lhs_scenarios by construction
  lhs_idx <- seq.int(n_corners + 1, nrow(lhs_scenarios))
  lhs_vals <- lhs_scenarios[[param]][lhs_idx]
  corner_vals <- corner_samples[[param]]

  lhs_df <- data.frame(value = lhs_vals, Type = "LHS")
  # unique min/max for clear corner lines on distribution panel
  corner_mm <- sort(unique(range(corner_vals, na.rm = TRUE)))
  corners_df_lines <- data.frame(value = corner_mm, Type = "Corner")

  # scatter data (downsample LHS for readability if huge)
  scatter_max <- 20000L
  if (length(lhs_idx) > scatter_max) {
    keep <- sample.int(length(lhs_idx), scatter_max)
    lhs_scatter <- data.frame(
      Index = lhs_idx[keep],
      value = lhs_vals[keep],
      Type  = "LHS"
    )
  } else {
    lhs_scatter <- data.frame(
      Index = lhs_idx,
      value = lhs_vals,
      Type  = "LHS"
    )
  }
  corners_scatter <- data.frame(
    Index = seq_len(n_corners),
    value = corner_vals,
    Type  = "Corner"
  )

  ## --- aesthetics & theme ---
  col_lhs    <- "#2E86AB"  # blue
  col_corner <- "#D7263D"  # red

  base_theme <- theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6))
    )

  ## --- top panel: histogram (bars) + density for LHS; corner min/max lines ---
  g1 <- ggplot(lhs_df, aes(x = value)) +
    geom_histogram(
      aes(y = after_stat(density)), bins = 50,
      fill = col_lhs, color = "grey80", alpha = 0.45
    ) +
    geom_density(aes(color = "LHS"), linewidth = 1.0, adjust = 1.0) +
    geom_vline(
      data = corners_df_lines,
      aes(xintercept = value, color = "Corner"),
      linewidth = 1.1, linetype = "longdash", alpha = 0.85
    ) +
    scale_color_manual(values = c("LHS" = col_lhs, "Corner" = col_corner)) +
    labs(
      title = paste("Distribution of", param),
      x = param, y = "Density"
    ) +
    base_theme

  ## --- bottom panel: scatter with alpha (LHS) and emphasized corners ---
  g2 <- ggplot() +
    geom_point(
      data = lhs_scatter,
      aes(x = Index, y = value, color = "LHS"),
      alpha = 0.20, size = 0.6, shape = 16
    ) +
    geom_point(
      data = corners_scatter,
      aes(x = Index, y = value, color = "Corner"),
      alpha = 0.95, size = 1.4, shape = 16
    ) +
    scale_color_manual(values = c("LHS" = col_lhs, "Corner" = col_corner)) +
    labs(
      title = paste("Scatter of", param, "values"),
      x = "Sample Index", y = param
    ) +
    base_theme

  ## --- log scaling for EIR ---
  if (identical(param, "eir")) {
    g1 <- g1 + geom_histogram(aes(y = after_stat(density)),
                              bins = 60, fill = col_lhs,
                              color = "grey80", alpha = 0.45)
  }

  ## --- render both panels on one page ---
  gridExtra::grid.arrange(g1, g2, ncol = 1, heights = c(0.55, 0.45))
}
dev.off()
