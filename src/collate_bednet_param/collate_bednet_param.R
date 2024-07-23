# Load necessary libraries
library(spearMINT)
browser()
# Define file paths
file_paths <- list(
  "pyrethroid_only" = "pyrethroid_only_median_july2024.csv",
  "pyrethroid_pbo" = "pyrethroid_pbo_median_july2024.csv",
  "pyrethroid_pyrrole" = "pyrethroid_pyrrole_median_july2024.csv"
)

# Combine the datasets
ITN_median_param <- spearMINT::combine_itn_data(file_paths)

# Store dataframe
spearMINT::save_itn_data(ITN_median_param, "bednet_params_raw.RDS")