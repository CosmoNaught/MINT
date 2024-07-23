# Load necessary libraries
library(dplyr)

# Function to read CSV and apply transformations
load_itn_data <- function(file_path, net_type) {
  data <- read.csv(file_path)
  data <- data %>%
    mutate(net_type = net_type) %>%
    rename(dn0 = ERG_d_ITN0, rn0 = ERG_r_ITN0) %>%
    select(dn0, rn0, gamman, bioassay_surv, net_type)
  return(data)
}

# Load the data
D1load <- load_itn_data("pyrethroid_only_median_july2024.csv", "pyrethroid_only")
D2load <- load_itn_data("pyrethroid_pbo_median_july2024.csv", "pyrethroid_pbo")
D3load <- load_itn_data("pyrethroid_pyrrole_median_july2024.csv", "pyrethroid_pyrrole")

# Combine the datasets
ITN_median_param <- bind_rows(D1load, D2load, D3load)

# Store dataframe
saveRDS(ITN_median_param, "bednet_params_raw.RDS")