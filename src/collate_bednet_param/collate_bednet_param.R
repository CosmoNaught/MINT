# Load necessary libraries
library(dplyr)

# Load the data
D1load <- read.csv("pyrethroid_only_median_july2024.csv")
D2load <- read.csv("pyrethroid_pbo_median_july2024.csv")
D3load <- read.csv("pyrethroid_pyrrole_median_july2024.csv")

# Add a new column to indicate the net type and rename the columns
D1load <- D1load %>%
  mutate(net_type = "pyrethroid_only") %>%
  rename(dn0 = ERG_d_ITN0, rn0 = ERG_r_ITN0) %>%
  select(dn0, rn0, gamman, bioassay_surv, net_type)

D2load <- D2load %>%
  mutate(net_type = "pyrethroid_pbo") %>%
  rename(dn0 = ERG_d_ITN0, rn0 = ERG_r_ITN0) %>%
  select(dn0, rn0, gamman, bioassay_surv, net_type)

D3load <- D3load %>%
  mutate(net_type = "pyrethroid_pyrrole") %>%
  rename(dn0 = ERG_d_ITN0, rn0 = ERG_r_ITN0) %>%
  select(dn0, rn0, gamman, bioassay_surv, net_type)

# Combine the datasets
ITN_median_param <- bind_rows(D1load, D2load, D3load)

# Store dataframe
saveRDS(ITN_median_param, "bednet_params_raw.RDS")