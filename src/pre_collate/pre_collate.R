library(spearMINT)
library(orderly2)

orderly2::orderly_parameters(
    indices = NULL,
    verbose = NULL,
    parallel = NULL,
    store_output = NULL)

# Split the indices string if it was passed as a comma-separated string
indices <- as.integer(unlist(strsplit(indices, ",")))

x <- spearMINT::orderly_prod(indices = indices,
                            verbose = verbose, 
                            parallel = parallel, 
                            store_output = store_output)

saveRDS(x, "ids.RDS")