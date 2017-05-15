# this file can be used to run the benchmarking on a server
source("R/UCR_TSC_data.R")
source("R/create_base_learners.R")
source("R/tune_base_learners.R")
source("R/tune_opt_knn_nderiv_learners.R")
source("R/benchmark.R")

# save benchmarking result
saveRDS(bmr, paste0(Sys.Date(), "bmr.RDS"))
saveRDS(bmr, "bmr.RDS")
