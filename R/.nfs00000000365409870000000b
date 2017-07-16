# this file can be used to run the benchmarking on a server
date()
source("R/UCR_TSC_data.R")
source("R/create_base_learners.R")
# source("R/tune_base_learners.R")
source("R/create_opt_base_learners.R")
source("R/benchmark.R")
# source("R/benchmark_simulation.R")
# source("R/benchmark_simulation_warped.R")
date()
# save benchmarking result
saveRDS(bmr, paste0("Benchmark_results/", Sys.Date(), "bmr.RDS"))
saveRDS(bmr, "Benchmark_results/bmr.RDS")
