# this file can be used to run the benchmarking on a server
date()
source("R/create_base_learners.R")
source("R/benchmark_simulation_warped_srivastava.R")
date()
# save benchmarking result
saveRDS(bmr, paste0("Benchmark_results/", Sys.Date(), "simu_warped_srivastava_bmr.RDS"))
saveRDS(bmr, "Benchmark_results/bmr.RDS")
