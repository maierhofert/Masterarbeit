# this file can be used to run the benchmarking on a server
# for the random trigonometric data
date()
source("R/create_base_learners.R")
source("R/benchmark_simulation_warped.R")
date()
# save benchmarking result
saveRDS(bmr, paste0("Benchmark_results/", Sys.Date(), "simu_warped_bmr.RDS"))
saveRDS(bmr, "Benchmark_results/bmr.RDS")
