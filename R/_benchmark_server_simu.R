# this file can be used to run the benchmarking on a server
# for the random splines data
date()
source("R/create_base_learners.R")
source("R/create_opt_base_learners.R")
source("R/benchmark_simulation.R")
date()
# save benchmarking result
saveRDS(bmr, paste0("Benchmark_results/", Sys.Date(), "simu_bmr.RDS"))
saveRDS(bmr, "Benchmark_results/bmr.RDS")
