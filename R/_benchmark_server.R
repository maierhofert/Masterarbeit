# this file can be used to run the benchmarking on a server
source("R/UCR_TSC_data.R")
source("R/create_base_learners.R")
source("R/tune_base_learners.R")
source("R/tune_opt_knn_nderiv_learners.R")
source("R/benchmark.R")
# source("R/benchmark_simulation.R")

# save benchmarking result
saveRDS(bmr, paste0("Benchmark_results/", Sys.Date(), "_simu_trigonometric_bmr.RDS"))
saveRDS(bmr, "Benchmark_results/bmr.RDS")
