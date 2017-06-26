# Benchmark the learners on the simulated data

# read in simulated task
tsk_list = list.files("Daten/Simulated Data/random_splines/", pattern = "task", 
                      full.names = TRUE)
tsks = lapply(tsk_list, readRDS)

# resampling description
# on local pc run a smaller benchmark
on_server = (.Platform$OS.type != "windows")
if (on_server) {
  res = makeResampleDesc(method = "CV", predict = "test",
                         stratify = TRUE,
                         iters = 10L)
  # lrns = lrns[c(1, 6)]
  # tsks = tsks[1:2]
} else {
  res = makeResampleDesc(method = "CV", predict = "test",
                         stratify = TRUE, iters = 2L)
  lrns = lrns[c(1, 6)]
  tsks = tsks[1:2]
}

# resampling instances
#  Stratification for tasks of type 'fdaclassif' not supported
set.seed(1234)
res_instances = lapply(tsks, makeResampleInstance, desc = res)


#################################################################
# start benchmarking
rm(bmr)

library("parallelMap")

# benchmark in parallel
if (on_server) {
  parallelStartSocket(cpus = 10, level = "mlr.resample")
  parallelLibrary("dtw", level = "mlr.resample")
} else {
  parallelStartSocket(cpus = 4, level = "mlr.resample")
  parallelLibrary("dtw", level = "mlr.resample")
}


# set a seed for reproducibility
parallel::clusterSetRNGStream(iseed = 42)

bmr = benchmark(learners = c(lrns, opt_knn_nderiv_learners),
                tasks = tsks,
                resamplings = res_instances,
                models = FALSE,
                keep.pred = FALSE,
                measures = list(multiclass.brier, mmce, 
                                timetrain, timepredict, timeboth))
bmr

parallelStop()
