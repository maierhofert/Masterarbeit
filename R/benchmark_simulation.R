# Benchmark the learners on the simulated data

# read in simulated task
task1 = readRDS("Daten/Simulated Data/random_splines_task.RDS")

tsks = list(task1)

# resampling description
# # TODO: Increase to
# res = makeResampleDesc(method = "CV", predict = "test",
#                        stratify = TRUE,
#                        iters = 10L)

res = makeResampleDesc(method = "CV", predict = "test",
                       stratify = TRUE, iters = 2L)
# resampling instances
#  Stratification for tasks of type 'fdaclassif' not supported
set.seed(1234)
res_instances = lapply(tsks, makeResampleInstance, desc = res)


#################################################################
# start benchmarking
rm(bmr)

library("parallelMap")

# benchmark in parallel
# setting a seed does not seem to work
set.seed(1234, "L'Ecuyer")

parallelStartSocket(cpus = 4)
# export the dtw package
parallelLibrary("dtw")

bmr = benchmark(learners = c(lrns[1:2], list(lrn.shortEuclidean.tuned)),
                tasks = tsks,
                resamplings = res_instances,
                models = FALSE,
                keep.pred = FALSE,
                measures = list(multiclass.brier, mmce, 
                                timetrain, timepredict, timeboth))
bmr

parallelStop()



#################################################################
# visualize benchmark results
plotBMRBoxplots(bmr, measure = mmce, pretty.names = FALSE)
plotBMRBoxplots(bmr, measure = multiclass.brier, pretty.names = FALSE)