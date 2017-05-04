# Benchmark the learners on the UCR TSC data
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
# set.seed(1234)
# bmr = benchmark(learners = lrns,
#                 tasks = tsks,
#                 resamplings = res_instances,
#                 measures = list(multiclass.brier, mmce, timeboth))
# bmr

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

# Friedman Test
friedmanTestBMR(bmr, measure = multiclass.brier)

# Nemenyi Post-Hoc Test
friedmanPostHocTestBMR(bmr, measure = multiclass.brier)

# critical difference diagram
g = generateCritDifferencesData(bmr, measure = multiclass.brier,
                                p.value = 0.1, test = "nemenyi")
plotCritDifferences(g, pretty.names = FALSE) #+ coord_cartesian(xlim = c(-1,5), ylim = c(0,2))
