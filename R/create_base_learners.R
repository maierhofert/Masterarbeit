library("mlr")
# benchmark classifiers
k1nd0_eucl = makeLearner(cl = "fdaclassif.classiKnn",
                         id = paste0("knn", 1, "nderiv", 0,
                                     "_eucl"),
                         metric = "Euclidean",
                         predict.type = "prob",
                         par.vals = list(knn = 1, nderiv = 0))
k1nd0_eucl$short.name = "Eucl: k 1; nderiv 0"
  
# create a dtw learner
library("dtw")
k1nd0_dtw = makeLearner(cl = "fdaclassif.classiKnn",
                        id = paste0("knn", 1, "nderiv", 0,
                                    "_dtw"),
                        metric = "dtw",
                        predict.type = "prob",
                        par.vals = list(knn = 1, nderiv = 0))
k1nd0_dtw$short.name = "dtw: k 1; nderiv 0"

# create the phase and amplitude distance learners
k1nd0_phase = makeLearner(cl = "fdaclassif.classiKnn",
                        id = paste0("knn", 1, "nderiv", 0,
                                    "_phase"),
                        metric = "phaseDistance",
                        predict.type = "prob",
                        par.vals = list(knn = 1, nderiv = 0))
k1nd0_phase$short.name = "phase: k 1; nderiv 0"
k1nd0_amplitude = makeLearner(cl = "fdaclassif.classiKnn",
                          id = paste0("knn", 1, "nderiv", 0,
                                      "_amplitude"),
                          metric = "amplitudeDistance",
                          predict.type = "prob",
                          par.vals = list(knn = 1, nderiv = 0))
k1nd0_amplitude$short.name = "amplitude: k 1; nderiv 0"

lrn.kernel = makeLearner("fdaclassif.classiKernel", predict.type = "prob")
# create parameter set
parSet.bandwidth = makeParamSet(
  makeNumericParam(id = "h", lower = -1, upper = 4, 
                   trafo = function(x) 10^x)
)
# control for tuning hyper parameters
# Use higher resolution in application
ctrl = makeTuneControlGrid(resolution = 20L)
# tuned learners
lrn.bandwidth.tuned = makeTuneWrapper(learner = lrn.kernel, 
                                      resampling = makeResampleDesc("CV", iters = 5),
                                      measures = mmce,
                                      par.set = parSet.bandwidth,
                                      control = ctrl)
lrn.kernel.tuned = lrn.bandwidth.tuned
lrn.kernel.tuned$short.name = "Eucl-Kernel: h CV-opt"
benchmark_classifiers = list(k1nd0_eucl, k1nd0_dtw, lrn.kernel.tuned)


# ######################################################################
# define range of knn and nderiv
knn = c(1L, 3L, 5L, 7L)
nderiv = c(0L, 1L, 2L)

# function to create a list of all hyperparameter combinations
createHyperParVals = function(knn, nderiv) {
  hyperpar.vals = list()
  for(i in 1:length(knn)) {
    for(j in 1:length(nderiv)) {
      hyperpar.vals[[(i - 1)*length(nderiv) + j]] = list(knn = knn[i], nderiv = nderiv[j])
    }
  }
  return(hyperpar.vals)
}

knn_eucl_lrns = lapply(createHyperParVals(knn, nderiv[1]), function(par.set) {
  makeLearner(cl = "fdaclassif.classiKnn",
              id = paste0("knn", par.set$knn, "nderiv", par.set$nderiv,
                          "_eucl"),
              metric = "Euclidean",
              predict.type = "prob",
              par.vals = par.set)
})

nderiv_eucl_lrns = lapply(createHyperParVals(knn[1], nderiv), function(par.set) {
  makeLearner(cl = "fdaclassif.classiKnn",
              id = paste0("knn", par.set$knn, "nderiv", par.set$nderiv,
                          "_eucl"),
              metric = "Euclidean",
              predict.type = "prob",
              par.vals = par.set)
})

nderivKnn_eucl_lrns = lapply(createHyperParVals(knn, nderiv), function(par.set) {
  makeLearner(cl = "fdaclassif.classiKnn",
              id = paste0("knn", par.set$knn, "nderiv", par.set$nderiv,
                          "_eucl"),
              metric = "Euclidean",
              predict.type = "prob",
              par.vals = par.set)
})


# Ensemble learners
# with knne Fuchs etal 2016
knn_eucl_ensemble = makeStackedLearner(id = "knn_eucl_ensemble",
                                       base.learners = knn_eucl_lrns,
                                       predict.type = "prob",
                                       resampling = makeResampleDesc("CV", iters = 3L),
                                       method = "classif.bs.optimal")
knn_eucl_ensemble$short.name = "Eucl-ensemble: k 1, 3, 5, 7; nderiv 0"
  
nderiv_eucl_ensemble = makeStackedLearner(id = "nderiv_eucl_ensemble",
                                          base.learners = nderiv_eucl_lrns,
                                          predict.type = "prob",
                                          resampling = makeResampleDesc("CV", iters = 3L),
                                          method = "classif.bs.optimal")
nderiv_eucl_ensemble$short.name = "Eucl-ensemble: k 1; nderiv 0, 1, 2"
  
nderivKnn_eucl_ensemble = makeStackedLearner(id = "nderivKnn_eucl_ensemble",
                                             base.learners = nderivKnn_eucl_lrns,
                                             predict.type = "prob",
                                             resampling = makeResampleDesc("CV", iters = 3L),
                                             method = "classif.bs.optimal")
nderivKnn_eucl_ensemble$short.name = "Eucl-ensemble: k 1, 3, 5, 7; nderiv 0, 1, 2"

# with random forest ensemble
rf_feat_eucl_ensemble = makeStackedLearner(id = "rf_feat_eucl_ensemble",
                                           base.learners = nderivKnn_eucl_lrns, 
                                           super.learner = "classif.randomForest",
                                           predict.type = "prob",
                                           use.feat = TRUE,
                                           # resampling = makeResampleDesc("CV", iters = 3L),
                                           method = "stack.cv")
rf_feat_eucl_ensemble$short.name = "Eucl-rf: k 1, 3, 5, 7; nderiv 0, 1, 2; use feat"
  
rf_nofeat_eucl_ensemble = makeStackedLearner(id = "rf_nofeat_eucl_ensemble",
                                             base.learners = nderivKnn_eucl_lrns, 
                                             super.learner = "classif.randomForest",
                                             predict.type = "prob",
                                             use.feat = FALSE,
                                             # resampling = makeResampleDesc("CV", iters = 3L),
                                             method = "stack.cv")
rf_nofeat_eucl_ensemble$short.name = "Eucl-rf: k 1, 3, 5, 7; nderiv 0, 1, 2; no feat"

# list of all learners to be compared
lrns = c(benchmark_classifiers,
         list(k1nd0_phase, k1nd0_amplitude,
              knn_eucl_ensemble,
              nderiv_eucl_ensemble,
              nderivKnn_eucl_ensemble,
              rf_nofeat_eucl_ensemble,
              rf_feat_eucl_ensemble))


# # train learners
# # This is not needed for benchmarking
# mod1 = train(learner = k1nn_eucl, task = tsks[[1]])
# mod2 = train(learner = k3nn_eucl, task = tsks[[2]])
# fda.pred = predict(mod1, task = tsks[[1]])
# fda.pred2 = predict(mod2, task = tsks[[2]])

