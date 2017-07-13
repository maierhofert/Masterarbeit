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



# ######################################################################
# define other semimetrics
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

# global min and global max
k1nd0_globMax = makeLearner(cl = "fdaclassif.classiKnn",
                            id = paste0("knn", 1, "nderiv", 0,
                                        "_globMax"),
                            metric = "globMax",
                            predict.type = "prob",
                            par.vals = list(knn = 1, nderiv = 0))
k1nd0_globMax$short.name = "globMax: k 1; nderiv 0"

k1nd0_globMin = makeLearner(cl = "fdaclassif.classiKnn",
                            id = paste0("knn", 1, "nderiv", 0,
                                        "_globMin"),
                            metric = "globMin",
                            predict.type = "prob",
                            par.vals = list(knn = 1, nderiv = 0))
k1nd0_globMin$short.name = "globMin: k 1; nderiv 0"

k1nd0_Manhattan = makeLearner(cl = "fdaclassif.classiKnn",
                              id = paste0("knn", 1, "nderiv", 0,
                                          "_manhattan"),
                              metric = "Manhattan",
                              predict.type = "prob",
                              par.vals = list(knn = 1, nderiv = 0))
k1nd0_Manhattan$short.name = "Manhattan: k 1; nderiv 0"

###############################################################################
# define range of knn and nderiv
knn = c(1L, 5L, 9L, 13L)
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

nderivKnn_manhattan_lrns = lapply(createHyperParVals(knn, nderiv), function(par.set) {
  makeLearner(cl = "fdaclassif.classiKnn",
              id = paste0("knn", par.set$knn, "nderiv", par.set$nderiv,
                          "_manhattan"),
              metric = "Manhattan",
              predict.type = "prob",
              par.vals = par.set)
})

nderivKnn_globMax_lrns = lapply(createHyperParVals(knn, nderiv), function(par.set) {
  makeLearner(cl = "fdaclassif.classiKnn",
              id = paste0("knn", par.set$knn, "nderiv", par.set$nderiv,
                          "_globMax"),
              metric = "globMax",
              predict.type = "prob",
              par.vals = par.set)
})

nderivKnn_globMin_lrns = lapply(createHyperParVals(knn, nderiv), function(par.set) {
  makeLearner(cl = "fdaclassif.classiKnn",
              id = paste0("knn", par.set$knn, "nderiv", par.set$nderiv,
                          "_globMin"),
              metric = "globMin",
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
knn_eucl_ensemble$short.name = "Eucl-ens: k 1, 3, 5, 7; nderiv 0"
  
nderiv_eucl_ensemble = makeStackedLearner(id = "nderiv_eucl_ensemble",
                                          base.learners = nderiv_eucl_lrns,
                                          predict.type = "prob",
                                          resampling = makeResampleDesc("CV", iters = 3L),
                                          method = "classif.bs.optimal")
nderiv_eucl_ensemble$short.name = "Eucl-ens: k 1; nderiv 0, 1, 2"

semimet_ensemble = makeStackedLearner(id = "semimet_ensemble",
                                       base.learners = list(k1nd0_eucl,
                                                         k1nd0_Manhattan,
                                                         k1nd0_globMax,
                                                         k1nd0_globMin),
                                       predict.type = "prob",
                                       resampling = makeResampleDesc("CV", iters = 3L),
                                       method = "classif.bs.optimal")
semimet_ensemble$short.name = "semimet-ens: k 1; nderiv 0"


# nderivKnn_eucl_ensemble = makeStackedLearner(id = "nderivKnn_eucl_ensemble",
#                                              base.learners = nderivKnn_eucl_lrns,
#                                              predict.type = "prob",
#                                              resampling = makeResampleDesc("CV", iters = 3L),
#                                              method = "classif.bs.optimal")
# nderivKnn_eucl_ensemble$short.name = "Eucl-ens: k 1, 3, 5, 7; nderiv 0, 1, 2"

nderivKnnSemimet_ensemble = makeStackedLearner(id = "nderivKnnSemimet_ensemble",
                                             base.learners = c(nderivKnn_eucl_lrns,
                                                               nderivKnn_manhattan_lrns,
                                                               nderivKnn_globMax_lrns,
                                                               nderivKnn_globMin_lrns),
                                             predict.type = "prob",
                                             resampling = makeResampleDesc("CV", iters = 3L),
                                             method = "classif.bs.optimal")
nderivKnnSemimet_ensemble$short.name = "semimet-ens: k 1, 3, 5, 7; nderiv 0, 1, 2"


# with random forest ensemble
# rf_feat_eucl_ensemble = makeStackedLearner(id = "rf_feat_eucl_ensemble",
#                                            base.learners = nderivKnn_eucl_lrns, 
#                                            super.learner = "classif.randomForest",
#                                            predict.type = "prob",
#                                            use.feat = TRUE,
#                                            # resampling = makeResampleDesc("CV", iters = 3L),
#                                            method = "stack.cv")
# rf_feat_eucl_ensemble$short.name = "Eucl-rf: k 1, 3, 5, 7; nderiv 0, 1, 2; use feat"
# 
# rf_nofeat_eucl_ensemble = makeStackedLearner(id = "rf_nofeat_eucl_ensemble",
#                                              base.learners = nderivKnn_eucl_lrns, 
#                                              super.learner = "classif.randomForest",
#                                              predict.type = "prob",
#                                              use.feat = FALSE,
#                                              # resampling = makeResampleDesc("CV", iters = 3L),
#                                              method = "stack.cv")
# rf_nofeat_eucl_ensemble$short.name = "Eucl-rf: k 1, 3, 5, 7; nderiv 0, 1, 2; no feat"

# random forest ensemble for different semimetrics
rf_feat_ensemble = makeStackedLearner(id = "rf_feat_semimet_ensemble",
                                           base.learners = c(nderivKnn_eucl_lrns,
                                                             nderivKnn_manhattan_lrns,
                                                             nderivKnn_globMax_lrns,
                                                             nderivKnn_globMin_lrns), 
                                           super.learner = "classif.randomForest",
                                           predict.type = "prob",
                                           use.feat = TRUE,
                                           # resampling = makeResampleDesc("CV", iters = 3L),
                                           method = "stack.cv")
rf_feat_ensemble$short.name = "rf-ens: k 1, 3, 5, 7; nderiv 0, 1, 2; use feat"

rf_nofeat_ensemble = makeStackedLearner(id = "rf_nofeat_semimet_ensemble",
                                             base.learners = c(nderivKnn_eucl_lrns,
                                                               nderivKnn_manhattan_lrns,
                                                               nderivKnn_globMax_lrns,
                                                               nderivKnn_globMin_lrns), 
                                             super.learner = "classif.randomForest",
                                             predict.type = "prob",
                                             use.feat = FALSE,
                                             # resampling = makeResampleDesc("CV", iters = 3L),
                                             method = "stack.cv")
rf_nofeat_ensemble$short.name = "rf-ens: k 1, 3, 5, 7; nderiv 0, 1, 2; no feat"



#  learners with a lot of random noisy base learners
random_knn_100 = list()
for(i in 1:100) {
  random_knn_100[[i]] = makeLearner(cl = "fdaclassif.classiKnn",
                                  id = paste0("noisy_learner", i),
                                  metric = "custom.metric",
                                  custom.metric = function(x, y) {
                                    runif(1)
                                  },
                                  predict.type = "prob")
}

rf_noisy_ensemble = makeStackedLearner(id = "rf_noisy_ensemble",
                                             base.learners = c(nderivKnn_eucl_lrns,
                                                               nderivKnn_manhattan_lrns,
                                                               nderivKnn_globMax_lrns,
                                                               nderivKnn_globMin_lrns,
                                                               random_knn_100), 
                                             super.learner = "classif.randomForest",
                                             predict.type = "prob",
                                             use.feat = FALSE,
                                             # resampling = makeResampleDesc("CV", iters = 3L),
                                             method = "stack.cv")
rf_noisy_ensemble$short.name = "rf-ens: noisy"

noisy_ensemble = makeStackedLearner(id = "noisy_eucl_ensemble",
                                             base.learners = c(nderivKnn_eucl_lrns,
                                                               nderivKnn_manhattan_lrns,
                                                               nderivKnn_globMax_lrns,
                                                               nderivKnn_globMin_lrns,
                                                               random_knn_100),
                                             predict.type = "prob",
                                             resampling = makeResampleDesc("CV", iters = 3L),
                                             method = "classif.bs.optimal")
noisy_ensemble$short.name = "nn-ens: noisy"



# list of learners to be compared together
reference_lrns = list(k1nd0_eucl, lrn.kernel.tuned, k1nd0_dtw)
ensemble_lrns = list(knn_eucl_ensemble,
                     nderiv_eucl_ensemble,
                     semimet_ensemble,
                     nderivKnnSemimet_ensemble,
                     rf_feat_ensemble,
                     rf_nofeat_ensemble)
noisy_lrns = list(rf_noisy_ensemble, noisy_ensemble)
warped_lrns = list(k1nd0_amplitude, k1nd0_phase)


# # train learners
# # This is not needed for benchmarking
# mod1 = train(learner = rf_noisy_ensemble, task = tsks[[1]])
# mod2 = train(learner = noisy_ensemble, task = tsks[[2]])
# fda.pred = predict(mod1, task = tsks[[1]])
# fda.pred2 = predict(mod2, task = tsks[[2]])

