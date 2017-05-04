library("mlr")
# benchmark classifiers
k1nd0_eucl = makeLearner(cl = "fdaclassif.classiKnn",
                         id = paste0("knn", 1, "nderiv", 0,
                                     "_eucl"),
                         metric = "Euclidean",
                         predict.type = "prob",
                         par.vals = list(knn = 1, nderiv = 0))

library("dtw")
k1nd0_dtw = makeLearner(cl = "fdaclassif.classiKnn",
                        id = paste0("knn", 1, "nderiv", 0,
                                    "_dtw"),
                        metric = "dtw",
                        predict.type = "prob",
                        par.vals = list(knn = 1, nderiv = 0))
benchmark_classifiers = list(k1nd0_eucl, k1nd0_dtw)
benchmark_classifiers = list(k1nd0_eucl)

# define range of knn and nderiv
knn = c(1L, 3L, 5L)
nderiv = c(0L, 1L)

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

nderiv_eucl_ensemble = makeStackedLearner(id = "nderiv_eucl_ensemble",
                                          base.learners = nderiv_eucl_lrns,
                                          predict.type = "prob",
                                          resampling = makeResampleDesc("CV", iters = 3L),
                                          method = "classif.bs.optimal")

nderivKnn_eucl_ensemble = makeStackedLearner(id = "nderivKnn_eucl_ensemble",
                                             base.learners = nderivKnn_eucl_lrns,
                                             predict.type = "prob",
                                             resampling = makeResampleDesc("CV", iters = 3L),
                                             method = "classif.bs.optimal")

# with random forest ensemble
rf_feat_eucl_ensemble = makeStackedLearner(id = "rf_feat_eucl_ensemble",
                                           base.learners = nderivKnn_eucl_lrns, 
                                           super.learner = "classif.randomForest",
                                           predict.type = "prob",
                                           use.feat = TRUE,
                                           # resampling = makeResampleDesc("CV", iters = 3L),
                                           method = "stack.cv")

rf_nofeat_eucl_ensemble = makeStackedLearner(id = "rf_nofeat_eucl_ensemble",
                                             base.learners = nderivKnn_eucl_lrns, 
                                             super.learner = "classif.randomForest",
                                             predict.type = "prob",
                                             use.feat = FALSE,
                                             # resampling = makeResampleDesc("CV", iters = 3L),
                                             method = "stack.cv")

# list of all learners to be compared
lrns = c(benchmark_classifiers,
         list(knn_eucl_ensemble,
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

