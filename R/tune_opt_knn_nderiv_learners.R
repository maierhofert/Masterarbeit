# this file creates learners with optimal values for
# knn and nderiv
library("mlr")

# hyper parameters for optimal nderiv/knn/semimetric
hp.ctrl = makeTuneControlGrid()
# tune wrapped learner
hp.res = makeResampleDesc("CV", iters = 5)

# parameter values
knn.vals = c(1, 3, 5, 7)
knn.pars = makeDiscreteLearnerParam("knn", knn.vals)
nderiv.vals = c(0, 1, 2)
nderiv.pars = makeDiscreteLearnerParam("nderiv", nderiv.vals)

# parameter sets
parSet.knn = makeParamSet(knn.pars)
parSet.nderiv = makeParamSet(nderiv.pars)
parSet.knnNderiv = makeParamSet(knn.pars, nderiv.pars)

# base learners with optimal nderiv/knn/semimetric
knnOptNderiv0_eucl = makeLearner(cl = "fdaclassif.classiKnn",
                                 id = "knnOptNderiv0_eucl",
                                 metric = "Euclidean",
                                 predict.type = "prob")
knnOptNderiv0_eucl = makeTuneWrapper(learner = knnOptNderiv0_eucl, 
                                     resampling = hp.res,
                                     measures = multiclass.brier,
                                     par.set = parSet.knn,
                                     control = hp.ctrl)

knn1NderivOpt_eucl = makeLearner(cl = "fdaclassif.classiKnn",
                                 id = "knn1NderivOpt_eucl",
                                 metric = "Euclidean",
                                 predict.type = "prob")
knn1NderivOpt_eucl = makeTuneWrapper(learner = knn1NderivOpt_eucl, 
                                     resampling = hp.res,
                                     measures = multiclass.brier,
                                     par.set = parSet.nderiv,
                                     control = hp.ctrl)

knnOptNderivOpt_eucl = makeLearner(cl = "fdaclassif.classiKnn",
                                   id = "knnOptNderivOpt_eucl",
                                   metric = "Euclidean",
                                   predict.type = "prob")
knnOptNderivOpt_eucl = makeTuneWrapper(learner = knnOptNderivOpt_eucl, 
                                       resampling = hp.res,
                                       measures = multiclass.brier,
                                       par.set = parSet.knnNderiv,
                                       control = hp.ctrl)

opt_knn_nderiv_learners = list(knn1NderivOpt_eucl, knnOptNderiv0_eucl, 
                               knnOptNderivOpt_eucl)
# # run test
# mod = train(knnOptNderivOpt_eucl, task1)
# mod$learner.model$opt.result

