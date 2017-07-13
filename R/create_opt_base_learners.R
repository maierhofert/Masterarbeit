# this file creates learners with optimal values for
# knn and nderiv
library("mlr")

# hyper parameters for optimal nderiv/knn/semimetric
hp.ctrl = makeTuneControlGrid()
# tune wrapped learner
hp.res = makeResampleDesc("CV", iters = 5)

# parameter values
knn.vals = c(1L, 5L, 9L, 13L)
knn.pars = makeDiscreteLearnerParam("knn", knn.vals)
nderiv.vals = c(0L, 1L, 2L)
nderiv.pars = makeDiscreteLearnerParam("nderiv", nderiv.vals)
semimet.vals = c(0L, 1L, 2L)
semimet.pars = makeDiscreteLearnerParam("metric", semimet.vals)


# parameter sets
parSet.knn = makeParamSet(knn.pars)
parSet.nderiv = makeParamSet(nderiv.pars)
parSet.semimet = makeParamSet(semimet.pars)
parSet.knnNderivSemimet = makeParamSet(knn.pars, nderiv.pars, semimet.pars)

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
knnOptNderiv0_eucl$short.name = "Eucl: opt k; nderiv 0"


knn1NderivOpt_eucl = makeLearner(cl = "fdaclassif.classiKnn",
                                 id = "knn1NderivOpt_eucl",
                                 metric = "Euclidean",
                                 predict.type = "prob")
knn1NderivOpt_eucl = makeTuneWrapper(learner = knn1NderivOpt_eucl, 
                                     resampling = hp.res,
                                     measures = multiclass.brier,
                                     par.set = parSet.nderiv,
                                     control = hp.ctrl)
knn1NderivOpt_eucl$short.name = "Eucl: k 1; opt nderiv"


knn1Nderiv0_semimetOpt = makeLearner(cl = "fdaclassif.classiKnn",
                                 id = "knn1Nderiv0_semimetOpt",
                                 # metric = "Euclidean",
                                 predict.type = "prob")
knn1Nderiv0_semimetOpt = makeTuneWrapper(learner = knn1Nderiv0_semimetOpt, 
                                     resampling = hp.res,
                                     measures = multiclass.brier,
                                     par.set = parSet.semimet,
                                     control = hp.ctrl)
knn1Nderiv0_semimetOpt$short.name = "Opt semimet: k 1; nderiv 0"


knnOptNderivOptSemimetOpt = makeLearner(cl = "fdaclassif.classiKnn",
                                   id = "knnOptNderivOptSemimetOpt",
                                   # metric = "Euclidean",
                                   predict.type = "prob")
knnOptNderivOptSemimetOpt = makeTuneWrapper(learner = knnOptNderivOptSemimetOpt, 
                                       resampling = hp.res,
                                       measures = multiclass.brier,
                                       par.set = parSet.knnNderivSemimet,
                                       control = hp.ctrl)
knnOptNderivOptSemimetOpt$short.name = "Opt semimet: opt k; opt nderiv"


opt_lrns = list(knn1NderivOpt_eucl, knnOptNderiv0_eucl, knn1Nderiv0_semimetOpt,
                knnOptNderivOptSemimetOpt)
# # run test
# mod = train(knnOptNderivOpt_eucl, task1)
# mod$learner.model$opt.result

