# this file creates learners form fuchs etal 2015 that need
# automated hyperparameter setting
# they are not inlcuded in the benchmark experiment of the thesis but are fully functional
library("mlr")

# base learners as proposed in fuchs etal
lrn.shortEuclidean = makeLearner(cl = "fdaclassif.classiKnn", 
                                 id = "knn1_shortEucl",
                                 par.vals = list(metric = "shortEuclidean"), 
                                 predict.type = "prob")
lrn.relAreas = makeLearner(cl = "fdaclassif.classiKnn",
                           id = "knn1_jump",
                           par.vals = list(metric = "relAreas"), 
                           predict.type = "prob")
lrn.jump = makeLearner(cl = "fdaclassif.classiKnn",
                       id = "knn1_jump",
                       par.vals = list(metric = "jump"), 
                       predict.type = "prob")

# parameter set
parSet = getParamSet(lrn.shortEuclidean)
parSet.shortEuclidean = makeParamSet(params = parSet$pars[c("dmin", "dmax")], 
                                     forbidden = parSet$forbidden[1])
parSet.relAreas = makeParamSet(params = parSet$pars[c("dmin1", "dmin2", "dmax1", "dmax2")], 
                               forbidden = parSet$forbidden[2])
parSet.jump = makeParamSet(params = parSet$pars[c("t1", "t2")])

# control for tuning hyper parameters
# ctrl = makeTuneControlRandom(maxit = 10L)
ctrl = makeTuneControlGrid(resolution = 8L)
res = makeResampleDesc("CV", iters = 5)

# tuned learners
lrn.shortEuclidean.tuned = makeTuneWrapper(learner = lrn.shortEuclidean, 
                                           resampling = res,
                                           measures = multiclass.brier,
                                           par.set = parSet.shortEuclidean,
                                           control = ctrl)
lrn.relAreas.tuned = makeTuneWrapper(learner = lrn.relAreas, 
                                     resampling = res,
                                     measures = multiclass.brier,
                                     par.set = parSet.relAreas,
                                     control = ctrl)
lrn.jump.tuned = makeTuneWrapper(learner = lrn.jump, 
                                 resampling = res,
                                 measures = multiclass.brier,
                                 par.set = parSet.jump,
                                 control = ctrl)

# # check that lrn.relAreas.tuned works
# mod = train(learner = lrn.relAreas.tuned, task = tsks[[1]])
# mod$learner.model$opt.result

auto_tune_lrns = list(lrn.jump.tuned, lrn.relAreas.tuned, lrn.shortEuclidean.tuned)
