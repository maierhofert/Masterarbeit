# base learners
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
                               forbidden = parSet$forbidden[2:3])
parSet.jump = makeParamSet(params = parSet$pars[c("t1", "t2")])

# tune wrapped learner
res = makeResampleDesc("CV", iters = 5)
ctrl = makeTuneControlRandom(maxit = 10L)
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

# check that lrn.relAreas.tuned works with new mlr version
mod = train(learner = lrn.relAreas.tuned, task = tsks[[1]])
mod$learner.model$opt.result
