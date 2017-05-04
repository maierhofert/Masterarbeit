# base learner
lrn.shortEuclidean = makeLearner(cl = "fdaclassif.classiKnn",
                                 par.vals = list(metric = "shortEuclidean"), 
                                 predict.type = "prob")
# parameter set
parSet = getParamSet(lrn.shortEuclidean)
parSet = makeParamSet(params = parSet$pars[c("dmin", "dmax")], 
                      forbidden = parSet$forbidden[1], 
                      keys = "task")

# evaluated parameter set
task = tsks[[2]]
parSetEval = evaluateParamExpressions(parSet, dict = list(task = task))

# tune wrapped learner
ctrl = makeTuneControlGrid()
lrn.shortEuclidean.tuned = makeTuneWrapper(learner = lrn.shortEuclidean, 
                                           resampling = makeResampleDesc("CV", iters = 5),
                                           measures = multiclass.brier,
                                           par.set = parSetEval,
                                           control = ctrl)
# trained model
mod = train(learner = lrn.shortEuclidean.tuned, task = task)
mod.eval


ctrl = makeTuneControlRandom(maxit = 30)

rdesc = makeResampleDesc("CV", iters = 3L)
res = tuneParams(lrn.shortEuclidean, task = task,
                 resampling = rdesc,
                 par.set = num_ps, control = ctrl)
