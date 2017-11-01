# this is the R Code that will be used in Section 4 of the paper

################################################################################
### Section 4, usage through mlr-interface
# Section 4.1 Replication of Basic Functionality
# Chunk 1

# install the mlr package once
install.packages("mlr")
# use the version on github if my pull request is not yet merged
devtools::install_github("maierhofert/mlr", ref = "classiFunc")

# load the mlr package in every new R-session
library("mlr")

# create learners (= model description in mlr)
lrn.nn = makeLearner(cl = "classif.classiFunc.knn")
lrn.ker = makeLearner(cl = "classif.classiFunc.kernel",
                      ker = "Ker.epa", h = 0.3, nderiv = 1)


# Chunk 2

# load the example data
data("DTI", package = "classiFunc")

# export DTI data to mlr data format
fdata = makeFunctionalData(DTI, fd.features = list(rcst = "rcst", 
                                                   cca = "cca"))
# create mlr task from data
task = makeClassifTask(data = fdata, target = "case", blocking = DTI$ID)

# use same train/test split as in  Section 3
set.seed(1234)
IDs = unique(DTI$ID)
# vector encoding if observation is part of test or training data
train.rows = DTI$ID %in% sample(IDs, size = 0.95 * length(IDs))

# create separate tasks for test/train split
task.train = subsetTask(task, features = "rcst", subset = train.rows)
task.test = subsetTask(task, features = "rcst", subset = !train.rows)


# chunk 3

# create models (train learners on training data)
mod.nn = train(learner = lrn.nn, task = task.train)
mod.ker = train(learner = lrn.ker, task = task.train)


# chunk 4

# use trained models to predict test data
pred.nn = predict(mod.nn, task = task.test)
pred.ker = predict(mod.ker, task = task.test)


# chunk 5

# confusion matrix for nn estimator
table(pred = getPredictionResponse(pred.nn), 
      true = DTI$case[!train.rows])
# confusion matrix for kernel estimator
table(pred = getPredictionResponse(pred.ker), 
      true = DTI$case[!train.rows])

################################################################################
# Section 4.2 Automated Hyperparameter Tuning
# Chunk 1

# create the classiKernel learner for classification of functional data
lrn.ker = makeLearner("classif.classiFunc.kernel", 
                      predict.type = "prob")

# create parameter set
parSet.h = makeParamSet(
  makeNumericParam(id = "h", lower = -2, upper = 2, 
                   trafo = function(x) 10 ^ x))

# control for tuning hyper parameters
# use higher resolution in application
ctrl = makeTuneControlGrid(resolution = 15L)

# control for resampling, use 5 fold CV
rdesc = makeResampleDesc("CV", iters = 5)

# create the tuned learner
lrn.bandwidth.tuned = makeTuneWrapper(learner = lrn.ker, 
                                      resampling = rdesc,
                                      measures = brier,
                                      par.set = parSet.h,
                                      control = ctrl)

# train the model on the training data task
m.kern = train(lrn.bandwidth.tuned, task.train)


# Chunk 2

# predict the test data set
pred.kern = predict(m.kern, task = task.test)
# confusion matrix for kernel estimator
table(pred = getPredictionResponse(pred.ker), 
      true = getTaskTargets(task.test))


# Chunk 3

# get predicted probabilities
getPredictionProbabilities(pred.kern)


################################################################################
# Section 4.3 Creating Customized Ensembles


# Chunk 1

# create base learners
b.lrn1 = makeLearner("classif.classiFunc.knn", 
                     id = "L1.lrn",
                     par.vals = list(metric = "globMax"), 
                     predict.type = "prob")
b.lrn2 = makeLearner("classif.classiFunc.knn", 
                     id = "L2.lrn",
                     par.vals = list(metric = "L2"), 
                     predict.type = "prob")
b.lrn3 = makeLearner("classif.classiFunc.knn",
                     id = "supremum.lrn",
                     par.vals = list(metric = "supremum"), 
                     predict.type = "prob")


# Chunk 2

set.seed(1234)

# create LCE
# set resampling to 10 fold CV (default is LOO-CV) for faster run time.
LCE.lrn = makeStackedLearner(
  base.learners = list(b.lrn1, b.lrn2, b.lrn3), 
  predict.type = "prob", 
  resampling = makeResampleDesc("CV", iters = 10L),
  method = "classif.bs.optimal")

# create RFE
RFE.lrn = makeStackedLearner(
  base.learners = list(b.lrn1, b.lrn2, b.lrn3), 
  super.learner = "classif.randomForest",
  predict.type = "prob",
  method = "stack.cv", 
  resampling = makeResampleDesc("CV", iters = 10L,  stratify = F))

# train the models on the training data
LCE.m = train(LCE.lrn, task = task.train)
RFE.m = train(RFE.lrn, task = task.train)

################################################################################

# Chunk 3

# predict the test data set
ensemble.pred = predict(ensemble.m, task = task.test)
rf.ensemble.pred = predict(rf.ensemble.m, task = task.test)

# compute mean misclassification error
measureMMCE(getTaskTargets(task.test), getPredictionResponse(ensemble.pred))
measureMMCE(getTaskTargets(task.test), getPredictionResponse(rf.ensemble.pred))

getPredictionProbabilities(ensemble.pred)
getPredictionProbabilities(rf.ensemble.pred)

rf = rf.ensemble.m$learner.model$super.model$learner.model
varImpPlot(rf)
ensemble.m$learner.model$weights




