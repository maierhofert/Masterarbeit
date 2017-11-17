# this is the R Code that will be used in Section 4 of the paper

################################################################################
### Section 4, usage through mlr-interface
# Section 4.1 Replication of Basic Functionality
# Chunk 1

# install mlr package once
install.packages("mlr")
# use the version on github if my pull request is not yet merged
devtools::install_github("maierhofert/mlr", ref = "classiFunc")

# load mlr package in every new R-session
library("mlr")

# create learners (= model description in mlr)
lrn.nn = makeLearner(cl = "classif.classiFunc.knn")
lrn.ker = makeLearner(cl = "classif.classiFunc.kernel",
                      ker = "Ker.epa", h = 0.7, nderiv = 1)


# Chunk 2

# load example data
data("DTI", package = "classiFunc")

# subsample DTI for equal case control size
DTI = DTI[!duplicated(DTI$ID),]
DTI = DTI[1:84,]

# export DTI data to mlr data format
fdata = makeFunctionalData(DTI, fd.features = list(rcst = "rcst", 
                                                   cca = "cca"))
# create mlr task from data
task = makeClassifTask(data = fdata, target = "case")

# use same train/test split as in  Section 3
set.seed(12345)
IDs = unique(DTI$ID)
# vector encoding if observation is part of test or training data
train.rows = DTI$ID %in% sample(IDs, size = 0.8 * length(IDs))

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
      true = DTI[!train.rows, "case"])
# confusion matrix for kernel estimator
table(pred = getPredictionResponse(pred.ker), 
      true = DTI[!train.rows, "case"])

################################################################################
# Section 4.2 Automated Hyperparameter Tuning
# Chunk 1

# create classiKernel learner for classification of functional data
lrn.ker = makeLearner("classif.classiFunc.kernel", 
                      nderiv = 1,
                      predict.type = "prob")

# create parameter set
parSet.h = makeParamSet(
  makeNumericParam(id = "h", lower = -1, upper = 1, 
                   trafo = function(x) 10 ^ x))

# control for tuning hyper parameters
# use higher resolution in application
ctrl = makeTuneControlGrid(resolution = 20L)

# control for resampling, use 5 fold CV
rdesc = makeResampleDesc("CV", iters = 5)

# create tuned learner
set.seed(12345)
lrn.bandwidth.tuned = makeTuneWrapper(learner = lrn.ker, 
                                      resampling = rdesc,
                                      measures = brier,
                                      par.set = parSet.h,
                                      control = ctrl)

# train model on training data task
m.kern.tuned = train(lrn.bandwidth.tuned, task.train)


# Chunk 2

# predict test data set
pred.kern.tuned = predict(m.kern.tuned, task = task.test)
# confusion matrix for kernel estimator
table(pred = getPredictionResponse(pred.kern.tuned), 
      true = getTaskTargets(task.test))


# Chunk 3
# get predicted probabilities
getPredictionProbabilities(pred.kern.tuned)


################################################################################
# Section 4.3 Creating Customized Ensembles


# Chunk 1

# create base learners
b.lrn1 = makeLearner("classif.classiFunc.knn", 
                     id = "globMin",
                     par.vals = list(metric = "globMin"), 
                     predict.type = "prob")

b.lrn2 = makeLearner("classif.classiFunc.knn", 
                     id = "globMax",
                     par.vals = list(metric = "globMax"), 
                     predict.type = "prob")
library("dtw")
b.lrn3 = makeLearner("classif.classiFunc.knn", 
                     id = "dtw",
                     par.vals = list(metric = "dtw"), 
                     predict.type = "prob")

b.lrn4 = makeLearner("classif.classiFunc.knn",
                     id = "L2",
                     par.vals = list(metric = "L2", nderiv = 1), 
                     predict.type = "prob")

b.lrn5 = makeLearner("classif.classiFunc.knn",
                     id = "sup",
                     par.vals = list(metric = "supremum"), 
                     predict.type = "prob")


b.lrn6 = makeLearner("classif.classiFunc.knn",
                     id = "globMin1",
                     par.vals = list(metric = "globMin", nderiv = 1), 
                     predict.type = "prob")

b.lrn7 = makeLearner("classif.classiFunc.knn",
                     id = "globMax1",
                     par.vals = list(metric = "globMax", nderiv = 1), 
                     predict.type = "prob")

b.lrn8 = makeLearner("classif.classiFunc.knn",
                     id = "sup1",
                     par.vals = list(metric = "supremum", nderiv = 1), 
                     predict.type = "prob")
# Chunk 2

set.seed(1234)

# create LCE
# set resampling to 10 fold CV (default is LOO-CV) for faster run time.
LCE.lrn = makeStackedLearner(
  base.learners = list(b.lrn1, b.lrn2, b.lrn3, b.lrn4, b.lrn5, b.lrn6, b.lrn7, b.lrn8), 
  predict.type = "prob", 
  resampling = makeResampleDesc("CV", iters = 10L),
  method = "classif.bs.optimal")

# create RFE
RFE.lrn = makeStackedLearner(
  base.learners = list(b.lrn1, b.lrn2, b.lrn3, b.lrn4, b.lrn5, b.lrn6, b.lrn7, b.lrn8), 
  super.learner = "classif.randomForest",
  predict.type = "prob",
  method = "stack.cv", 
  # use.feat = TRUE,
  resampling = makeResampleDesc("CV", iters = 10L))


# Chunk 3

# train models on the training data
LCE.m = train(LCE.lrn, task = task.train)
RFE.m = train(RFE.lrn, task = task.train)


# Chunk 4

# predict test data set
LCE.pred = predict(LCE.m, task = task.test)
RFE.pred = predict(RFE.m, task = task.test)

# confusion matrix for LCE
table(pred = getPredictionResponse(LCE.pred), 
      true = getTaskTargets(task.test))
# confusion matrix for RFE
table(pred = getPredictionResponse(RFE.pred), 
      true = getTaskTargets(task.test))

##############################
# MISC
# compute mean misclassification error
measureMMCE(getTaskTargets(task.test), getPredictionResponse(LCE.pred))
measureMMCE(getTaskTargets(task.test), getPredictionResponse(RFE.pred))

# getPredictionProbabilities(LCE.pred)
# getPredictionProbabilities(RFE.pred)


# plots

library("randomForest")
rf = RFE.m$learner.model$super.model$learner.model
varImpPlot(rf)
plot(LCE.m$learner.model$weights)




