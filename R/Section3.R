# this is the R Code that will be used in Section 3 of the paper




################################################################################
### Section 3, classiFunc package
# install the package once
install.packages("classiFunc")

# load the package in every new R-session
library("classiFunc")

# load the example data
data("DTI", package = "classiFunc")
# check out help file for DTI data
?DTI

# randomly assign participant IDs into test and training data
set.seed(1234)
IDs = unique(DTI$ID)
# vector encoding if observation is part of test or training data
train_rows = DTI$ID %in% sample(IDs, size = 0.95 * length(IDs))

# create nearest neighbor estimator with default values
nn_mod = classiKnn(classes = DTI$case[train_rows], 
                   fdata = DTI$rcst[train_rows,])

# create Epanechnikov kernel estimator for first order derivatives
# with manually set bandwidth
ker_mod = classiKernel(classes = DTI$case[train_rows], 
                       fdata = DTI$rcst[train_rows,],
                       ker = "Ker.epa",
                       h = 0.3, 
                       nderiv = 1)
# Chunk 3
# predict the nearest neighbor estimators
# hyperparameters (k, h, ker, nderiv, ...) are stored in the model,
# they do not have to be specified again
pred_nn = predict(nn_mod, newdata = DTI$rcst[!train_rows,])
pred_ker = predict(ker_mod, newdata = DTI$rcst[!train_rows,])

# Chunk 4
# confusion matrix for nn estimator
table(pred = pred_nn, true = DTI$case[!train_rows])
# confusion matrix for kernel estimator
table(pred = pred_ker, true = DTI$case[!train_rows])

################################################################################

### Section 4, usage through mlr-interface
# Chunk 1

# install the mlr package once
install.packages("mlr")
# use the version on github if my pull request is not yet merged
devtools::install_github("maierhofert/mlr", ref = "classiFunc")

# load the mlr package in every new R-session
library("mlr")

# create learners (= model description in mlr)
lrn_nn = makeLearner(cl = "classif.classiFunc.knn")
lrn_ker = makeLearner(cl = "classif.classiFunc.kernel",
                      ker = "Ker.epa", h = 0.3, nderiv = 1)


# Chunk 2

# load the example data
data("DTI", package = "classiFunc")

# export DTI data to mlr data format
fdata = makeFunctionalData(DTI, fd.features = list(rcst = "rcst", 
                                                   cca = "cca"))
# create mlr task from data
task = makeClassifTask(data = fdata, target = "case")

# use same train/test split as in  Section 3
set.seed(1234)
IDs = unique(DTI$ID)
# vector encoding if observation is part of test or training data
train_rows = DTI$ID %in% sample(IDs, size = 0.95 * length(IDs))

# create separate tasks for test/train split
task_train = subsetTask(task, features = "rcst", subset = train_rows)
task_test = subsetTask(task, features = "rcst", subset = !train_rows)




# chunk 3
# create models (train learners on training data)
mod_nn = train(learner = lrn_nn, task = task_train)
mod_ker = train(learner = lrn_ker, task = task_train)

# chunk 4
# use trained models to predict test data
pred_nn = predict(mod_nn, task = task_test)
pred_ker = predict(mod_ker, task = task_test)

# chunk 5
# confusion matrix for nn estimator
table(pred = getPredictionResponse(pred_nn), 
      true = DTI$case[!train_rows])
# confusion matrix for kernel estimator
table(pred = getPredictionResponse(pred_ker), 
      true = DTI$case[!train_rows])







