# this is the R Code that will be used in Section 3 of the paper

# install.packages("classiFunc")
library("classiFunc")

## Creating Models
# chunk 1

# classiKnn(classes, fdata, grid = 1:ncol(fdata), knn = 1L, 
#           metric = "Euclidean", nderiv = 0L, 
#           derived = FALSE, deriv.method = "base.diff",
#           custom.metric = function(x, y, ...) {
#             return(sqrt(sum((x - y)^2)))}, 
#           ...)
# classiKernel(classes, fdata, grid = 1:ncol(fdata), h = 1,
#              metric = "Euclidean", ker = "Ker.norm", nderiv = 0L, 
#              derived = FALSE, deriv.method = "base.diff",
#              custom.metric = function(x, y, ...) {
#                return(sqrt(sum((x - y)^2)))}, 
#              custom.ker = function(u) {
#                return(dnorm(u))}, 
#              ...) 

classiKnn(classes, fdata, grid = 1:ncol(fdata), knn = 1L, 
          metric = "L2", nderiv = 0L, ...)

classiKernel(classes, fdata, grid = 1:ncol(fdata), h = 1, 
             metric = "L2", ker = "Ker.norm", 
             nderiv = 0L, ...)


# # chunk 2
# 
# # install the package once
# install.packages("classiFunc")
# 
# # load the package in every new R-session
# library("classiFunc")
# 
# # load the example data
# data("Growth", package = "classiFunc")
# 
# # determine number of observations N
# N = nrow(Growth$height)
# 
# # randomly assign data to test and training data
# set.seed(12345)
# train_ids = sample(1:N, size = 0.8 * N, replace = FALSE)
# test_ids = (1:N)[!(1:N) %in% train_ids]
# 
# # create nearest neighbor estimator with default values
# nn_mod1 = classiKnn(classes = Growth$sex[train_ids], 
#                  fdata = Growth$height[train_ids,])
# 
# # create nearest neighbor estimator using the supremum distance
# nn_mod2 = classiKernel(classes = Growth$sex[train_ids], 
#                  fdata = Growth$height[train_ids,],
#                  # metric = "supremum",
#                  h = 0.2)
# 
# 
# # Chunk 3
# # predict the nearest neighbor estimators
# pred1 = predict(nn_mod1, newdata = Growth$height[test_ids,])
# pred2 = predict(nn_mod2, newdata = Growth$height[test_ids,])
# 
# # Chunk 4
# table(pred = pred1, true = Growth$sex[test_ids])
# table(pred = pred2, true = Growth$sex[test_ids])


################################################################################
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
train_ids = sample(IDs, size = 0.95 * length(IDs))
# vector encoding if observation is part of test or training data
train_rows = DTI$ID %in% train_ids

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

### Chapter 4, usage through mlr-interface
# Chunk 4.1
# install the mlr package once
install.packages("mlr")
devtools::install_github("maierhofert/mlr", ref = "classiFunc")

# load the package in every new R-session
library("mlr")

# load the example data
data("DTI", package = "classiFunc")

# export DTI data to mlr data format
fdata = makeFunctionalData(data = DTI, fd.features = list(rcst = "rcst", cca = "cca"))
task = makeClassifTask(data = fdata, target = "case")









