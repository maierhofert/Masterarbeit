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

# determine number of observations N
IDs = unique(DTI$ID)

# randomly assign data to test and training data
set.seed(1234)
train_ids = sample(IDs, size = 0.9 * length(IDs))
# vector to check if observation is part of test or training
train_rows = DTI$ID %in% train_ids

# create nearest neighbor estimator with default values
nn_mod = classiKnn(classes = DTI$case[train_rows], 
                    fdata = DTI$rcst[train_rows,])

# create Epanechnikov kernel estimator with manually selected bandwidth
ker_mod = classiKernel(classes = DTI$case[train_rows], 
                       fdata = DTI$rcst[train_rows,],
                       ker = "Ker.epa",
                       h = 0.8)
# Chunk 3
# predict the nearest neighbor estimators
pred_nn = predict(nn_mod, newdata = DTI$rcst[!train_rows,])
pred_ker = predict(ker_mod, newdata = DTI$rcst[!train_rows,])

# Chunk 4
table(pred = pred_nn, true = DTI$case[!train_rows])
table(pred = pred_ker, true = DTI$case[!train_rows])
################################################################################
