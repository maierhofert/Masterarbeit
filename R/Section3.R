# this is the R Code that will be used in Section 3 of the paper

# TODO install package
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


# chunk 2

# install the package once
install.packages("classiFunc")

# load the package in every new R-session
library("classiFunc")

# load the example data
data(Growth, package = "classiFunc")

# determine number of observations N
N = nrow(Growth$height)

# randomly assign data to test and training data
set.seed(123)
train_ids = sample(1:N, size = 0.8 * N, replace = FALSE)
test_ids = (1:N)[!(1:N) %in% train_ids]

# create nearest neighbor estimator with default values
nn_mod1 = classiKnn(classes = Growth$sex[train_ids], 
                 fdata = Growth$height[train_ids,])

# create nearest neighbor estimator using the supremum distance
nn_mod2 = classiKnn(classes = Growth$sex[train_ids], 
                 fdata = Growth$height[train_ids,],
                 metric = "supremum",
                 nderiv = 0, knn = 1)



# Chunk 3
# predict the nearest neighbor estimators
pred1 = predict(nn_mod1, newdata = Growth$height[test_ids,])
pred2 = predict(nn_mod2, newdata = Growth$height[test_ids,])

# Chunk 4
table(pred = pred1, true = Growth$sex[test_ids])
table(pred = pred2, true = Growth$sex[test_ids])


