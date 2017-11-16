# this is the R Code that will be used in Section 3 of the paper

################################################################################
### Section 3, classiFunc package

# Chunk 1

classiKnn(classes, fdata, grid = 1:ncol(fdata), knn = 1L, 
          metric = "L2", nderiv = 0L, ...)

classiKernel(classes, fdata, grid = 1:ncol(fdata), h = 1, 
             metric = "L2", ker = "Ker.norm", 
             nderiv = 0L, ...)


# Chunk 2

# install classiFunc package once
install.packages("classiFunc")

# load package in every new R-session
library("classiFunc")

# load example data
data("DTI", package = "classiFunc")
# check out help file for DTI data
?DTI

# subsample DTI for equal case control size
DTI = DTI[!duplicated(DTI$ID),]
DTI = DTI[1:84,]

# randomly assign participant IDs into test and training data
set.seed(12345)
IDs = unique(DTI$ID)
# vector encoding if observation is part of test or training data
train.rows = DTI$ID %in% sample(IDs, size = 0.8 * length(IDs))

# create nearest neighbor estimator with default values
nn.mod = classiKnn(classes = DTI[train.rows, "case"], 
                   fdata = DTI[train.rows, "rcst"])

# create Epanechnikov kernel estimator for first order derivatives
# with manually set bandwidth
ker.mod = classiKernel(classes = DTI[train.rows, "case"], 
                       fdata = DTI[train.rows, "rcst"],
                       ker = "Ker.epa",
                       h = 0.7, 
                       nderiv = 1)


# Chunk 3

# predict nearest neighbor estimators
# hyperparameters (k, h, ker, nderiv, ...) are stored in model
# and do not have to be specified again
pred.nn = predict(nn.mod, newdata = DTI[!train.rows, "rcst"])
pred.ker = predict(ker.mod, newdata = DTI[!train.rows, "rcst"])


# Chunk 4

# confusion matrix for nn estimator
table(pred = pred.nn, true = DTI[!train.rows, "case"])
# confusion matrix for kernel estimator
table(pred = pred.ker, true = DTI[!train.rows, "case"])

################################################################################

