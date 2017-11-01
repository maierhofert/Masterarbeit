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

# install the package once
install.packages("classiFunc")

# load the package in every new R-session
library("classiFunc")

# load the example data
data("DTI", package = "classiFunc")
# check out help file for DTI data
?DTI
# DTI = DTI[!duplicated(DTI$ID),]

# randomly assign participant IDs into test and training data
set.seed(1234)
IDs = unique(DTI$ID)
# vector encoding if observation is part of test or training data
train.rows = DTI$ID %in% sample(IDs, size = 0.95 * length(IDs))

# create nearest neighbor estimator with default values
nn.mod = classiKnn(classes = DTI$case[train.rows], 
                   fdata = DTI$rcst[train.rows,])

# create Epanechnikov kernel estimator for first order derivatives
# with manually set bandwidth
ker.mod = classiKernel(classes = DTI$case[train.rows], 
                       fdata = DTI$rcst[train.rows,],
                       ker = "Ker.epa",
                       h = 0.3, 
                       nderiv = 1)


# Chunk 3

# predict the nearest neighbor estimators
# hyperparameters (k, h, ker, nderiv, ...) are stored in the model,
# they do not have to be specified again
pred.nn = predict(nn.mod, newdata = DTI$rcst[!train.rows,])
pred.ker = predict(ker.mod, newdata = DTI$rcst[!train.rows,])


# Chunk 4

# confusion matrix for nn estimator
table(pred = pred.nn, true = DTI$case[!train.rows])
# confusion matrix for kernel estimator
table(pred = pred.ker, true = DTI$case[!train.rows])

################################################################################