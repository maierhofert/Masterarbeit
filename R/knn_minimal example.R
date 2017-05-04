# minimal working example for knn classification algorithm

# two matrices with the same number of colums
# "training data"
mat1 = matrix(1:25, ncol = 5, byrow = TRUE)
# vector with the true classes of the training observations
classes = c(0,0,1,1,1)
# classes = 1:5

# "test data"
mat2 = matrix(15:1, nrow = 3, ncol = 5, byrow = TRUE)

# distance matrix for the rows of the observations
# this is super efficient already
dist.mat = proxy::dist(mat1, mat2)

# for 3 nearest neighbors
knn = 3L

# start the actual k nearest eighbor algorithm
# can you make this more efficient?

# matrix containing which nearest neighbor the training observation is
# for the new observation
nn.mat = apply(dist.mat, 2, order)


result.response = apply(nn.mat, 2, function(x) {
  names(which.max(table(classes[x][1:knn])))
})
result.response

# korrektere Alternative
nn.mat = apply(dist.mat, 2, rank)
result.response = apply(nn.mat, 2, function(x) {
  names(which.max(table(classes[x < knn + 1])))
})
result.response




# probabilities for the classes
result.prob = t(apply(nn.mat, 2, function(x) {
  table(classes[x][1:knn]) / knn
}))
result.prob

