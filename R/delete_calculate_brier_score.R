
library("mlr")
brier_score_2_class = 2 * 0.5 ^ 2
brier_score_10_class = 0.9 ^ 2 + 9 * 0.1 ^ 2

# prob = matrix(1/10, ncol = 10, nrow = 100)
# colnames(prob) = 1:10
# measureMulticlassBrier(probabilities = prob, 
#                  truth = rep(1:10, 10))
# 
# prob2 = matrix(1/2, ncol = 2, nrow = 100)
# colnames(prob2) = 1:2
# measureMulticlassBrier(probabilities = prob2, 
#                        truth = rep(1:2, 50))
