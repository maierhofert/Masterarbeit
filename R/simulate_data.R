# This file can be used to create simulated data

# set hyperparameters for simulation
nclasses = 3
nknots = 10
norder = 3 # for cubic spline
nbasis = nknots + norder - 1 # - 2 ? see help of create.bspline.basis
  
nobs_per_class = 10
length_per_data = 50
x_seq = seq(0, 1, length.out = length_per_data)

# dummy object to plot pretty spline basis
library(fda)
basisobj = create.bspline.basis(c(0, 1), nbasis = nbasis, norder = norder)
plot(basisobj)



class_splines = list()
class_centers = matrix(NA, ncol = length_per_data, nrow = nclasses)
simu_data = matrix(NA, ncol = length_per_data, nrow = nclasses * nobs_per_class)

# actual data simulation
set.seed(1234)
for(class in 1:nclasses) {
  # create a cubic basis object to be (ab)used
  sp = smooth.spline(x = x_seq, 
                     y = rep(0, length_per_data), 
                     nknots = nknots, keep.data = FALSE)
  # sample random spline coefficients for this class
  sp$fit$coef = rnorm(length(sp$fit$coef))
  class_splines[[class]] = sp
  class_centers[class,] = predict(sp, x = x_seq)$y
  for(i in 1:nobs_per_class) {
    this.spline = class_splines[[class]]
    # sample spline coefficients for a new observation of a class by adding a 
    # random error to the spline coefficients of the class
    this.spline$fit$coef = this.spline$fit$coef + rnorm(length(sp$fit$coef))
    simu_data[i + nobs_per_class * (class - 1),] = predict(this.spline, x = x_seq)$y
  }
}

# plot the individual observations
matplot(t(simu_data), type = "l", 
        col = rep(1:nclasses, each = nobs_per_class))
# plot the class centers
matplot(t(class_centers), type = "l", 
        col = 1:nclasses)

# add true class label
save_data = data.frame(simu_data)
save_data$target = rep(1:nclasses, each = nobs_per_class)

saveRDS(save_data, "Daten/Simulated Data/random_splines.RDS")

# convert to mlr task
library("mlr")
simu_data_task = makeFDAClassifTask(data = save_data,
                     id = "random_splines",
                     fd.features = list(ff = 1:(ncol(save_data) - 1)),
                     target = "target")
saveRDS(simu_data_task, "Daten/Simulated Data/random_splines_task.RDS")
