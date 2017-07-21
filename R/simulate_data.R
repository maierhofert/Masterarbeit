# This file creates the random splines data

# set hyperparameters for simulation
nclasses = 3
nknots = 10
norder = 3 # for cubic spline
nbasis = nknots + norder - 1 # - 2 ? see help of create.bspline.basis

nobs_per_class = 10
length_per_data = 50
x_seq = seq(0, 1, length.out = length_per_data)

# create one object outside of the loops
# dummy object to plot pretty spline basis
library(fda)
basisobj = create.bspline.basis(c(0, 1), nbasis = nbasis, norder = norder)
pdf("Grafiken/Bsplines_basis3.pdf", width = 12, height = 7)
par(mar = c(5, 7, 4, 2) + 0.1)
plot(basisobj, col = 1, lty = 1, cex.axis = 2, cex.lab = 2.5, 
     xlab = "t", ylab = expression(B[j](t)))
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()

# class_splines = list()
# class_centers = matrix(NA, ncol = length_per_data, nrow = nclasses)
# simu_data = matrix(NA, ncol = length_per_data, nrow = nclasses * nobs_per_class)
#
# # data simulation
# set.seed(1234)
# for(class in 1:nclasses) {
#   # create a cubic basis object to be (ab)used
#   sp = smooth.spline(x = x_seq, 
#                      y = rep(0, length_per_data), 
#                      nknots = nknots, keep.data = FALSE)
#   # sample random spline coefficients for this class
#   sp$fit$coef = rnorm(length(sp$fit$coef))
#   class_splines[[class]] = sp
#   class_centers[class,] = predict(sp, x = x_seq)$y
#   for(i in 1:nobs_per_class) {
#     this.spline = class_splines[[class]]
#     # sample spline coefficients for a new observation of a class by adding a 
#     # random error to the spline coefficients of the class
#     this.spline$fit$coef = this.spline$fit$coef + rnorm(length(sp$fit$coef))
#     simu_data[i + nobs_per_class * (class - 1),] = predict(this.spline, x = x_seq)$y
#   }
# }
# 
# # plot the individual observations
# matplot(t(simu_data), type = "l", 
#         lty = rep(1:nclasses, each = nobs_per_class),
#         col = rep(1:nclasses, each = nobs_per_class))
# # plot the class centers
# matplot(t(class_centers), type = "l", 
#         lty = 1:nclasses,
#         col = 1:nclasses)
# 
# # add true class label
# save_data = data.frame(simu_data)
# save_data$target = rep(1:nclasses, each = nobs_per_class)
# 
# saveRDS(save_data, "Daten/Simulated Data/random_splines.RDS")
# 
# # convert to mlr task
# library("mlr")
# simu_data_task = makeFDAClassifTask(data = save_data,
#                                     id = "random_splines",
#                                     fd.features = list(ff = 1:(ncol(save_data) - 1)),
#                                     target = "target")
# saveRDS(simu_data_task, "Daten/Simulated Data/random_splines_task.RDS")



# parameters for splines
nknots = 10
norder = 3 # for cubic spline
nbasis = nknots + norder - 1 # - 2 ? see help of create.bspline.basis

# parameters for data
length_per_data = 50
x_seq = seq(0, 1, length.out = length_per_data)

# iterate over the settings of the simulation study
# create data sets and convert to mlr tasks
library("mlr")
library("fda")

set.seed(1234)
for(nclasses in c(2, 10)) {
  for(nobs_per_class in c(20, 100)) {
    for(var_within_classes in c(0.5, 2)) {
      # dummy object to plot pretty spline basis
      basisobj = create.bspline.basis(c(0, 1), nbasis = nbasis, norder = norder)
      
      class_splines = list()
      class_centers = matrix(NA, ncol = length_per_data, nrow = nclasses)
      simu_data = matrix(NA, ncol = length_per_data, nrow = nclasses * nobs_per_class)
      
      # actual data simulation
      for(class in 1:nclasses) {
        # create a cubic basis object to be (ab)used
        sp = smooth.spline(x = x_seq, 
                           y = rep(0, length_per_data), 
                           nknots = nknots, keep.data = FALSE)
        # sample random spline coefficients for this class
        sp$fit$coef = rnorm(length(sp$fit$coef), sd = 1)
        class_splines[[class]] = sp
        class_centers[class,] = predict(sp, x = x_seq)$y
        for(i in 1:nobs_per_class) {
          this.spline = class_splines[[class]]
          # sample spline coefficients for a new observation of a class by adding a 
          # random error to the spline coefficients of the class
          this.spline$fit$coef = this.spline$fit$coef + rnorm(length(sp$fit$coef), sd = sqrt(var_within_classes))
          simu_data[i + nobs_per_class * (class - 1),] = predict(this.spline, x = x_seq)$y
        }
      }
      
      # add true class label
      save_data = data.frame(simu_data)
      save_data$target = rep(1:nclasses, each = nobs_per_class)
      
      saveRDS(save_data, paste0("Daten/Simulated Data/random_splines/random_splines", 
                                "_ncl", nclasses, "_nobs", nobs_per_class, 
                                "_vwc", var_within_classes, ".RDS"))
      
      
      simu_data_task = makeFDAClassifTask(data = save_data,
                                          id = paste0("random_splines",
                                                      "_ncl", nclasses, 
                                                      "_nobs", nobs_per_class, 
                                                      "_vwc", var_within_classes),
                                          fd.features = list(ff = 1:(ncol(save_data) - 1)),
                                          target = "target")
      saveRDS(simu_data_task, paste0("Daten/Simulated Data/random_splines/random_splines_task", 
                                     "_ncl", nclasses, "_nobs", nobs_per_class, 
                                     "_vwc", var_within_classes, ".RDS"))
      
    }
  }
}

# Look into the simulated data
path = "Daten/Simulated Data/random_splines/random_splines_ncl2_nobs20_vwc0.5.RDS"
simu_data = readRDS(path)
# plot the individual observations
fda::matplot(t(simu_data[,-51]), type = "l", 
             lty = simu_data[,51],
             col = simu_data[,51])

