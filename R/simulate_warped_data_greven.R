# This file can be used to create simulated data

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

# create a warping function as described by Markus Strohmeier 
# see email of Prof Greven on 03.07.17
warping_fun = function(x, shape = 1, rate = 1) {
  r_vec = rgamma(length(x), shape, rate)
  cumsum(r_vec) / sum(r_vec)
}

set.seed(1234)
for(nclasses in c(2, 10)) {
  for(nobs_per_class in c(100)) {
    for(var_between_classes in c(0, 1)) {
      for(phase_factor in c(0, 1)) {
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
          sp$fit$coef = rnorm(length(sp$fit$coef), mean = 1:length(sp$fit$coef), sd = var_between_classes)
          class_splines[[class]] = sp
          class_centers[class,] = predict(sp, x = x_seq)$y
          
          for(i in 1:nobs_per_class) {
            this.spline = class_splines[[class]]
            # sample spline coefficients for a new observation of a class by adding a 
            # random error to the spline coefficients of the class
            this.spline$fit$coef = this.spline$fit$coef + rnorm(length(sp$fit$coef), sd = sqrt(var_within_classes))
            simu_data[i + nobs_per_class * (class - 1),] = 
              predict(this.spline, 
                      x = warping_fun(x_seq, 
                                      rate = 0.1 + phase_factor * seq(1/class, class, 
                                                                      length.out = length(x_seq))
                                      ))$y
          }
        }
        
        
        
        # add true class label
        save_data = data.frame(simu_data)
        save_data$target = rep(1:nclasses, each = nobs_per_class)
        
        saveRDS(save_data, paste0("Daten/Simulated Data/warped_greven/warped_greven", 
                                  "_ncl", nclasses, "_nobs", nobs_per_class, 
                                  "_vbc", var_between_classes,
                                  "_phf", phase_factor, ".RDS"))
        
        
        simu_data_task = makeFDAClassifTask(data = save_data,
                                            id = paste0("random_splines",
                                                        "_ncl", nclasses, 
                                                        "_nobs", nobs_per_class, 
                                                        "_vbc", var_between_classes,
                                                        "_phf", phase_factor),
                                            fd.features = list(ff = 1:(ncol(save_data) - 1)),
                                            target = "target")
        saveRDS(simu_data_task, paste0("Daten/Simulated Data/warped_greven/warped_greven_task", 
                                       "_ncl", nclasses, "_nobs", nobs_per_class, 
                                       "_vbc", var_between_classes,
                                       "_phf", phase_factor, ".RDS"))
      }
    }
  }
}

# Look into the simulated data
path = "Daten/Simulated Data/warped_greven/warped_greven_ncl2_nobs100_vbc1_phf0.RDS"
simu_data = readRDS(path)
# plot the individual observations
fda::matplot(t(simu_data[,-51]), type = "l", 
             lty = simu_data[,51],
             col = simu_data[,51])

