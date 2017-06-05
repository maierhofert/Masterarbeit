# this file creates simulated warped functional data sets using trigonometric functions
# initiate data support
length_per_data = 50
x_seq = seq(0, 2*pi, length.out = length_per_data)

# data simulation
set.seed(123)
for(nclasses in c(2, 10)) {
  for(nobs_per_class in c(10, 100)) {
    # initiate data frames
    class_centers = matrix(NA, ncol = length_per_data, nrow = nclasses)
    simu_data = matrix(NA, ncol = length_per_data, nrow = nclasses * nobs_per_class)
    
    for(vamp in c(0.5, 1, 2)) {
      for(vphase in c(0.5, 1, 2)) {
        for(class in 1:nclasses) {
          # create a cubic basis object to be (ab)used
          # sample random spline coefficients for this class
          coef = rnorm(4, sd = 1)
          class_centers[class,] = coef[1] * cos(coef[2] + x_seq) +
            coef[3] * sin(coef[4] + x_seq) 
          for(i in 1:nobs_per_class) {
            this.coef = coef + rnorm(4, sd = c(vamp, vphase, vamp, vphase))
            simu_data[i + nobs_per_class * (class - 1),] = cos(this.coef[2] + x_seq) +
              this.coef[3] * sin(this.coef[4] + x_seq) 
          }
        }
        
        # add true class label
        save_data = data.frame(simu_data)
        save_data$target = rep(1:nclasses, each = nobs_per_class)
        
        saveRDS(save_data, paste0("Daten/Simulated Data/random_trigonometric/random_trigonometric", 
                                  "_ncl", nclasses, "_nobs", nobs_per_class, 
                                  "_vamp", vamp, "_vphase", vphase, ".RDS"))
        
        
        simu_data_task = makeFDAClassifTask(data = save_data,
                                            id = paste0("random_trigonometric", 
                                                        "_ncl", nclasses, "_nobs", nobs_per_class, 
                                                        "_vamp", vamp, "_vphase", vphase),
                                            fd.features = list(ff = 1:(ncol(save_data) - 1)),
                                            target = "target")
        saveRDS(simu_data_task, paste0("Daten/Simulated Data/random_trigonometric/random_trigonometric_task", 
                                       "_ncl", nclasses, "_nobs", nobs_per_class, 
                                       "_vamp", vamp, "_vphase", vphase, ".RDS"))
      }
    }
  }
}

# Look into the simulated data
path = "Daten/Simulated Data/random_trigonometric/random_trigonometric_ncl2_nobs10_vamp0.5_vphase0.5.RDS"
simu_data = readRDS(path)
# plot the individual observations
fda::matplot(t(simu_data[,-51]), type = "l", 
             lty = simu_data[,51],
             col = simu_data[,51])
