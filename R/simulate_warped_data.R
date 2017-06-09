# this file creates simulated warped functional data sets using trigonometric functions
# initiate data support
length_per_data = 50
x_seq = seq(0, 2*pi, length.out = length_per_data)

# data simulation
set.seed(123)
for(nclasses in c(2, 10)) {
  # for(nobs_per_class in c(10, 100)) { # TODO später groß laufen lassen
  for(nobs_per_class in c(10)) {
    # initiate data frames
    class_centers = matrix(NA, ncol = length_per_data, nrow = nclasses)
    simu_data = matrix(NA, ncol = length_per_data, nrow = nclasses * nobs_per_class)
    
    for(max.phase.dif in c(0.1, 0.5)) {
      for(vamp in c(0.1, 1)) {
        for(class in 1:nclasses) {
          class.phase = 2 * pi * class / nclasses
          class.amp = 1
          class_centers[class,] = class.amp * cos(class.phase + x_seq)
          for(i in 1:nobs_per_class) {
            this.phase = class.phase + runif(1, 0, max.phase.dif * pi)
            this.amp = class.amp + rnorm(1, sd = sqrt(vamp))
            simu_data[i + nobs_per_class * (class - 1),] = this.amp * cos(this.phase + x_seq)
          }
        }
        
        # add true class label
        save_data = data.frame(simu_data)
        save_data$target = rep(1:nclasses, each = nobs_per_class)
        
        saveRDS(save_data, paste0("Daten/Simulated Data/random_trigonometric/random_trigonometric", 
                                  "_ncl", nclasses, "_nobs", nobs_per_class, 
                                  "_vamp", vamp, "_max.phase.dif", max.phase.dif, ".RDS"))
        
        
        simu_data_task = makeFDAClassifTask(data = save_data,
                                            id = paste0("random_trigonometric", 
                                                        "_ncl", nclasses, "_nobs", nobs_per_class, 
                                                        "_vamp", vamp, "_max.phase.dif", max.phase.dif),
                                            fd.features = list(ff = 1:(ncol(save_data) - 1)),
                                            target = "target")
        saveRDS(simu_data_task, paste0("Daten/Simulated Data/random_trigonometric/random_trigonometric_task", 
                                       "_ncl", nclasses, "_nobs", nobs_per_class, 
                                       "_vamp", vamp, "_max.phase.dif", max.phase.dif, ".RDS"))
      }
    }
  }
}

# Look into the simulated data
path = "Daten/Simulated Data/random_trigonometric/random_trigonometric_ncl2_nobs10_vamp1_max.phase.dif0.1.RDS"
simu_data = readRDS(path)
# plot the individual observations
fda::matplot(t(simu_data[,-51]), type = "l", 
             lty = simu_data[,51],
             col = simu_data[,51])
matplot(t(class_centers))
