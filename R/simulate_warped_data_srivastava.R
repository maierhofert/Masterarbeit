# this file creates simulated warped functional data sets using trigonometric functions
# initiate data support
length_per_data = 50
x_seq = seq(-3, 3, length.out = length_per_data)

warp_fun = function(x, a) {
  if(a == 0) {
    return(x)
  } else {
    return(6 * (exp(a * (x + 3) / 6) - 1) / (exp(a) - 1) - 3)
  }
}
simu_fun = function(x, z) {
  return(z[1] * exp(-(x - 1.5) ^ 2 / 2) + 
           z[2] * exp(-(x + 1.5) ^ 2 / 2))
}


# data simulation
set.seed(123)
for(nclasses in c(2, 10)) {
  for(nobs_per_class in c(10, 100)) {
    # initiate data frames
    class_centers = matrix(NA, ncol = length_per_data, nrow = nclasses)
    simu_data = matrix(NA, ncol = length_per_data, nrow = nclasses * nobs_per_class)
    
    for(cl.z.diff in c(0, 0.1)) {
      for(cl.a.diff in c(0, 1)) {
        for(obs.z.var in c(0.01)) {
          for(obs.a.var in c(0.01)) {
            for(class in 1:nclasses) {
              z = 1 + rep(class * cl.z.diff, 2)
              a = cl.a.diff * seq(-nclasses / 2 + 0.5, nclasses / 2 - 0.5, length.out = nclasses) [class]
              
              class_centers[class,] = simu_fun(warp_fun(x_seq, a), z)
              
              for(i in 1:nobs_per_class) {
                this.z = z + rnorm(2, mean = 0, sd = sqrt(obs.z.var))
                this.a = a + rnorm(1, mean = 0, sd = sqrt(obs.a.var))
                simu_data[i + nobs_per_class * (class - 1),] = simu_fun(warp_fun(x_seq, this.a), this.z)
              }
            }
          }
        }
        
        # add true class label
        save_data = data.frame(simu_data)
        save_data$target = rep(1:nclasses, each = nobs_per_class)
        
        saveRDS(save_data, paste0("Daten/Simulated Data/random_srivastava/random_srivastava",
                                  "_ncl", nclasses, "_nobs", nobs_per_class, 
                                  "_cl.z.diff", cl.z.diff, "_cl.a.diff", cl.a.diff,
                                  # "_obs.z.var", obs.z.var, "_obs.a.var", obs.a.var,
                                  ".RDS"))
        
        
        simu_data_task = makeFDAClassifTask(data = save_data,
                                            id = paste0("random_srivastava",
                                                        "_ncl", nclasses, "_nobs", nobs_per_class, 
                                                        "_cl.z.diff", cl.z.diff, "_cl.a.diff", cl.a.diff
                                                        # "_obs.z.var", obs.z.var, "_obs.a.var", obs.a.var
                                                        ),
                                            fd.features = list(ff = 1:(ncol(save_data) - 1)),
                                            target = "target")
        saveRDS(simu_data_task, paste0("Daten/Simulated Data/random_srivastava/random_srivastava_task",
                                       "_ncl", nclasses, "_nobs", nobs_per_class, 
                                       "_cl.z.diff", cl.z.diff, "_cl.a.diff", cl.a.diff,
                                       # "_obs.z.var", obs.z.var, "_obs.a.var", obs.a.var,
                                       ".RDS"))
      }
    }
  }
}

# Look into the simulated data
path = "Daten/Simulated Data/random_srivastava/random_srivastava_ncl2_nobs10_cl.z.diff0_cl.a.diff0.RDS"
simu_data = readRDS(path)
# plot the individual observations
fda::matplot(t(simu_data[,-51]), type = "l", 
             lty = simu_data[,51],
             col = simu_data[,51])
matplot(t(class_centers))
