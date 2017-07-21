# data simulation
set.seed(123)
length_per_data = 50
x_seq = seq(0, 2*pi, length.out = length_per_data)

param = 1

for(nclasses in c(2)) {
  for(nobs_per_class in c(10)) {
    # initiate data frames
    class_centers = matrix(NA, ncol = length_per_data, nrow = nclasses)
    simu_data = matrix(NA, ncol = length_per_data, nrow = nclasses * nobs_per_class)
    
    for(vamp in param) {
      for(vphase in param) {
        for(class in 1:nclasses) {
          # create a cubic basis object to be (ab)used
          # sample random spline coefficients for this class
          coef = rnorm(4, sd = param)
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
        
        fda::matplot(x_seq / pi, t(save_data[,-51]), type = "l", 
                     lty = save_data[,51],
                     col = save_data[,51])
      }
    }
  }
}
