# create example plots for the simulated data

# plot and save with ggplot
library("ggplot2")
mytheme = theme_bw(20)
#  create a transformation function
my_transform = function(simu_data) {
  # transform into long format
  simu_data$ID = 1:nrow(simu_data)
  plot_data = reshape(simu_data, varying = list(names(simu_data)[1:50]),
                      direction = "long", idvar = "ID")
  plot_data$time = plot_data$time / 50
  plot_data
}

# create a plot function
simu_plot = function(plot_data) {
  ggplot(plot_data, aes(time, X1, group = ID, col = factor(target))) +
    geom_line() +
    scale_color_manual(name = "group", 
                       values = c("firebrick3", "steelblue3")) +
    xlab("t") +
    ylab("x(t)") +
    mytheme
}


# ###########################################################
# # random splines
# read in the simulated data
path = "Daten/Simulated Data/random_splines/random_splines_ncl2_nobs20_vwc0.5.RDS"
simu_data = readRDS(path)

plot_data = my_transform(simu_data)

p = simu_plot(plot_data) +
  geom_line(size = 1)
p

ggsave("Grafiken/simu_data_random_splines_ncl2_nobs20_vwc0.5.pdf", p,
       height = 7, width = 10)



# ###########################################################
# # random trigonometric
# read in the simulated data
path = "Daten/Simulated Data/random_trigonometric/random_trigonometric_ncl2_nobs100_vamp0.5_max.phase.dif1.RDS"
simu_data = readRDS(path)

plot_data = my_transform(simu_data)

p2 = simu_plot(plot_data)
p2

ggsave("Grafiken/simu_data_random_trigonometric_ncl2_nobs100_vamp0.5_max.phase.dif1.pdf", p2,
       height = 7, width = 10)


# ###########################################################
# # random srivastava
# read in the simulated data
path = "Daten/Simulated Data/random_srivastava/random_srivastava_ncl2_nobs100_cl.z.diff0_cl.a.diff0.RDS"
simu_data = readRDS(path)

plot_data = my_transform(simu_data)

p3.1 = simu_plot(plot_data)
p3.1

ggsave("Grafiken/simu_data_random_srivastava_ncl2_nobs100_cl.z.diff0_cl.a.diff0.pdf", p3.1,
       height = 7, width = 10)

###############
path = "Daten/Simulated Data/random_srivastava/random_srivastava_ncl2_nobs100_cl.z.diff0.1_cl.a.diff1.RDS"
simu_data = readRDS(path)

plot_data = my_transform(simu_data)

p3.4 = simu_plot(plot_data)
p3.4

ggsave("Grafiken/random_srivastava_ncl2_nobs100_cl.z.diff0.1_cl.a.diff1.pdf", p3,
       height = 7, width = 10)

# ###########################################################
# # random greven
# read in the simulated data
path = "Daten/Simulated Data/warped_greven/warped_greven_ncl2_nobs100_vbc0_phf0.RDS"
simu_data = readRDS(path)

plot_data = my_transform(simu_data)

p4.1 = simu_plot(plot_data)
p4.1

ggsave("Grafiken/simu_data_warped_greven_ncl2_nobs100_vbc0_phf0.pdf", p4.1,
       height = 7, width = 10)

path = "Daten/Simulated Data/warped_greven/warped_greven_ncl2_nobs100_vbc1_phf0.RDS"
simu_data = readRDS(path)

plot_data = my_transform(simu_data)

p4.2 = simu_plot(plot_data)
p4.2

ggsave("Grafiken/simu_data_warped_greven_ncl2_nobs100_vbc1_phf0.pdf", p4.2,
       height = 7, width = 10)

# ###########
path = "Daten/Simulated Data/warped_greven/warped_greven_ncl2_nobs100_vbc0_phf1.RDS"
simu_data = readRDS(path)

plot_data = my_transform(simu_data)

p4.3 = simu_plot(plot_data)
p4.3

ggsave("Grafiken/simu_data_warped_greven_ncl2_nobs100_vbc0_phf1.pdf", p4.3,
       height = 7, width = 10)
# ###########
path = "Daten/Simulated Data/warped_greven/warped_greven_ncl2_nobs100_vbc1_phf1.RDS"
simu_data = readRDS(path)

plot_data = my_transform(simu_data)

p4.4 = simu_plot(plot_data)
p4.4

ggsave("Grafiken/simu_data_warped_greven_ncl2_nobs100_vbc1_phf1.pdf", p4.4,
       height = 7, width = 10)

