# this file looks into the results of the benchmark analysis
library("mlr")
library("ggplot2")
mytheme = theme_bw(15)

# pretty labels for learners
lrns.colors = c("thistle4", "palegoldenrod",
                "grey80", "steelblue3",
                "firebrick1", "violetred3",
                "chartreuse1", "chartreuse3")
lrns.ids = c("knn1nderiv0_eucl", "knn1nderiv0_dtw",
                "fdaclassif.classiKernel.tuned", "knn_eucl_ensemble",
                "nderiv_eucl_ensemble", "nderivKnn_eucl_ensemble",
                "rf_nofeat_eucl_ensemble", "rf_feat_eucl_ensemble")
lrns.labels = c("Eucl: k 1; nderiv 0", "dtw: k 1; nderiv 0",
           "Eucl-Kernel: h CV-opt", "Eucl-ensemble: k 1, 3, 5, 7; nderiv 0",
           "Eucl-ensemble: k 1; nderiv 0, 1, 2", "Eucl-ensemble: k 1, 3, 5, 7; nderiv 0, 1, 2",
           "Eucl-rf: k 1, 3, 5, 7; nderiv 0, 1, 2; no feat", "Eucl-rf: k 1, 3, 5, 7; nderiv 0, 1, 2; use feat")
lrns_scale_fill = scale_fill_manual(
  values = lrns.colors,
  name = "learner",
  limits = lrns.ids,
  labels = lrns.labels)
lrns_scale_color = scale_color_manual(
  values = lrns.colors,
  name = "learner",
  limits = lrns.ids,
  labels = lrns.labels)

# pretty labels for sumulated data
simulation.data.limits = c("random_splines_ncl10_nobs10_vwc0.5", "random_splines_ncl10_nobs10_vwc1",
                           "random_splines_ncl10_nobs10_vwc2", "random_splines_ncl10_nobs100_vwc0.5",
                           "random_splines_ncl10_nobs100_vwc1", "random_splines_ncl10_nobs100_vwc2",
                           "random_splines_ncl2_nobs10_vwc0.5", "random_splines_ncl2_nobs10_vwc1", 
                           "random_splines_ncl2_nobs10_vwc2", "random_splines_ncl2_nobs100_vwc0.5", 
                           "random_splines_ncl2_nobs100_vwc1", "random_splines_ncl2_nobs100_vwc2")

simulation.data.labels = c("random splines: ncl 10; nobs   10; vwc 0.5", 
                           "random splines: ncl 10; nobs   10; vwc    1",
                           "random splines: ncl 10; nobs   10; vwc    2", 
                           "random splines: ncl 10; nobs 100; vwc 0.5",
                           "random splines: ncl 10; nobs 100; vwc    1", 
                           "random splines: ncl 10; nobs 100; vwc    2",
                           "random splines: ncl   2; nobs   10; vwc 0.5", 
                           "random splines: ncl   2; nobs   10; vwc    1", 
                           "random splines: ncl   2; nobs   10; vwc    2", 
                           "random splines: ncl   2; nobs 100; vwc 0.5", 
                           "random splines: ncl   2; nobs 100; vwc    1", 
                           "random splines: ncl   2; nobs 100; vwc    2")


# read in most current benchmark
bmr = readRDS("Benchmark_results/bmr.RDS")

# # data frame containing results
# getBMRAggrPerformances(bmr, as.df = TRUE)

p.dots = plotBMRSummary(bmr, trafo = "rank", jitter = 0, pretty.names = FALSE) +
  scale_y_discrete(limits = simulation.data.limits,
                   labels = simulation.data.labels) +
  geom_point(size = 5) +
  lrns_scale_color +
  mytheme
p.dots
ggsave("Grafiken/benchmark_simulation_dots.pdf", p.dots, 
       width = 13, height = 7)

p.bars = plotBMRRanksAsBarChart(bmr, pretty.names = FALSE) + 
  lrns_scale_fill +
  ylab("count") +
  mytheme
p.bars
ggsave("Grafiken/benchmark_simulation_bars.pdf", p.bars, 
       width = 13, height = 7)

#################################################################
# visualize benchmark results
plotBMRBoxplots(bmr, measure = timeboth, pretty.names = FALSE)
plotBMRBoxplots(bmr, measure = multiclass.brier, pretty.names = FALSE)

# bp = plotBMRBoxplots(bmr, measure = timeboth, pretty.names = FALSE)
# str(bp)

# Friedman Test
friedmanTestBMR(bmr, measure = multiclass.brier)

# Nemenyi Post-Hoc Test
friedmanPostHocTestBMR(bmr, measure = multiclass.brier)

# critical difference diagram
g = generateCritDifferencesData(bmr, measure = multiclass.brier,
                                p.value = 0.05, test = "nemenyi")
p.cd = plotCritDifferences(g, pretty.names = FALSE)
p.cd
