# this file looks into the results of the benchmark analysis
library("mlr")
library("ggplot2")
mytheme = theme_bw(15)

# read in most current benchmark
bmr = readRDS("Benchmark_results/2017-06-09simu_warped_bmr.RDS")


# pretty labels for learners
lrns.colors = c("grey20", "grey60",
                "darkorange3",
                "orange1", "goldenrod", 
                "violetred2",
                "darkslateblue", "deeppink4",
                "chartreuse1", "chartreuse3")
lrns.ids = c("knn1nderiv0_eucl", "fdaclassif.classiKernel.tuned", 
             "knn1nderiv0_dtw", 
             "knn1nderiv0_phase", "knn1nderiv0_amplitude", 
             "knn_eucl_ensemble",
             "nderiv_eucl_ensemble", "nderivKnn_eucl_ensemble",
             "rf_nofeat_eucl_ensemble", "rf_feat_eucl_ensemble")
# lrns.short.names = c("Eucl: k 1; nderiv 0", "Eucl-Kernel: h CV-opt",
#                      "dtw: k 1; nderiv 0",
#                      "phase: k 1; nderiv 0", "amplitude: k 1; nderiv 0",
#                      "Eucl-ensemble: k 1, 3, 5, 7; nderiv 0",
#                      "Eucl-ensemble: k 1; nderiv 0, 1, 2", "Eucl-ensemble: k 1, 3, 5, 7; nderiv 0, 1, 2",
#                      "Eucl-rf: k 1, 3, 5, 7; nderiv 0, 1, 2; no feat", "Eucl-rf: k 1, 3, 5, 7; nderiv 0, 1, 2; use feat")


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


# # data frame containing results
# getBMRAggrPerformances(bmr, as.df = TRUE)
p.dots = plotBMRSummary(bmr, trafo = "rank", jitter = 0, pretty.names = TRUE) +
  scale_y_discrete(limits = simulation.data.limits,
                   labels = simulation.data.labels) +
  geom_point(size = 10) +
  scale_x_continuous(breaks = 1:10, minor_breaks = 1:10) +
  scale_color_manual(values = lrns.colors, name = "learner")  +
  xlab("Rank of Brier score") +
  mytheme
p.dots

ggsave("Grafiken/benchmark_simulation_dots.pdf", p.dots, 
       width = 13, height = 7)

p.bars = plotBMRRanksAsBarChart(bmr, pretty.names = TRUE, 
                                order.lrns = lrns.ids) + 
  scale_fill_manual(values = lrns.colors, name = "learner") +
  ylab("count") +
  mytheme
p.bars
ggsave("Grafiken/benchmark_simulation_bars.pdf", p.bars, 
       width = 13, height = 7)

#################################################################
# visualize benchmark results
plotBMRBoxplots(bmr, measure = timeboth, pretty.names = FALSE,
                facet.wrap.ncol = 3)
p.box = plotBMRBoxplots(bmr, measure = multiclass.brier, pretty.names = FALSE,
                        facet.wrap.ncol = 3) + 
  geom_boxplot(aes(fill = learner.id)) +
  scale_fill_manual(
    values = lrns.colors,
    # limits = lrns.short.names,
    name = "learner") +
  ylab("Brier score") +
  mytheme +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = -45, hjust = 0))
p.box
ggsave("Grafiken/benchmark_simulation_boxplot.pdf", p.box, 
       width = 13, height = 7)

# Friedman Test
friedmanTestBMR(bmr, measure = multiclass.brier)

# Nemenyi Post-Hoc Test
friedmanPostHocTestBMR(bmr, measure = multiclass.brier)

# critical difference diagram
g = generateCritDifferencesData(bmr, measure = multiclass.brier,
                                p.value = 0.05, test = "nemenyi")
p.cd = plotCritDifferences(g, pretty.names = TRUE) +
  scale_color_manual(values = lrns.colors,
                     limits = lrns.ids,
                     name = "learner")
p.cd
ggsave("Grafiken/benchmark_simulation_cd.pdf", p.cd, 
       width = 13, height = 7)
