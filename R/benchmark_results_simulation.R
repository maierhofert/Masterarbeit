# this file looks into the results of the benchmark analysis
library("mlr")
library("ggplot2")
mytheme = theme_bw(20)

# read in most recent benchmark
bmr = readRDS("Benchmark_results/2017-06-26simu_bmr.RDS")
name = "bmr_simu"

# pretty labels for learners
lrns.colors = c("grey20", "grey60",
                "darkorange3",
                "orange1", "goldenrod", 
                # "violetred2",
                # "darkslateblue", "deeppink4",
                "red4",
                "navy", "deeppink4",
                "chartreuse1", "chartreuse3",
                "darkolivegreen3", "darkorchid1", 
                "red1", 
                "royalblue2", "deeppink1")
lrns.ids = c("knn1nderiv0_eucl", "fdaclassif.classiKernel.tuned", 
             "knn1nderiv0_dtw", 
             "knn1nderiv0_phase", "knn1nderiv0_amplitude", 
             "knn_eucl_ensemble",
             "nderiv_eucl_ensemble", "nderivKnn_eucl_ensemble",
             "rf_nofeat_eucl_ensemble", "rf_feat_eucl_ensemble",
             "rf_noisy_ensemble", "noisy_eucl_ensemble",
             "knn1NderivOpt_eucl.tuned", "knnOptNderiv0_eucl.tuned",
             "knnOptNderivOpt_eucl.tuned")

# pretty labels for simulated data
simulation.data.limits = c("random_splines_ncl10_nobs10_vwc0.5", "random_splines_ncl10_nobs10_vwc2", 
                           "random_splines_ncl10_nobs100_vwc0.5", "random_splines_ncl10_nobs100_vwc2",
                           "random_splines_ncl2_nobs10_vwc0.5", "random_splines_ncl2_nobs10_vwc2", 
                           "random_splines_ncl2_nobs100_vwc0.5", "random_splines_ncl2_nobs100_vwc2")

simulation.data.labels = c("ncl 10; nobs   10; vwc 0.5", 
                           # "ncl 10; nobs   10; vwc    1",
                           "ncl 10; nobs   10; vwc    2", 
                           "ncl 10; nobs 100; vwc 0.5",
                           # "ncl 10; nobs 100; vwc    1", 
                           "ncl 10; nobs 100; vwc    2",
                           "ncl   2; nobs   10; vwc 0.5", 
                           # "ncl   2; nobs   10; vwc    1", 
                           "ncl   2; nobs   10; vwc    2", 
                           "ncl   2; nobs 100; vwc 0.5", 
                           # "ncl   2; nobs 100; vwc    1", 
                           "ncl   2; nobs 100; vwc    2")
order.lrns = c(1:2, 6:8, 13:15, 9:10, 12:11, 3:5)

# # data frame containing results
# getBMRAggrPerformances(bmr, as.df = TRUE)
p.dots = plotBMRSummary(bmr, trafo = "rank", pretty.names = TRUE, 
                        jitter = 0.1, pointsize = 10L) +
  guides(col = guide_legend(ncol = 2, override.aes = aes(size = 4))) +
  scale_y_discrete(limits = simulation.data.limits,
                   labels = simulation.data.labels) +
  scale_x_continuous(breaks = 1:15, minor_breaks = 1:15) +
  scale_color_manual(values = lrns.colors, 
                     breaks = getBMRLearnerShortNames(bmr)[order.lrns], 
                     name = "")  +
  xlab("Rank of Brier score") +
  mytheme +
  theme(legend.position = "bottom",
        plot.margin = unit(c(1, 5, 0.5, 0.5), "lines"))
# , text = element_text(size = 20))
p.dots

ggsave(paste0("Grafiken/benchmark/", name, "_dots.pdf"), p.dots, 
       width = 13, height = 7)

p.bars = plotBMRRanksAsBarChart(bmr, pretty.names = TRUE, 
                                order.lrns = getBMRLearnerIds(bmr)[order.lrns]) + 
  scale_fill_manual(values = lrns.colors, breaks = lrns.short.names, 
                    name = "model") +
  ylab("count") +
  mytheme
p.bars
ggsave(paste0("Grafiken/benchmark/", name, "_bars.pdf"), p.bars, 
       width = 13, height = 7)

#################################################################
# visualize benchmark results
plotBMRBoxplots(bmr, measure = timeboth, pretty.names = FALSE,
                facet.wrap.ncol = 2)
p.box = plotBMRBoxplots(bmr, measure = multiclass.brier, pretty.names = TRUE, 
                        facet.wrap.ncol = 2L,
                        order.lrns = getBMRLearnerIds(bmr)[order.lrns]) +
  geom_boxplot(aes(fill = learner.id)) +
  scale_fill_manual(values = lrns.colors,
    name = "") +
  ylab("Brier score") +
  mytheme +
  guides(fill = F) +
  theme(text = element_text(size = 15),
        plot.margin = unit(c(1, 5, 0.5, 0.5), "lines"),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = -45, hjust = 0),
        legend.position = "bottom")
labeller = function(lab) {
  pos = which(simulation.data.limits ==lab)
  simulation.data.labels[pos]
}
p.box$data$task.id = sapply(p.box$data$task.id, labeller)
p.box
ggsave(paste0("Grafiken/benchmark/", name, "_boxplot.pdf"), p.box, 
       width = 12, height = 13)

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
                     name = "learner") +
  theme(text = element_text(size = 10),
        plot.margin = unit(c(2, 1, 0.5, 0.5), "lines"))
p.cd
ggsave(paste0("Grafiken/benchmark/", name, "_cd.pdf"), p.cd, 
       width = 0.8*13, height = 0.8*7)
