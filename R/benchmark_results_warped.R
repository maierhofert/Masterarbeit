# this file looks into the results of the benchmark analysis
library("mlr")
library("ggplot2")
mytheme = theme_bw(20)

# read in most current benchmark
bmr = readRDS("Benchmark_results/2017-07-17simu_warped_bmr.RDS")
name = "bmr_warped_trigonometric"
bmr

# pretty labels for learners
lrns.colors = c("grey20", "grey60",
                "darkorange3",
                "orange1", "goldenrod")
lrns.ids = c("knn1nderiv0_eucl", "fdaclassif.classiKernel.tuned", 
             "knn1nderiv0_dtw", 
             "knn1nderiv0_phase", "knn1nderiv0_amplitude")
order.lrns = 1:5

# pretty labels for simulated data
simulation.data.limits = c("random_trigonometric_ncl10_nobs100_vamp0.5_max.phase.dif1", 
                           "random_trigonometric_ncl10_nobs100_vamp0.5_max.phase.dif1.5",
                           "random_trigonometric_ncl10_nobs100_vamp2_max.phase.dif1",    
                           "random_trigonometric_ncl10_nobs100_vamp2_max.phase.dif1.5",  
                           "random_trigonometric_ncl2_nobs100_vamp0.5_max.phase.dif1",   
                           "random_trigonometric_ncl2_nobs100_vamp0.5_max.phase.dif1.5", 
                           "random_trigonometric_ncl2_nobs100_vamp2_max.phase.dif1",     
                           "random_trigonometric_ncl2_nobs100_vamp2_max.phase.dif1.5")

simulation.data.labels = c("ncl 10; vamp 0.5; max.ph.diff 1   ", 
                           "ncl 10; vamp 0.5; max.ph.diff 1.5",
                           "ncl 10; vamp    2; max.ph.diff 1   ", 
                           "ncl 10; vamp    2; max.ph.diff 1.5",
                           #
                           "ncl   2; vamp 0.5; max.ph.diff 1   ", 
                           "ncl   2; vamp 0.5; max.ph.diff 1.5",
                           "ncl   2; vamp    2; max.ph.diff 1   ", 
                           "ncl   2; vamp    2; max.ph.diff 1.5")

# # data frame containing results
# getBMRAggrPerformances(bmr, as.df = TRUE)
p.dots = plotBMRSummary(bmr, trafo = "rank", pretty.names = TRUE, 
                        jitter = 0.05, pointsize = 10L) +
  guides(col = guide_legend(ncol = 2, override.aes = aes(size = 4))) +
  scale_y_discrete(limits = simulation.data.limits,
                   labels = simulation.data.labels) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5) +
  scale_color_manual(values = lrns.colors,
                     name = "")  +
  xlab("Rank of Brier score") +
  mytheme +
  theme(legend.position = "bottom",
        plot.margin = unit(c(1, 5, 0.5, 0.5), "lines"))
p.dots

ggsave(paste0("Grafiken/benchmark/", name, "_dots.pdf"), p.dots, 
       width = 13, height = 7)

p.bars = plotBMRRanksAsBarChart(bmr, pretty.names = TRUE) + 
  scale_fill_manual(values = lrns.colors, 
                    limits = getBMRLearnerShortNames(bmr)[order.lrns], 
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
                        facet.wrap.ncol = 2L) +
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
p.box
ggsave(paste0("Grafiken/benchmark/", name, "_boxplot.pdf"), p.box, 
       width = 13, height = 30)

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
       width = 0.8*13, height = 0.8*9)
