# this file looks into the results of the benchmark analysis 
# for the UCR TSC data
library("mlr")
library("ggplot2")
mytheme = theme_bw(20)

# read in most current benchmark
bmr1 = readRDS("Benchmark_results/2017-07-17bmr.RDS")
bmr2 = readRDS("Benchmark_results/2017-07-22bmr.RDS")
my_mergeBenchmarkResults = function (bmrs) {
  assertList(bmrs, types = "BenchmarkResult")
  unique.tt = unique(unlist(lapply(bmrs, function(x) mlr:::getBMRObjects(x, 
                                                                   fun = getTaskType))))
  if (length(unique.tt) != 1) 
    stopf("Different task types found: %s", collapse(unique.tt))
  # task.rin = peelList(lapply(bmrs, function(bmr) getBMRObjects(bmr, 
  #                                                              fun = function(x) getRRPredictions(x)$instance$desc)))
  # task.rin = groupNamedListByNames(task.rin)
  # unique.rin = vlapply(task.rin, function(x) length(unique(x)) == 
  #                        1)
  # if (any(!unique.rin)) 
  #   stopf("Different resample description found for tasks: %s", 
  #         collapse(names(unique.rin)[!unique.rin]))
  learner.ids = unique(unlist(lapply(bmrs, getBMRLearnerIds)))
  task.ids = unique(unlist(lapply(bmrs, getBMRTaskIds)))
  all.combos = expand.grid(task.id = task.ids, learner.id = learner.ids)
  all.combos = stringi::stri_paste(all.combos$task.id, all.combos$learner.id, 
                          sep = " - ")
  existing.combos = rbindlist(lapply(bmrs, function(bmr) {
    getBMRAggrPerformances(bmr, as.df = TRUE)[, c("task.id", 
                                                  "learner.id")]
  }))
  existing.combos = stringi::stri_paste(existing.combos$task.id, existing.combos$learner.id, 
                               sep = " - ")
  if (!identical(sort(existing.combos), sort(all.combos))) {
    dupls = existing.combos[duplicated(existing.combos)]
    diff = setdiff(all.combos, existing.combos)
    msg = collapse(unique(c(dupls, diff)), "\n* ")
    stopf("The following task - learner combination(s) occur either multiple times or are missing: \n* %s\n", 
          msg)
  }
  lrns.merged = mlr:::peelList(lapply(bmrs, getBMRLearners))
  lrns.merged = unique(lrns.merged)
  res.merged = mlr:::peelList(BBmisc::extractSubList(bmrs, "results", simplify = FALSE))
  res.merged = mlr:::groupNamedListByNames(res.merged)
  measures.merged = mlr:::peelList(lapply(bmrs, getBMRMeasures))
  measures.merged = unique(measures.merged)
  for (i in seq_along(res.merged)) {
    for (j in seq_along(res.merged[[i]])) {
      res.merged[[i]][[j]] = addRRMeasure(res.merged[[i]][[j]], 
                                          measures.merged)
    }
  }
  BBmisc::makeS3Obj("BenchmarkResult", results = res.merged, measures = measures.merged, 
            learners = lrns.merged)
}
bmr = my_mergeBenchmarkResults(bmrs = list(bmr1, bmr2))
name = "bmr"

# pretty labels for learners
lrns.colors = c("grey20", "grey60",
                "darkorange3",
                "orange1", "goldenrod", 
                #
                "red3", "red1", 
                "navy", "royalblue2",
                "coral", "coral3",
                "deeppink4", "deeppink1",
                #
                "chartreuse1", "chartreuse3",
                "darkorchid1", "darkolivegreen3"
)
lrns.ids = c("knn1nderiv0_eucl", "fdaclassif.classiKernel.tuned", 
             "knn1nderiv0_dtw", 
             "knn1nderiv0_amplitude", "knn1nderiv0_phase", 
             #
             "knn_eucl_ensemble", "knnOptNderiv0_eucl.tuned",
             "nderiv_eucl_ensemble", "knn1NderivOpt_eucl.tuned",
             "semimet_ensemble", "knn1Nderiv0_semimetOpt.tuned",
             "nderivKnnSemimet_ensemble", "knnOptNderivOptSemimetOpt.tuned",
             # 
             "rf_nofeat_semimet_ensemble", "rf_feat_semimet_ensemble",
             #
             "noisy_eucl_ensemble", "rf_noisy_ensemble")

order.lrns = c(1:3, 16:17, c(4, 11, 5, 10, 6, 12, 7, 13),
               9:8, 15:14)

# # data frame containing results
# getBMRAggrPerformances(bmr, as.df = TRUE)
p.dots = plotBMRSummary(bmr, trafo = "rank", pretty.names = TRUE, 
                        jitter = 0.05, pointsize = 10L) +
  guides(col = guide_legend(ncol = 2, override.aes = aes(size = 4))) +
  scale_x_continuous(breaks = 1:17, minor_breaks = 1:15) +
  scale_color_manual(values = lrns.colors, 
                     limits = getBMRLearnerShortNames(bmr)[order.lrns],
                     name = "")  +
  xlab("rank of Brier score") +
  mytheme +
  theme(legend.position = "bottom",
        plot.margin = unit(c(1, 5, 0.5, 0.5), "lines"))
p.dots

ggsave(paste0("Grafiken/benchmark/", name, "_dots.pdf"), p.dots, 
       width = 13, height = 20)

p.bars = plotBMRRanksAsBarChart(bmr, pretty.names = TRUE,
                                order.lrns = getBMRLearnerIds(bmr)[order.lrns]) + 
  scale_fill_manual(values = lrns.colors, 
                    limits = getBMRLearnerShortNames(bmr)[order.lrns], 
                    name = "model") +
  scale_x_discrete(breaks = 1:17, 
                   labels = c(1, "", 3, "", 5, "", 7, "", 9, "", 
                              11, "", 13, "", 15, "", 17)) +
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
                    limits = getBMRLearnerShortNames(bmr)[order.lrns],
                    name = "") +
  ylab("Brier score") +
  mytheme +
  guides(fill = F) +
  theme(text = element_text(size = 15),
        plot.margin = unit(c(1, 5, 0.5, 0.5), "lines"),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = -60, hjust = 0),
        legend.position = "bottom")
p.box
ggsave(paste0("Grafiken/benchmark/", name, "_boxplot.pdf"), p.box, 
       width = 13, height = 55, limitsize = FALSE)

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

# # create critical difference diagrams for useful subsets of the learners
# # critical difference diagram
# learner.ids = c("knn1nderiv0_eucl",
#                 "fdaclassif.classiKernel.tuned")

# new helper function to extract bmr objects containing only a subset of the learners
subsetBMR = function(bmr, learner.ids) {
  bmr_new = bmr
  bmr_new$results = mlr:::getBMRObjects(bmr, learner.ids = learner.ids,
                                        fun = function(x) {x})
  bmr_new$learners = bmr$learners[learner.ids]
  return(bmr_new)
}

# is it better to ensemble or to choose using CV
ens_better = c("knn_eucl_ensemble",
               "nderiv_eucl_ensemble", "semimet_ensemble",            
               "nderivKnnSemimet_ensemble", 
               "knn1NderivOpt_eucl.tuned",
               "knnOptNderiv0_eucl.tuned", "knn1Nderiv0_semimetOpt.tuned",
               "knnOptNderivOptSemimetOpt.tuned")

bmr_ens_better = subsetBMR(bmr, learner.ids = ens_better)

g_ens_better = generateCritDifferencesData(bmr_ens_better, measure = multiclass.brier,
                                           p.value = 0.05, test = "nemenyi")
p.cd_ens_better = plotCritDifferences(g_ens_better, pretty.names = TRUE) +
  scale_color_manual(values = lrns.colors[which(lrns.ids %in% ens_better)],
                     limits = lrns.ids[which(lrns.ids %in% ens_better)],
                     name = "learner")
# theme(text = element_text(size = 10),
#      plot.margin = unit(c(2, 1, 0.5, 0.5), "lines"))
p.cd_ens_better

ggsave(paste0("Grafiken/benchmark/", name, "_ensemble_better_cd.pdf"),
       plot = p.cd_ens_better, width = 0.8*13, height = 0.8*9)


# is it better to use the rf or the nn ensemble
rf_better = c("knn1nderiv0_eucl", "fdaclassif.classiKernel.tuned",
              "knn1nderiv0_dtw", 
              "nderivKnnSemimet_ensemble", "knnOptNderivOptSemimetOpt.tuned",
              "rf_feat_semimet_ensemble", "rf_nofeat_semimet_ensemble",
              "rf_noisy_ensemble", "noisy_eucl_ensemble")

bmr_rf_better = subsetBMR(bmr, rf_better)

g_rf_better = generateCritDifferencesData(bmr_rf_better, measure = multiclass.brier,
                                          p.value = 0.05, test = "nemenyi")
p.cd_rf_better = plotCritDifferences(g_rf_better, pretty.names = TRUE) +
  scale_color_manual(values = lrns.colors[which(lrns.ids %in% rf_better)],
                     limits = lrns.ids[which(lrns.ids %in% rf_better)],
                     name = "learner")
# theme(text = element_text(size = 10),
#       plot.margin = unit(c(2, 1, 0.5, 0.5), "lines"))
p.cd_rf_better

ggsave(paste0("Grafiken/benchmark/", name, "_rf_better_cd.pdf"),
       plot = p.cd_rf_better, width = 0.8*13, height = 0.8*9)

######################
ref_mod = c("knn1nderiv0_eucl", "fdaclassif.classiKernel.tuned",
            "knn1nderiv0_dtw",
            "knn1nderiv0_amplitude",         
            "knn1nderiv0_phase",
            "knn_eucl_ensemble",
            "nderiv_eucl_ensemble", "semimet_ensemble",
            "rf_nofeat_semimet_ensemble",
            "rf_feat_semimet_ensemble")
bmr_ref_mod = subsetBMR(bmr, ref_mod)

g_ref_mod = generateCritDifferencesData(bmr_ref_mod, measure = multiclass.brier,
                                        p.value = 0.05, test = "nemenyi")
p.cd_ref_mod = plotCritDifferences(g_ref_mod, pretty.names = TRUE) +
  scale_color_manual(values = lrns.colors[which(lrns.ids %in% ref_mod)],
                     limits = lrns.ids[which(lrns.ids %in% ref_mod)],
                     name = "learner")
p.cd_ref_mod

ggsave(paste0("Grafiken/benchmark/", name, "_ref_mod_cd.pdf"),
       plot = p.cd_ref_mod, width = 0.8*13, height = 0.8*9)

