library("ggplot2")
library("mlr")
library("foreign")
# generate plots
mytheme = theme_bw(25)

# read in learners
source("R/create_base_learners.R")

knn = c(1, 5, 9)
nderiv = c(0, 1)
semimet = c("Euclidean", "globMax", "amplitudeDistance", "phaseDistance")
for(this.knn in 1:length(knn)) {
  for(this.nderiv in 1:length(nderiv)) {
    for(this.semimet in 1:length(semimet)) {
      assign(paste0("knn", knn[this.knn], 
                    "nderiv", nderiv[this.nderiv], 
                    "_", semimet[this.semimet]),
             makeLearner(cl = "fdaclassif.classiKnn",
                                     id = paste0("knn", knn[this.knn], 
                                                 "nderiv", nderiv[this.nderiv], 
                                                 "_", semimet[this.semimet]),
                                     metric = semimet[this.semimet],
                                     predict.type = "prob",
                                     par.vals = list(knn = knn[this.knn], 
                                                     nderiv = nderiv[this.nderiv],
                                                     metric = semimet[this.semimet])))
    }
  }
}

base.learners = list(knn1nderiv0_Euclidean, knn5nderiv0_Euclidean,
                    knn1nderiv0_globMax, knn5nderiv0_globMax,
                    knn1nderiv0_amplitudeDistance, knn5nderiv0_amplitudeDistance,
                    knn1nderiv0_phaseDistance, knn5nderiv0_phaseDistance,
                    
                    knn1nderiv1_Euclidean, knn5nderiv1_Euclidean,
                    knn1nderiv1_globMax, knn5nderiv1_globMax,
                    knn1nderiv1_amplitudeDistance, knn5nderiv1_amplitudeDistance,
                    knn1nderiv1_phaseDistance, knn5nderiv1_phaseDistance)
base.learner.labels = c("Eucl: k = 1, a = 0", "Eucl: k = 5, a = 0",
                         "global max: k = 1, a = 0", "global max: k = 5, a = 0",
                         "amplitude: k = 1, a = 0", "amplitude: k = 5, a = 0",
                         "phase: k = 1, a = 0", "phase: k = 5, a = 0",
                         
                         "Eucl: k = 1, a = 1", "Eucl: k = 5, a = 1",
                         "global max: k = 1, a = 1", "global max: k = 5, a = 1",
                         "amplitude: k = 1, a = 1", "amplitude: k = 5, a = 1",
                         "phase: k = 1, a = 1", "phase: k = 5, a = 1")

# Ensemble learners
# with knne Fuchs etal 2016
nn_ensemble = makeStackedLearner(id = "knn_eucl_ensemble",
                                       base.learners = base.learners,
                                       predict.type = "prob",
                                       resampling = makeResampleDesc("CV", iters = 5L),
                                       method = "classif.bs.optimal")
nn_ensemble$short.name = "nn ensemble"

rf_ensemble = makeStackedLearner(id = "rf_nofeat_eucl_ensemble",
                                 base.learners = base.learners, 
                                 super.learner = "classif.randomForest",
                                 predict.type = "prob",
                                 use.feat = FALSE,
                                 method = "stack.cv")
rf_ensemble$short.name = "rf ensemble"

source("R/growth_study_data.R")
tsk = makeFDAClassifTask(data = growth_wide[,2:ncol(growth_wide)],
                         id = "growth_study",
                         fd.features = list(ff = 2:(ncol(growth_wide) - 1)),
                         target = "sex")

# train the models
# this has a run time of about 2 days
nn_ensemble_mod = train(learner = nn_ensemble, task = tsk)
rf_ensemble_mod = train(learner = rf_ensemble, task = tsk)

# save models
saveRDS(rf_ensemble_mod, paste0("rf_ensemble_mod.RDS"))
saveRDS(nn_ensemble_mod, paste0("nn_ensemble_mod.RDS"))
# read in the models
rf_ensemble_mod = readRDS("rf_ensemble_mod.RDS")
nn_ensemble_mod = readRDS("nn_ensemble_mod.RDS")

# #############
# generate data for the nn_ensemble plot
weight = nn_ensemble_mod$learner.model$weights
base.learners = BBmisc::extractSubList(nn_ensemble_mod$learner.model$base.models, "learner",
                                       simplify = FALSE)
base.learners.id = sapply(base.learners, getLearnerId)
plot.data = data.frame(id = base.learners.id, weight = weight)


# barplot with the nnensemble weights
weight.plot <- ggplot(data = plot.data, aes(x = id, y = weight)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = base.learners.id,
                   labels = base.learner.labels) +
  xlab("base model") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0.95,
                                   vjust = 0.5))
weight.plot
ggsave(paste0("Grafiken/weightplot_nn_ensemble.pdf"), weight.plot, 
       width = 12, height = 10)


# generate data for the rf_ensemble plot
library("randomForest")
randomForest_mod = rf_ensemble_mod$learner.model$super.model$learner.model
feat_imp = importance(randomForest_mod)
plot.data = data.frame(id = rownames(feat_imp), var_imp = as.vector(feat_imp))
varImpPlot(randomForest_mod)
# barplot with the nnensemble weights
weight.plot <- ggplot(data = plot.data, aes(x = id, y = var_imp)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = base.learners.id,
                   labels = base.learner.labels) +
  xlab("base model") +
  ylab("variable importance") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0.95,
                                   vjust = 0.5))
weight.plot
ggsave(paste0("Grafiken/weightplot_rf_ensemble.pdf"), weight.plot, 
       width = 12, height = 10)
