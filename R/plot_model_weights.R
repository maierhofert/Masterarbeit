library("ggplot2")
library("mlr")
library("foreign")
# generate plots
mytheme = theme_bw(20)

# read in learners
source("R/create_base_learners.R")

# read in example task
# dat = read.arff("Daten/TSC Problems/
#                 /ECG200.arff")
# tsk = makeFDAClassifTask(data = dat[,1:ncol(dat)],
#                          id = "ECG200",
#                          fd.features = list(ff = 1:(ncol(dat) - 1)),
#                          target = "target")

dat = read.arff("Daten/TSC Problems/ArrowHead/ArrowHead.arff")
# only use tow classes
dat = dat[dat$target %in% c(0, 1),]
dat$target = droplevels(dat$target)
tsk = makeFDAClassifTask(data = dat,
                         id = "ArrowHead",
                         fd.features = list(ff = 1:(ncol(dat) - 1)),
                         target = "target")

nn_ensemble = train(learner = nderivKnn_eucl_ensemble, task = tsk)
rf_ensemble = train(learner = rf_nofeat_eucl_ensemble, task = tsk)

# #############
# generate data for the nn_ensemble plot
weight = nn_ensemble$learner.model$weights
base.learners = BBmisc::extractSubList(nn_ensemble$learner.model$base.models, "learner",
                                       simplify = FALSE)
base.learners.id = sapply(base.learners, getLearnerId)
plot.data = data.frame(id = base.learners.id, weight = weight)


# barplot with the nnensemble weights
weight.plot <- ggplot(data = plot.data, aes(x = id, y = weight)) +
  geom_bar(stat = "identity") +
  xlab("base model") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, 
                                   # size = 8,
                                   vjust = 0.5))
weight.plot
ggsave(paste0("Grafiken/weightplot_nn_ensemble.pdf"), weight.plot, 
       width = 13, height = 7)


# generate data for the rf_ensemble plot
randomForest_mod = rf_ensemble$learner.model$super.model$learner.model
feat_imp = importance(randomForest_mod)
plot.data = data.frame(id = rownames(feat_imp), var_imp = as.vector(feat_imp))
varImpPlot(randomForest_mod)
# barplot with the nnensemble weights
weight.plot <- ggplot(data = plot.data, aes(x = id, y = var_imp)) +
  geom_bar(stat = "identity") +
  xlab("base model") +
  ylab("variable importance") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, 
                                   # size = 8,
                                   vjust = 0.5))
weight.plot
ggsave(paste0("Grafiken/weightplot_rf_ensemble.pdf"), weight.plot, 
       width = 13, height = 7)
