# generate plots
mytheme = theme_bw(20)

# read in learners
source("R/create_base_learners.R")
# read in example task
tsk = readRDS("Daten/Simulated Data/random_splines/random_splines_task_ncl2_nobs10_vwc2.RDS")
nn_ensemble = train(learner = nderivKnn_eucl_ensemble, task = tsk)

# generate data for the plot
weights = mod$learner.model$weights
base.learners = BBmisc::extractSubList(mod$learner.model$base.models, "learner",
                                       simplify = FALSE)
base.learners.id = sapply(base.learners, getLearnerId)
plot.data = data.frame(id = base.learners.id, weights = weights)


library(ggplot2)

# barplot with the weights
weight.plot <- ggplot(data = plot.data, aes(x = id, y = weights)) +
  geom_bar(stat = "identity") +
  xlab("learner id") +
  mytheme
weight.plot
