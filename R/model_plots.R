# generate plots
mytheme = theme_gray(15)

mod = train(learner = knn_eucl_ensemble, task = tsks[[3]])

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
