library("foreign")
library("mlr")
library("reshape2")
library("ggplot2")
mytheme = theme_bw(20)

dat = read.arff("Daten/TSC Problems/BeetleFly/BeetleFly.arff")
# dat$target = droplevels(dat$target)
# tsk = makeFDAClassifTask(data = dat,
#                          id = "ArrowHead",
#                          fd.features = list(ff = 1:(ncol(dat) - 1)),
#                          target = "target")

# subset to use for plotting
subs = c(1:6)
subs = 1:40
subs = seq(1, 40, by = 8)

# example plot
matplot(t(dat[subs,1:512]), type = "l", 
        lty = as.numeric(dat[subs,513]),
        col = factor(dat[subs,513]))

# create derived data frames
dat_der1 = data.frame(t(apply(dat[,1:(ncol(dat) - 1)], 1, diff)))
dat_der2 = data.frame(t(apply(dat_der1[,1:(ncol(dat) - 2)], 1, diff)))
dat_der1$target = dat_der2$target = dat$target

# introduce ID variable
dat$ID = dat_der1$ID = dat_der2$ID = 1:nrow(dat)

# melt data
melt_dat = melt(dat, id.vars = c("ID", "target"))
melt_dat_der1 = melt(dat_der1, id.vars = c("ID", "target"))
melt_dat_der2 = melt(dat_der2, id.vars = c("ID", "target"))

# add time variable
melt_dat$x = as.numeric(gsub("att", "", melt_dat$variable))
melt_dat_der1$x = as.numeric(gsub("att", "", melt_dat_der1$variable))
melt_dat_der2$x = as.numeric(gsub("att", "", melt_dat_der2$variable))

dat = melt_dat

plot_data = function(dat) {
  ggplot(dat[dat$ID %in% subs,], aes(x, value, colour = target, group = ID)) +
  geom_line(size = 1) +
  # geom_smooth(se = F, method = "loess", span = 0.2) +
  scale_color_manual(name = "Outline", 
                     labels = c("Beetle", "Fly"),
                     values = c("firebrick3", "steelblue3")) +
  guides(color = guide_legend(override.aes = list(size=7,linetype=1))) +
  mytheme
}
p = plot_data(melt_dat) +
  ylab("distance from center") +
  xlab("contour position")
p
p1 = plot_data(melt_dat_der1) +
  ylab("first derivative\nof distance from center") +
  xlab("contour position")
p1

p2 = plot_data(melt_dat_der2) +
  ylab("first derivative\nof distance from center") +
  xlab("contour position")
p2

# Abspeichern der Grafiken
ggsave(filename = "Grafiken/beetlefly.pdf", plot = p, height = 7, width = 12)
# ggsave(filename = "Grafiken/beetlefly_der1.pdf", plot = p1, height = 7, width = 13)
# ggsave(filename = "Grafiken/beetlefly_der2.pdf", plot = p2, height = 7, width = 13)
