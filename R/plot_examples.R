
library("foreign")
library("mlr")
library("reshape2")

dat = read.arff("Daten/TSC Problems/ArrowHead/ArrowHead.arff")
# only use tow classes
dat = dat[dat$target %in% c(0, 1),]
dat$target = droplevels(dat$target)
tsk = makeFDAClassifTask(data = dat,
                         id = "ArrowHead",
                         fd.features = list(ff = 1:(ncol(dat) - 1)),
                         target = "target")

# subset to use for plotting
subs = c(1:6)

# example plot
matplot(t(dat[subs,1:251]), type = "l", 
        lty = as.numeric(dat[subs,252]),
        col = factor(dat[subs,252]))

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
  # geom_line(size = 1) +
  geom_smooth(se = F, method = "loess", span = 0.2) +
  scale_color_manual(name = "Arrow Type", 
                     labels = c("Avonlea", "Clovis"),
                     values = c("firebrick3", "steelblue3")) +
  guides(color = guide_legend(override.aes = list(size=7,linetype=1))) +
  mytheme
}
p = plot_data(melt_dat) +
  ylab("distance from center") +
  xlab("radial position")
p
p1 = plot_data(melt_dat_der1) +
  ylab("first derivative\nof distance from center") +
  xlab("radial position")
p1

p2 = plot_data(melt_dat_der2) +
  ylab("first derivative\nof distance from center") +
  xlab("radial position")
p2

# Abspeichern der Grafiken
ggsave(filename = "Grafiken/arrowhead.pdf", plot = p, height = 7, width = 13)
ggsave(filename = "Grafiken/arrowhead_der1.pdf", plot = p1, height = 7, width = 13)
ggsave(filename = "Grafiken/arrowhead_der2.pdf", plot = p2, height = 7, width = 13)



# # Abspeichern der Grafiken ohne Legende
# ggsave(filename = "Grafiken/Groesse_no_legend.pdf", 
#        plot = p0 + guides(color = "none") + theme_gray(40), 
#        height = 13, width = 10)
# ggsave(filename = "Grafiken/Groesse_abl_no_legend.pdf", 
#        plot = p1 + guides(color = "none") + theme_gray(40), 
#        height = 13, width = 10)
# ggsave(filename = "Grafiken/Groesse_abl2_no_legend.pdf", 
#        plot = p2 + guides(color = "none") + theme_gray(40),
#        height = 13, width = 10)
# 
# ################################################################################
# # Weitere Grafiken
# # Erstelle die Mittelwertskurve
# head(growth_long)
# mean_long <- aggregate(height ~ time + sex + deriv + Geschlecht + Geschlecht2, 
#                        growth_long, mean)
# head(mean_long)
# 
# mean_long2 <- dcast(mean_long, time + deriv ~ sex,
#                     fun.aggregate = mean, value.var = "height")
# 
# med_long <- aggregate(height ~ time + sex + deriv + Geschlecht + Geschlecht2, 
#                       growth_long, median)
# 
# # #################################################################
# # Euklidische Distanz
# # Funktion zur Erstellung der Grafiken
# eukl_plot <- function(deriv) {
#   ggplot(mean_long[mean_long$deriv == deriv,]) +
#     # Fl?che zwischen den Kurven
#     geom_ribbon(aes(x = time, ymax = female, ymin = male), 
#                 data = mean_long2[mean_long2$deriv == deriv,],
#                 alpha = 0.4) +
#     # Mittelwertslinien der Geschlechter
#     geom_line(aes(time, height, colour = Geschlecht), size = 2) +
#     scale_color_manual(name = "Geschlecht", 
#                        labels = c("m?nnl.", "weibl."),
#                        values = c("firebrick3", "steelblue3")) +
#     guides(color = guide_legend(override.aes = list(size=7,linetype=1)))  +
#     theme_gray(30)
# }
# 
# # Erstellen der Grafiken
# eukl0 <- eukl_plot(deriv = 0) +
#   ylab("K?rpergr??e in cm") +
#   xlab("Alter in Jahren")
# 
# eukl1 <- eukl_plot(deriv = 1) +
#   ylab("Ableitung der K?rpergr??e in cm / Jahr") +
#   xlab("Alter in Jahren")
# 
# eukl2 <-  eukl_plot(deriv = 2) +
#   ylab("2. Ableitung der K?rpergr??e in cm / Jahr ^ 2") +
#   xlab("Alter in Jahren")
# 
# # Abspeichern der Grafiken
# ggsave(filename = "Grafiken/Eukl_dist.pdf", plot = eukl0, height = 7, width = 10)
# ggsave(filename = "Grafiken/Eukl_dist_abl.pdf", plot = eukl1, height = 7, width = 10)
# ggsave(filename = "Grafiken/Eukl_dist_abl2.pdf", plot = eukl2, height = 7, width = 10)
# 
# 
# # Abspeichern der Grafiken ohne Legende
# ggsave(filename = "Grafiken/Eukl_dist_no_legend.pdf", 
#        plot = eukl0 + guides(color = "none") + theme_gray(40), 
#        height = 13, width = 10)
# ggsave(filename = "Grafiken/Eukl_dist_abl_no_legend.pdf", 
#        plot = eukl1 + guides(color = "none") + theme_gray(40), 
#        height = 13, width = 10)
# ggsave(filename = "Grafiken/Eukl_dist_abl2_no_legend.pdf", 
#        plot = eukl2 + guides(color = "none") + theme_gray(40),
#        height = 13, width = 10)
# 
# # ###############################################################################
# # Maximums - Semimetrik
# max_long <- mean_long
# max_long <- ddply(max_long, .(sex, deriv), mutate, 
#                   maximum = max(height),
#                   minimum = min(height),
#                   xmax = time[which.max(height)],
#                   xmin = time[which.min(height)])
# 
# # Funktion zur Erstellung der Grafiken
# max_plot <- function(deriv) {
#   ggplot(max_long[max_long$deriv == deriv,]) +
#     # Mittelwertslinien der Geschlechter
#     geom_line(aes(time, height, colour = Geschlecht), size = 2) +
#     scale_color_manual(name = "Geschlecht", 
#                        labels = c("m?nnl.", "weibl."),
#                        values = c("firebrick3", "steelblue3")) +
#     guides(color = guide_legend(override.aes = list(size=7,linetype=1)))  +
#     geom_hline(aes(yintercept = maximum)) +
#     geom_hline(aes(yintercept = minimum), linetype = "dashed") +
#     # Einf?gen der vertikalen Pfeile
#     geom_segment(aes(x = 5,
#                      xend = 5,
#                      y = max_long[max_long$sex == "female" & max_long$deriv == deriv, "maximum"][1],
#                      yend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "maximum"][1]),
#                  size = 1.5,
#                  arrow = arrow(ends = "both")) +
#     geom_segment(aes(x = 5,
#                      xend = 5,
#                      y = max_long[max_long$sex == "female" & max_long$deriv == deriv, "minimum"][1],
#                      yend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "minimum"][1]),
#                  size = 2,
#                  arrow = arrow(ends = "both")) +
#     #     # Schr?ge Pfeile
#     #     geom_segment(aes(x = max_long[max_long$sex == "female" & max_long$deriv == deriv, "xmax"][1],
#     #                      xend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "xmax"][1],
#     #                      y = max_long[max_long$sex == "female" & max_long$deriv == deriv, "maximum"][1],
#     #                      yend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "maximum"][1]),
#     #                  size = 2,
#     #                  arrow = arrow(ends = "both")) +
#     #     geom_segment(aes(x = max_long[max_long$sex == "female" & max_long$deriv == deriv, "xmin"][1],
#     #                      xend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "xmin"][1],
#     #                      y = max_long[max_long$sex == "female" & max_long$deriv == deriv, "minimum"][1],
#     #                      yend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "minimum"][1]),
#   #                  size = 2,
#   #                  arrow = arrow(ends = "both")) +
#   theme_gray(30)
# }
# 
# # Erstellen der Grafiken
# max0 <- max_plot(deriv = 0) + 
#   ylab("K?rpergr??e in cm") +
#   xlab("Alter in Jahren")
# 
# max1 <- max_plot(deriv = 1) + 
#   ylab("Ableitung der K?rpergr??e in cm / Jahr") +
#   xlab("Alter in Jahren")
# 
# max2 <- max_plot(deriv = 2) + 
#   ylab("2. Ableitung der K?rpergr??e in cm / Jahr ^ 2") +
#   xlab("Alter in Jahren")
# 
# 
# # Abspeichern der Grafiken
# ggsave(filename = "Grafiken/Max_dist.pdf", plot = max0, height = 7, width = 10)
# ggsave(filename = "Grafiken/Max_dist_abl.pdf", plot = max1, height = 7, width = 10)
# ggsave(filename = "Grafiken/Max_dist_abl2.pdf", plot = max2, height = 7, width = 10)
# 
# # Abspeichern der Grafiken ohne Legende
# ggsave(filename = "Grafiken/Max_dist_no_legend.pdf", 
#        plot = max0 + guides(color = "none") + theme_gray(40),
#        height = 13, width = 10)
# ggsave(filename = "Grafiken/Max_dist_abl_no_legend.pdf", 
#        plot = max1 + guides(color = "none") + theme_gray(40),
#        height = 13, width = 10)
# ggsave(filename = "Grafiken/Max_dist_abl2_no_legend.pdf", 
#        plot = max2 + guides(color = "none") + theme_gray(40), 
#        height = 13, width = 10)