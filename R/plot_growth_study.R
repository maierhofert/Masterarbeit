# this file creates plots for the Berkley growth study data
# Underived height
source("R/growth_study_data.R")
mytheme = theme_bw(20)
library("ggplot2")

col.male = "chartreuse3"
col.male = "royalblue"
col.female = "chartreuse3"

# function to create figures
plot_data <- function(deriv) {
  ggplot(growth_long[growth_long$deriv == deriv,]) +
    geom_line(aes(time, height, colour = sex, group = ID)) +
    scale_color_manual(name = "sex", 
                       # labels = c("male", "female"),
                       values = c(col.female, col.male)) +
    guides(color = guide_legend(override.aes = list(size=7,linetype=1))) +
    # facet_grid(~sex) +
    mytheme  
}

# creating figures
p <- plot_data(deriv = 0) +
  ylab("height in cm") +
  xlab("age in years")

p0 <- plot_data(deriv = 0) +
  geom_smooth(aes(time, height, group = sex),
              color = "gray10",
              size = 1.5) +
  ylab("height in cm") +
  xlab("age in years")

p1 <- plot_data(deriv = 1) +
  geom_smooth(aes(time, height, group = sex),
              color = "gray10",
              size = 1.5) +
  ylab("1st derivative of height in cm / year") +
  xlab("age in years")

p2 <- plot_data(deriv = 2) +
  geom_smooth(aes(time, height, group = sex),
              color = "gray10",
              size = 1.5) +
  ylab("2nd derivative of height in cm / year^2") +
  xlab("age in years")



# save plots
ggsave(filename = "Grafiken/Berkeley_growth_study/height.pdf", 
       plot = p0, height = 7, width = 12)
# ggsave(filename = "Grafiken/Berkeley_growth_study/height_no_facet.pdf", 
#        plot = p + facet_null(), height = 7, width = 13)
ggsave(filename = "Grafiken/Berkeley_growth_study/height_der.pdf", 
       plot = p1, height = 7, width = 12)
ggsave(filename = "Grafiken/Berkeley_growth_study/height_2der.pdf", 
       plot = p2, height = 7, width = 12)
# ggsave(filename = "Grafiken/Berkeley_growth_study/height_no_legend_no_facet.pdf", 
#        plot = p + guides(color = "none") + facet_null(), 
#        height = 7, width = 12)


################################################################################
# further figures 
# create mean curves per sex
mean_long <- aggregate(height ~ time + sex + deriv + sex, 
                       growth_long, mean)

mean_long2 <- dcast(mean_long, time + deriv ~ sex,
                    fun.aggregate = mean, value.var = "height")

med_long <- aggregate(height ~ time + sex + deriv, 
                      growth_long, median)

# #################################################################
# manhattan distance
# function to create figures
manh_plot <- function(deriv) {
  ggplot(mean_long[mean_long$deriv == deriv,]) +
    # area between curves
    geom_ribbon(aes(x = time, ymax = female, ymin = male), 
                data = mean_long2[mean_long2$deriv == deriv,],
                alpha = 0.2) +
    # mean curves per sex
    geom_line(aes(time, height, colour = sex), size = 2) +
    scale_color_manual(name = "sex",
                       values = c(col.female, col.male)) +
    guides(color = guide_legend(override.aes = list(size=7,linetype=1)))  +
    mytheme
}

# Erstellen der Grafiken
manh0 <- manh_plot(deriv = 0) +
  ylab("height in cm") +
  xlab("age in years")

manh1 <- manh_plot(deriv = 1) +
  ylab("1st derivative of height in cm / year") +
  xlab("age in years")

manh2 <-  manh_plot(deriv = 2) +
  ylab("2nd derivative of height in cm / year^2") +
  xlab("age in years")

# save the plots
# ggsave(filename = "Grafiken/Berkeley_growth_study/manh_dist.pdf", 
#        plot = manh0, height = 12, width = 12)
# ggsave(filename = "Grafiken/Berkeley_growth_study/manh_dist_der.pdf", 
#        plot = manh1, height = 12, width = 12)
# ggsave(filename = "Grafiken/Berkeley_growth_study/manh_dist_2der.pdf", 
#        plot = manh2 + theme_bw(40), height = 12, width = 12)


# save the plots without legend
ggsave(filename = "Grafiken/Berkeley_growth_study/manh_dist_no_legend.pdf", 
       plot = manh0 + guides(color = "none") + theme_bw(45), height = 12, width = 10)
ggsave(filename = "Grafiken/Berkeley_growth_study/manh_dist_der_no_legend.pdf", 
       plot = manh1 + guides(color = "none") + theme_bw(45), height = 12, width = 10)
ggsave(filename = "Grafiken/Berkeley_growth_study/manh_dist_2der_no_legend.pdf",
       plot = manh2 + guides(color = "none") + theme_bw(45), height = 12, width = 10)


# ###############################################################################
# maximums semimetric
max_long <- mean_long
max_long <- ddply(max_long, .(sex, deriv), mutate, 
                  maximum = max(height),
                  minimum = min(height),
                  xmax = time[which.max(height)],
                  xmin = time[which.min(height)],
                  mean.height = mean(height))

# function to create figures
max_plot <- function(deriv) {
  ggplot(max_long[max_long$deriv == deriv,]) +
    # Mittelwertslinien der Geschlechter
    geom_line(aes(time, height, colour = sex), size = 2) +
    scale_color_manual(name = "sex", 
                       values = c(col.female, col.male)) +
    guides(color = guide_legend(override.aes = list(size=7,linetype=1)))  +
    geom_hline(aes(yintercept = maximum), linetype = "dotted", size = 1) +
    geom_hline(aes(yintercept = minimum), linetype = "longdash", size = 1) +
    geom_hline(aes(yintercept = mean.height), linetype = "solid", size = 1) +
    # add vertical arrows
    geom_segment(aes(x = 5,
                     xend = 5,
                     y = max_long[max_long$sex == "female" & max_long$deriv == deriv, "maximum"][1],
                     yend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "maximum"][1]),
                 size = 1.2,
                 arrow = arrow(ends = "both")) +
    geom_segment(aes(x = 5,
                     xend = 5,
                     y = max_long[max_long$sex == "female" & max_long$deriv == deriv, "minimum"][1],
                     yend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "minimum"][1]),
                 size = 1.2,
                 arrow = arrow(ends = "both")) +
    geom_segment(aes(x = 5,
                     xend = 5,
                     y = max_long[max_long$sex == "female" & max_long$deriv == deriv, "mean.height"][1],
                     yend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "mean.height"][1]),
                 size = 1.2,
                 arrow = arrow(ends = "both")) +
  mytheme
}

# create plots
max0 <- max_plot(deriv = 0) + 
  ylab("height in cm") +
  xlab("age in years")

max1 <- max_plot(deriv = 1) + 
  ylab("1st derivative of height in cm / year") +
  xlab("age in years")

max2 <- max_plot(deriv = 2) + 
  ylab("2nd derivative of height in cm / year^2") +
  xlab("age in years")


# save without legend
ggsave(filename = "Grafiken/Berkeley_growth_study/max_dist_no_legend.pdf", 
       plot = max0 + guides(color = "none")+ theme_bw(45), height = 12, width = 10)
ggsave(filename = "Grafiken/Berkeley_growth_study/max_dist_der_no_legend.pdf", 
       plot = max1 + guides(color = "none")+ theme_bw(45), height = 12, width = 10)
ggsave(filename = "Grafiken/Berkeley_growth_study/max_dist_2der_no_legend.pdf", 
       plot = max2 + guides(color = "none")+ theme_bw(45), height = 12, width = 10)


# #################################################################################
# square root velocity plots
srv_long <- ddply(max_long, .(sex, deriv), mutate, 
                  srv_ = height / sqrt(abs(height)))

# function to create figures
srv_plot <- function(deriv) {
  ggplot(srv_long[max_long$deriv == (deriv + 1),]) +
    # Mittelwertslinien der Geschlechter
    geom_line(aes(time, srv_, colour = sex), size = 2) +
    scale_color_manual(name = "sex", 
                       values = c(col.female, col.male)) +
    guides(color = guide_legend(override.aes = list(size=7,linetype=1))) +
    mytheme
}

# create plots
srv0 <- srv_plot(deriv = 0) + 
  ylab("srv(height in cm)") +
  xlab("age in years")

srv1 <- srv_plot(deriv = 1) + 
  ylab("srv(1st derivative of height in cm / year)") +
  xlab("age in years")

srv2 <- srv_plot(deriv = 2) + 
  ylab("srv(2nd derivative of height in cm / year^2)") +
  xlab("age in years")

# save without legend
ggsave(filename = "Grafiken/Berkeley_growth_study/srv.pdf", 
       plot = srv0 + guides(color = "none")+ theme_bw(45), height = 12, width = 10)
ggsave(filename = "Grafiken/Berkeley_growth_study/srv_der.pdf", 
       plot = srv1 + guides(color = "none")+ theme_bw(45), height = 12, width = 10)
ggsave(filename = "Grafiken/Berkeley_growth_study/srv_2der.pdf", 
       plot = srv2 + guides(color = "none")+ theme_bw(45), height = 12, width = 10)

