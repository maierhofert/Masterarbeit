# In diesem File werden deskriptive Grafiken zur Berkeley Growth Study erstellt
# Underived height
source("growth_study_data.R")

library("ggplot2")

# Funktion zur Erstellung der Grafiken
plot_data <- function(deriv) {
  ggplot(growth_long[growth_long$deriv == deriv,]) +
    geom_line(aes(time, height, colour = Geschlecht, group = ID)) +
    scale_color_manual(name = "Geschlecht", 
                       labels = c("männl.", "weibl."),
                       values = c("firebrick3", "steelblue3")) +
    guides(color = guide_legend(override.aes = list(size=7,linetype=1))) +
    facet_grid(~Geschlecht2) +
    theme_gray(30)  
}

# Erstellung der Grafiken
p <- plot_data(deriv = 0) +
  ylab("Körpergröße in cm") +
  xlab("Alter in Jahren")

p0 <- plot_data(deriv = 0) +
  geom_smooth(aes(time, height, 
                  color = Geschlecht),
              color = "gray10",
              size = 1.5) +
  ylab("Körpergröße in cm") +
  xlab("Alter in Jahren")

p1 <- plot_data(deriv = 1) +
  geom_smooth(aes(time, height, 
                  color = Geschlecht),
              color = "gray10",
              size = 1.5) +
  ylab("Ableitung der Körpergröße in cm / Jahr") +
  xlab("Alter in Jahren")

p2 <- plot_data(deriv = 2) +
  geom_smooth(aes(time, height, 
                  color = Geschlecht),
              color = "gray10",
              size = 1.5) +
  ylab("2. Ableitung der Körpergröße in cm / Jahr ^ 2") +
  xlab("Alter")



# Abspeichern der Grafiken
ggsave(filename = "Grafiken/Groesse.pdf", plot = p0, height = 7, width = 10)
ggsave(filename = "Grafiken/Groesse_no_facet.pdf", plot = p + facet_null(), height = 7, width = 13)
ggsave(filename = "Grafiken/Groesse_abl.pdf", plot = p1, height = 7, width = 10)
ggsave(filename = "Grafiken/Groesse_abl2.pdf", plot = p2, height = 7, width = 10)
ggsave(filename = "Grafiken/Groesse_no_legend_no_facet.pdf", 
       plot = p + guides(color = "none") + facet_null(), 
       height = 7, width = 10)


# Abspeichern der Grafiken ohne Legende
ggsave(filename = "Grafiken/Groesse_no_legend.pdf", 
       plot = p0 + guides(color = "none") + theme_gray(40), 
       height = 13, width = 10)
ggsave(filename = "Grafiken/Groesse_abl_no_legend.pdf", 
       plot = p1 + guides(color = "none") + theme_gray(40), 
       height = 13, width = 10)
ggsave(filename = "Grafiken/Groesse_abl2_no_legend.pdf", 
       plot = p2 + guides(color = "none") + theme_gray(40),
       height = 13, width = 10)
################################################################################
# Weitere Grafiken
# Erstelle die Mittelwertskurve
head(growth_long)
mean_long <- aggregate(height ~ time + sex + deriv + Geschlecht + Geschlecht2, 
                       growth_long, mean)
head(mean_long)

mean_long2 <- dcast(mean_long, time + deriv ~ sex,
                    fun.aggregate = mean, value.var = "height")

med_long <- aggregate(height ~ time + sex + deriv + Geschlecht + Geschlecht2, 
                      growth_long, median)

# #################################################################
# Euklidische Distanz
# Funktion zur Erstellung der Grafiken
eukl_plot <- function(deriv) {
  ggplot(mean_long[mean_long$deriv == deriv,]) +
    # Fläche zwischen den Kurven
    geom_ribbon(aes(x = time, ymax = female, ymin = male), 
                data = mean_long2[mean_long2$deriv == deriv,],
                alpha = 0.4) +
    # Mittelwertslinien der Geschlechter
    geom_line(aes(time, height, colour = Geschlecht), size = 2) +
    scale_color_manual(name = "Geschlecht", 
                       labels = c("männl.", "weibl."),
                       values = c("firebrick3", "steelblue3")) +
    guides(color = guide_legend(override.aes = list(size=7,linetype=1)))  +
    theme_gray(30)
}

# Erstellen der Grafiken
eukl0 <- eukl_plot(deriv = 0) +
  ylab("Körpergröße in cm") +
  xlab("Alter in Jahren")

eukl1 <- eukl_plot(deriv = 1) +
  ylab("Ableitung der Körpergröße in cm / Jahr") +
  xlab("Alter in Jahren")

eukl2 <-  eukl_plot(deriv = 2) +
  ylab("2. Ableitung der Körpergröße in cm / Jahr ^ 2") +
  xlab("Alter in Jahren")

# Abspeichern der Grafiken
ggsave(filename = "Grafiken/Eukl_dist.pdf", plot = eukl0, height = 7, width = 10)
ggsave(filename = "Grafiken/Eukl_dist_abl.pdf", plot = eukl1, height = 7, width = 10)
ggsave(filename = "Grafiken/Eukl_dist_abl2.pdf", plot = eukl2, height = 7, width = 10)


# Abspeichern der Grafiken ohne Legende
ggsave(filename = "Grafiken/Eukl_dist_no_legend.pdf", 
       plot = eukl0 + guides(color = "none") + theme_gray(40), 
       height = 13, width = 10)
ggsave(filename = "Grafiken/Eukl_dist_abl_no_legend.pdf", 
       plot = eukl1 + guides(color = "none") + theme_gray(40), 
       height = 13, width = 10)
ggsave(filename = "Grafiken/Eukl_dist_abl2_no_legend.pdf", 
       plot = eukl2 + guides(color = "none") + theme_gray(40),
       height = 13, width = 10)

# ###############################################################################
# Maximums - Semimetrik
max_long <- mean_long
max_long <- ddply(max_long, .(sex, deriv), mutate, 
                  maximum = max(height),
                  minimum = min(height),
                  xmax = time[which.max(height)],
                  xmin = time[which.min(height)])

# Funktion zur Erstellung der Grafiken
max_plot <- function(deriv) {
  ggplot(max_long[max_long$deriv == deriv,]) +
    # Mittelwertslinien der Geschlechter
    geom_line(aes(time, height, colour = Geschlecht), size = 2) +
    scale_color_manual(name = "Geschlecht", 
                       labels = c("männl.", "weibl."),
                       values = c("firebrick3", "steelblue3")) +
    guides(color = guide_legend(override.aes = list(size=7,linetype=1)))  +
    geom_hline(aes(yintercept = maximum)) +
    geom_hline(aes(yintercept = minimum), linetype = "dashed") +
    # Einfügen der vertikalen Pfeile
    geom_segment(aes(x = 5,
                     xend = 5,
                     y = max_long[max_long$sex == "female" & max_long$deriv == deriv, "maximum"][1],
                     yend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "maximum"][1]),
                 size = 1.5,
                 arrow = arrow(ends = "both")) +
    geom_segment(aes(x = 5,
                     xend = 5,
                     y = max_long[max_long$sex == "female" & max_long$deriv == deriv, "minimum"][1],
                     yend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "minimum"][1]),
                 size = 2,
                 arrow = arrow(ends = "both")) +
    #     # Schräge Pfeile
    #     geom_segment(aes(x = max_long[max_long$sex == "female" & max_long$deriv == deriv, "xmax"][1],
    #                      xend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "xmax"][1],
    #                      y = max_long[max_long$sex == "female" & max_long$deriv == deriv, "maximum"][1],
    #                      yend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "maximum"][1]),
    #                  size = 2,
    #                  arrow = arrow(ends = "both")) +
    #     geom_segment(aes(x = max_long[max_long$sex == "female" & max_long$deriv == deriv, "xmin"][1],
    #                      xend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "xmin"][1],
    #                      y = max_long[max_long$sex == "female" & max_long$deriv == deriv, "minimum"][1],
    #                      yend = max_long[max_long$sex == "male" & max_long$deriv == deriv, "minimum"][1]),
  #                  size = 2,
  #                  arrow = arrow(ends = "both")) +
  theme_gray(30)
}

# Erstellen der Grafiken
max0 <- max_plot(deriv = 0) + 
  ylab("Körpergröße in cm") +
  xlab("Alter in Jahren")

max1 <- max_plot(deriv = 1) + 
  ylab("Ableitung der Körpergröße in cm / Jahr") +
  xlab("Alter in Jahren")

max2 <- max_plot(deriv = 2) + 
  ylab("2. Ableitung der Körpergröße in cm / Jahr ^ 2") +
  xlab("Alter in Jahren")


# Abspeichern der Grafiken
ggsave(filename = "Grafiken/Max_dist.pdf", plot = max0, height = 7, width = 10)
ggsave(filename = "Grafiken/Max_dist_abl.pdf", plot = max1, height = 7, width = 10)
ggsave(filename = "Grafiken/Max_dist_abl2.pdf", plot = max2, height = 7, width = 10)

# Abspeichern der Grafiken ohne Legende
ggsave(filename = "Grafiken/Max_dist_no_legend.pdf", 
       plot = max0 + guides(color = "none") + theme_gray(40),
       height = 13, width = 10)
ggsave(filename = "Grafiken/Max_dist_abl_no_legend.pdf", 
       plot = max1 + guides(color = "none") + theme_gray(40),
       height = 13, width = 10)
ggsave(filename = "Grafiken/Max_dist_abl2_no_legend.pdf", 
       plot = max2 + guides(color = "none") + theme_gray(40), 
       height = 13, width = 10)