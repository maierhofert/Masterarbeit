# Erzeugt Grafiken für das Beispiel zum Nonparametric Functional Kernel Estimator
# auf einem Teil der Berkeley Growth Study
source("growth_study_data.R")
growth_long_short <- growth_long[growth_long$ID %in% c("boy02", "boy04", "girl04"),]

growth_long_short[growth_long_short$ID == "boy02", "Geschlecht"] <- "unbekannt"


growth_long_short <- ddply(growth_long_short, .(ID, deriv), mutate, 
                  maximum = max(height),
                  minimum = min(height),
                  xmax = time[which.max(height)],
                  xmin = time[which.min(height)])

# Funktion zur Erstellung der Plots
ferraty_plot_raw <- function(deriv, dat = growth_long_short) {
  ggplot(dat[dat$deriv == deriv,]) +
    geom_line(aes(time, height, colour = Geschlecht, group = ID), size = 1) +
    scale_color_manual(name = "Geschlecht", 
                       limits = c("männl.", "weibl.", "unbekannt"),
                       labels = c("männl.", "weibl.", "unbekannt"),
                       values = c("firebrick3", "steelblue3", "black")) +
    guides(color = guide_legend(override.aes = list(size=7,linetype=1))) +
    theme_gray(30) 
}

# Erstellen der Plots
fp_raw <- ferraty_plot_raw(0) +
  ylab("Körpergröße in cm") +
  xlab("Alter in Jahren")  

# Abspeichern der Grafiken
ggsave(filename = "Grafiken/ferraty_plot_raw.pdf", 
       plot = fp_raw, 
       height = 7, width = 13)
ggsave(filename = "Grafiken/ferraty_plot_raw_maxima.pdf", 
       plot = fp_raw + geom_hline(aes(yintercept = maximum), linetype = "dashed", size = 1), 
       height = 7, width = 13)






# Funktion zur Erstellung der Plots
ferraty_plot <- function(deriv, dat = growth_long_short) {
  ggplot(dat[dat$deriv == deriv,]) +
    geom_line(aes(time, height, colour = Geschlecht, group = ID), size = 1) +
    scale_color_manual(name = "Geschlecht", 
                       limits = c("männl.", "weibl.", "unbekannt"),
                       labels = c("männl.", "weibl.", "unbekannt"),
                       values = c("firebrick3", "steelblue3", "black")) +
    guides(color = guide_legend(override.aes = list(size=7,linetype=1))) +
    theme_gray(30) +
    geom_hline(aes(yintercept = maximum), linetype = "dashed", size = 1) +
    # geom_hline(aes(yintercept = minimum)) +
    # Einfügen der vertikalen Pfeile
    geom_segment(aes(x = 12.5,
                     xend = 12.5,
                     y = dat[dat$Geschlecht == "unbekannt" & dat$deriv == deriv, "maximum"][1],
                     yend = dat[dat$Geschlecht == "männl." & dat$deriv == deriv, "maximum"][1]),
                 size = 1.5,
                 arrow = arrow(ends = "both")) +
    geom_segment(aes(x = 12,
                     xend = 12,
                     y = dat[dat$Geschlecht == "unbekannt" & dat$deriv == deriv, "maximum"][1],
                     yend = dat[dat$Geschlecht == "weibl." & dat$deriv == deriv, "maximum"][1]),
                 size = 2,
                 arrow = arrow(ends = "both"))
}

# Erstellen der Plots
fp0 <- ferraty_plot(0) +
  ylab("Körpergröße in cm") +
  xlab("Alter in Jahren")  

# Abspeichern der Grafiken
ggsave(filename = "Grafiken/ferraty_plot.pdf", plot = fp0, height = 7, width = 13)
ggsave(filename = "Grafiken/ferraty_plot_zoom.pdf", 
       plot = fp0 + coord_cartesian(ylim = c(160, 185), xlim = c(10, 18)), 
       height = 7, width = 13)






# berechne Abständer der Maximumsmetrik
# Berechne Klassenwskten
unique(growth_long_short$maximum)

# read in the height of the kids
b <- 182
un <- 179
g <- 168

# Calculate the distances
dist_b <- abs(un - b)
dist_g <- abs(un - g)

# Define Triangular Kernel
K <- function(u) {
  if(u > -1 & u < 1) {
    return(1 - abs(u))
  } 
  else {
    return(0)
  }
}

# Define bandwidth h = 20
h = 20

# Get the class probs
pi_b <- K(dist_b / h) / (K(dist_b / h) + K(dist_g / h))
# 0.65

pi_g <- K(dist_g / h) / (K(dist_b / h) + K(dist_g / h))
# 0.35
