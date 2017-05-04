# Run this File after running fuchs_test.R
# to get get the ensemble weights c_l

# growth study data
source("growth_study_data.R")

### number of covariate types
nb.covs <- 1

# semimetrics being used
semimets <- c("metric.eucl", "metric.max", "metric.min")


### "lengthh": number of measurement points
### "l.deg": list of predefined set of orders of derivation
### "l.tvec": list of predefined observation points (used in "metric.jump")
### "l.D.small": list of predefined domains of defintion (used in "metric.shorteucl", 
###     "metric.relareas", and their centered counterparts)
lengthh <- length(tt)
l.deg <- l.tvec <- l.D.small <- l.tt2 <- l.phi.func <- l.ii <- list()
# l.deg <- list(1)
l.deg[[1]] <- 0
l.deg[[2]] <- 1
l.deg[[3]] <- 2

#############################################
# calculate and save the ensemble members and coefficients
# on the whole data (results table 5 in the mentioned publication)
#############################################
### for details on the list "pis", see file "pi_calculation.r"
pis <- list()
# debugonce(pi.estimation)
pis <- pi.estimation(semimets = semimets, 
                     nb.covs = nb.covs, 
                     x = x, y = y, 
                     classes = y.classes.bin, 
                     kernel.func = kernel1, tt = tt, knN = knN, 
                     l.deg = l.deg, l.D.small = l.D.small, 
                     l.ii = l.ii, l.tvec = l.tvec, l.tt2 = l.tt2, 
                     jj = 3, no.sim = 0, phi.func = phi1, 
                     point.list = point.list)
# Liste
# Enthält uA pi.hat
# und P.tot und z (die beiden werden f?r die Optimierung gebraucht)

### delete the single semi-metric results "pis$metrics" to reduce the required memory (optional)
pis$metrics <- list()

### calculate the ensemble coefficients "Cs" by limSolve -> lsei
Cs <- vector()
Q.vec.lsei <- inb <- vector()
A <- (pis$P.tot) * (-1)
b <- (pis$z) * (-1)
G <- matrix(0, ncol(A), ncol(A))
diag(G) <- 1
hh <- rep(0, length = ncol(A))

# Optimierung (6) des quadratischen Optimierungsproblems
Q.vec.lsei <- try(lsei(A = A, B = b, G = G, H = hh)$X)
if (length(Q.vec.lsei) > 0) Cs <- Q.vec.lsei

# Abspeichern der geschätzen Cs und pis
filee = paste("Results_growth.rdata", sep = "")
if (file.exists(filee) == FALSE) save(file = filee, Cs = Cs, pis = pis)




#############################################
# build a coefficient table in which every column gives
# the number of one tuple, i.e. coefficient ID, and every
# row gives the tuple details, i.e. to which covariate
# number (in case of multiple functional covariates), degree
# of derivative, number "k" of nearest neighbours, semi-metric,
# and number of semi-metric parameter (if a choice of the same is 
# necessary,e.g. D_1 in the short_Eucl semi-metric) the
# coefficient ID corresponds
#############################################
metrics <- list()
coeff.table <- vector()
x <- x.inb.val[[1]][[1]][[1]]
y <- y.inb.val[[1]][[1]]
y.classes <- y.classes.bin
for (b in 1:length(semimets)) {
  metrics[[b]] <- list()
  metrics[[b]] <- metric(semimets = semimets[b], nb.covs = nb.covs, x = x, y = y, classes = y.classes, tt = tt, knN = knN, l.deg = l.deg, l.D.small = l.D.small, 
                         l.ii = l.ii, l.tvec = l.tvec, l.tt2 = l.tt2, jj = 3, no.sim = 0, phi.func = phi1, point.list = point.list, deg = 1)
}
for (covs in 1:nb.covs) for (deg in 1:length(l.deg)) for (nb.k in 1:length(knN)) for (b in 1:length(semimets)) if (is.list(metrics[[b]]$distances[[1]])) {
  for (d in 1:length(metrics[[b]]$distances)) {
    coeff.table <- rbind(coeff.table, c(covs, deg, nb.k, b, d))
  }
} else coeff.table <- rbind(coeff.table, c(covs, deg, nb.k, b, 1))
coeff.table <- as.data.frame(t(coeff.table), row.names = c("Covariate no.", "(Degree of derivative + 1)", "Number of knN", "Semimetric no.", "Parameter choice no."))
write.table(coeff.table, append = FALSE, file = "CoeffTable.txt", sep = " ", col.names = FALSE, row.names = TRUE, quote = FALSE, na = "NaN", eol = "\n")


# Results
result <- t(rbind(coeff.table, cs = Cs))
colnames(result) <- c("covariate_no", "deg_deriv", "knn_no",
                      "semimet_no", "param_choice_no", "cs")


# Result of the weights
res <- ddply(data.frame(result), .(deg_deriv, semimet_no),
      summarize, cs = round(sum(cs), 2))

# Result of the MMCR estimated by CV
m.MCR <- as.numeric(t(m.MCR.val[-4,-4]))

res$MMCR <- m.MCR * 100
res$Ableitung <- factor(res$deg_deriv - 1, levels = c(2, 1, 0))
res$Semimetrik <- "Eukl."
res$Semimetrik[res$semimet_no == 2] <- "Maximum"
res$Semimetrik[res$semimet_no == 3] <- "Minimum"

## plot data
library("ggplot2")
ensemble_plot <- ggplot(res, aes(Semimetrik, Ableitung)) +
  geom_tile(aes(fill = MMCR)) + 
  geom_text(aes(label = round(cs * 100, 3)),
            size = 10) +
  # scale_fill_gradient("MMCR\nin %", low = "palegreen2", high = "white") +
  scale_fill_gradient2("MMCR\nin %", low = "green3", mid = "snow", high = "firebrick2", midpoint = 15) +
  theme_gray(30)
ensemble_plot

# Abspeichern der Grafiken
ggsave(filename = "Grafiken/ensemble_plot.pdf", 
       plot = ensemble_plot, height = 7, width = 10)





