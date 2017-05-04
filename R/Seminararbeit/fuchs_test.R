# This is the semimetrics/degree of derivation test for the fuchs paper on the growth data set

### setwd("L:/Users/maierhofert/Documents/FDA Seminar")

# Einlesen aller benötigten Funktionen
source("growth_study_data.R")

################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################

### Please note: this variable gives the number of different types of functional
###     covariates. It always has to be defined. In this help-file example, the number
###     of functional covariate types is 3, namely "FET", "IDES", and "CLARK" (each
###     list element in "x" contains one covariate type). If only one functional
###     covariate type is present in the data, please set this variable equal to 1.
### actual classes
y.classes.bin <- as.integer(attributes(factor(y.inb.cal[[1]][[1]]))$levels)

### number of covariate types
nb.covs <- 1

### "knN": set of predefined numbers of nearest neighbours
knN <- c(1, 5, 11)

### The following is an example of how to estimate the ensemble coefficients
###     of the knnE from a data set and evaluate new curves, i.e. predict the
###     class of a new curve

#############################################
# load calculation function of the posterior probabilities/
# ensemble members
#############################################
### Please not that, in order to predict the class of a new curve, a second file named
###    "semimetrics_definition_val.r", called by "pi_calculation_val.r", has to be called.
###     Here, the semi-metrics are defined analogously to those in the modeling step. The
###     only difference is that the posterior probabilities of the new curve haven to be computed
###     relatively to all curves of the modeling data set.
source(file = "pi_calculation.r")
source(file = "pi_calculation_val.r")



#############################################
# load package for coefficient estimation
#############################################
library(limSolve)



### "Kfold": number of folds in the CV
### "KW": number of replications of the CV
### "semimets.test": semi-metrics used in the following example (this vector can include every
###     semi-metric implemented in the file "semimetrics_definition_val.r", called by "pi_calculation_val.r";
###     defined analogously to the modeling semi-metrics)
Kfold <- 10
KW <- 10

# Liste aller Semimetrik-Kombinationen
semimets.list <- list(c("metric.eucl"), c("metric.max"), c("metric.min"),
                      c("metric.eucl", "metric.max", "metric.min"))
semimets.test.list <- list(c("metric.eucl.test"), c("metric.max.test"), c("metric.min.test"),
                           c("metric.eucl.test", "metric.max.test", "metric.min.test"))

### "lengthh": number of measurement points
### "l.deg": list of predefined set of orders of derivation
### "l.tvec": list of predefined observation points (used in "metric.jump")
### "l.D.small": list of predefined domains of defintion (used in "metric.shorteucl",
###     "metric.relareas", and their centered counterparts)
lengthh <- length(tt)
l.deg <- l.tvec <- l.D.small <- l.tt2 <- l.phi.func <- l.ii <- list()
# l.deg[[1]] <- 0
# l.deg[[2]] <- 1
# l.deg[[3]] <- 2

# Liste aller Ableitungs-Kombinationen
# l.deg.list <- list(l.deg[[1]], l.deg[[2]], l.deg[[3]], l.deg)
l.deg.list <- list(list(0), list(1), list(2), list(0, 1, 2))


## Does not make (much) sense for growth
# l.tvec[[1]] <- c(36, 39, 36, 39)
# l.tvec[[2]] <- c(65, 68, 65, 68)
l.D.small[[1]] <- list(tt[1:9]) # 1-6 Jahre
l.D.small[[2]] <- list(tt[10:19]) # 7 - 12 Jahre
l.D.small[[3]] <- list(tt[21:31]) # 12.5 - 18 Jahre

### "l.tt2": list of predefined measurement points (used in "metric.relareas" and its centered
###     counterpart if the covariates are defined on differing domains)
### "l.ii": list of predefined numbers of domains of defintion  (used in "metric.relareas" and its
###     centered counterpart)
### "point.list": list of predefined points of impact  (used in "metric.points" and its centered
###     counterpart)
### "no.sim": variable to indicate which of various settings, i.e. which values for the variables
###     sigma and tau are used in "metric.scan" and its centered counterpart (the values of sigma
###     and tau are defined in ""semimetrics_definition.r")
### "jj": variable to indicate which of the curve regions defined in the list "l.D.small" is used as
###     "D_2" in "metric.relareas" and its centered counterpart
### "kernel.func": name of the kernel function used in the posterior probability estimation to give
###     a weight profil to the distances calculated from the single semi-metrics (optional; has to be
###     given over to the "pi.estimation" function call, but is not used yet)
l.tt2[[1]] <- tt
l.ii[[1]] <- 1
l.ii[[2]] <- 3
point.list <- seq(1, length(tt), length = 10)
no.sim <- 0
jj <- 3


### the variables that have to be set by the user in order to apply the knnE, i.e. the above
###     definitions of regions, points of impact, etc., will be reused in this example

m.MCR.rf.val <- m.BS.val <- m.MCR.val <- matrix(data = NA,
                                                nrow = length(l.deg.list),
                                                ncol = length(semimets.list))



# Loop for test study
for(a in 1:length(l.deg.list)) {
  print(paste(c("Beginne Ableitungsgrad", l.deg.list[[a]], "-----------------------------------")))
  this.l.deg <- l.deg.list[[a]]

  # Einzelne Semimetriken
  for(semimet.no in 1:length(semimets.list)) {
    print(paste(c("Beginne Semimetrik", semimets.list[[semimet.no]])))
    this.semimets = semimets.list[[semimet.no]]
    this.semimets.test = semimets.test.list[[semimet.no]]

    #############################################
    # calculate and save the ensemble members and coefficients,
    # Brier scores and misclassification rates for both,
    # calibration and validation data, by a 10times replicated
    # 10-fold CV
    #############################################
    Cs.CV <- pis.CV <- pis.CV.val <- vector("list")
    this.MCR.rf.val <- MCR.rf.val <- BS.cal <- BS.val <- MCR.cal <- MCR.val <- vector()
    for (w in 1:KW) {
      Cs.CV[[w]] <- pis.CV[[w]] <- pis.CV.val[[w]] <- vector("list")
      for (rep.no in 1:Kfold) {
        Cs.CV[[w]][[rep.no]] <- pis.CV[[w]][[rep.no]] <- pis.CV.val[[w]][[rep.no]] <- vector("list")
        pii <- list()
        ### calculate posterior probability estimates for the actual calibration data set
        pii <- pi.estimation(semimets = this.semimets,
                             nb.covs = nb.covs,
                             x = x.inb.cal[[w]][[rep.no]],
                             y = y.inb.cal[[w]][[rep.no]],
                             classes = y.classes.bin, kernel.func = kernel1,
                             tt = tt,
                             knN = knN,
                             l.deg = this.l.deg,
                             l.D.small = l.D.small, l.ii = l.ii,
                             l.tvec = l.tvec, l.tt2 = l.tt2, jj = 3,
                             no.sim = 0, phi.func = phi1,
                             point.list = point.list)
        pii$metrics <- list()
        pis.CV[[w]][[rep.no]] <- pii

        ### estimate coefficients for actual calibration data set up to 3 times (because of
        ### possible computing instability)
        tryy <- 0
        checkk <- 1
        while (checkk == 1) {
          Cs.CV[[w]][[rep.no]] <- vector()

          ### by limSolve -> lsei
          Q.vec.lsei <- inb <- vector()
          A <- (pii$P.tot) * (-1)
          b <- (pii$z) * (-1)
          G <- matrix(0, ncol(A), ncol(A))
          diag(G) <- 1
          hh <- rep(0, length = ncol(A))
          Q.vec.lsei <- try(lsei(A = A, B = b, G = G, H = hh)$X)
          if (length(Q.vec.lsei) > 0)
            Cs.CV[[w]][[rep.no]] <- Q.vec.lsei

          if (class(Cs.CV[[w]][[rep.no]]) != "try-error")
            checkk <- 0
          if (checkk == 1)
            tryy <- tryy + 1
          if (tryy >= 3) {
            checkk <- 0
            Cs.CV[[w]][[rep.no]] <- rep(0, length = ncol(pii$P.tot))
            show(paste("WARNING! estimation of coefficients for data w", w,
                       ", rep.no", rep.no, " failed."))
          }
        }
        rm(pii)


        ### calculate posterior probability estimates for the actual validation data set
        ### calculate Brier score and MCR for actual validation data set
        pii <- list()
        # debugonce(pi.estimation.test)
        pii <- pi.estimation.test(semimets.test = this.semimets.test,
                                  nb.covs = nb.covs,
                                  xcal = x.inb.cal[[w]][[rep.no]],
                                  ycal = y.inb.cal[[w]][[rep.no]],
                                  x = x.inb.val[[w]][[rep.no]],
                                  y = y.inb.val[[w]][[rep.no]],
                                  classes = y.classes.bin,
                                  kernel.func = kernel1,
                                  tt = tt, knN = knN,
                                  l.deg = this.l.deg,
                                  l.D.small = l.D.small,
                                  l.ii = l.ii, l.tvec = l.tvec, l.tt2 = l.tt2, jj = 3,
                                  no.sim = 0,
                                  phi.func = phi1, point.list = point.list)


        pii$metrics <- list()
        pis.CV.val[[w]][[rep.no]] <- pii
        rm(pii)

        inb <- inb1 <- inb2 <- inb3 <- inb4 <- classes <- y.pred <- vector()
        inb <- pis.CV.val[[w]][[rep.no]]$P.tot
        if (sum(is.na(pis.CV.val[[w]][[rep.no]]$P.tot)) != 0)
          inb[is.na(inb)] <- 0
        inb1 <- pis.CV.val[[w]][[rep.no]]$z - (inb %*% Cs.CV[[w]][[rep.no]])
        if (rep.no == 1 && w == 1)
          BS.val <- t(inb1) %*% inb1 else BS.val <- c(BS.val, t(inb1) %*% inb1)
        classes <- as.integer(attributes(factor(y.inb.val[[w]][[rep.no]]))$levels)
        inb1 <- as.vector(inb %*% Cs.CV[[w]][[rep.no]])
        inb2 <- length(classes)
        inb3 <- 1
        for (b in 1:length(y.inb.val[[w]][[rep.no]])) {
          if (is.na(inb1[inb3]) == FALSE) {
            if (length(which(inb1[inb3:(inb3 + inb2 - 1)] == max(inb1[inb3:(inb3 +
                                                                              inb2 - 1)]))) == 1)
              y.pred[b] <- classes[which(inb1[inb3:(inb3 + inb2 - 1)] == max(inb1[inb3:(inb3 +
                                                                                          inb2 - 1)]))] else y.pred[b] <- -1
          } else y.pred[b] <- NA
          inb3 <- inb3 + inb2
        }
        inb4 <- y.inb.val[[w]][[rep.no]] - y.pred
        if (rep.no == 1 && w == 1)
          MCR.val <- mean(inb4 != 0, na.rm = TRUE) else MCR.val <- c(MCR.val, mean(inb4 != 0, na.rm = TRUE))

        # Random Forest Implementation
        require("randomForest")
        dat <- data.frame(yrf = factor(y.inb.cal[[w]][[rep.no]]),
                          data.frame(pis.CV[[w]][[rep.no]]$P.tot)[c(TRUE, FALSE),])
        rf <- randomForest(yrf ~ ., data = dat)

        this.MCR.rf.val <- abs(mean(
          predict(rf, newdata = data.frame(pis.CV.val[[w]][[rep.no]]$P.tot)[c(TRUE, FALSE),],
                  type = "response") !=
            y.inb.val[[w]][[rep.no]]
        ))

        if (rep.no == 1 && w == 1)
          MCR.rf.val <- this.MCR.rf.val else MCR.rf.val <- c(MCR.rf.val, this.MCR.rf.val)


      }  #end (rep.no in 1:nb.reps)
    }  # for (w in 1:KW){
    # print(MCR.val)

    # Result
    m.MCR.val[a, semimet.no] = mean(MCR.val)
    m.BS.val[a, semimet.no] = mean(BS.val)
    m.MCR.rf.val[a, semimet.no] = mean(MCR.rf.val)
    MCR.val = BS.val = -1
  }
}

m.MCR.val
m.BS.val
m.MCR.rf.val
