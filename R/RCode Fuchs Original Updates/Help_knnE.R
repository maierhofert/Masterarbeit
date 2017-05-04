
### Help file for the application of the functional k-nearest-neighbor 
###     ensemble (knnE)
### The following is an example of how to estimate the ensemble coefficients 
###     of the knnE described in Fuchs et al. (2015), Chem. Lab. 146, 186-197

#############################################
# load calculation function of the posterior probabilities/
# ensemble members
#############################################
rm(list = ls())
source(file = "pi_calculation.r")


                            
#############################################
# load package for coefficient estimation
#############################################
library(limSolve)



#############################################
# definition of the simulation study set up
#############################################
### load data file "CellData_CalibrationValidationDatasets.rdata"
### containing variables:
### 
### list "x" with elements "FET", "IDES", and "CLARK" containing
### the respective cell chip data signals of Q=89 measurement points 
### and N=120 observations
### 
### vector "y" with the true response (coded in "0" for "no AAP" and 
### "1" for "2.5mM AAP")
###
### lists "x.inb.cal"/ "x.inb.val" with elements "FET", "IDES", and 
### "CLARK" containing the calibration and validation data sets of
### the 25times repeated 15-fold CV
###
### vectors "y.inb.cal"/ "y.inb.val" with the calibration and validation 
### data of the true response (coded in "0" for "no AAP" and 
### "1" for "2.5mM AAP")
###
### the vector "timee" with the measurement points (in [min])

load(paste("CellData_CalibrationValidationDatasets.rdata", sep = ""))
tt <- timee

### number of covariate types
nb.covs <- 3
### Please note: this variable gives the number of different types of functional 
###     covariates. It always has to be defined. In this help-file example, the number 
###     of functional covariate types is 3, namely "FET", "IDES", and "CLARK" (each 
###     list element in "x" contains one covariate type). If only one functional 
###     covariate type is present in the data, please set this variable equal to 1.
### actual classes
y.classes.bin <- as.integer(attributes(factor(y.inb.cal[[1]][[1]]))$levels)




### the following variables have to be set by the user in order to apply the knnE
      
      ### "knN": set of predefined numbers of nearest neighbours
      ### "semimets": semi-metrics used in the following example (this vector can include every 
      ###     semi-metric implemented in the file "semimetrics_definition.r", called by "pi_calculation.r"; 
      ###     for example, "metric.eucl" and "metric.2.eucl" are identical in calculating the Euclidian 
      ###     distance of two curves, but "metric.eucl" uses the original covariates while "metric.eucl" uses 
      ###     curve-wise centered covariates)
      Kfold <- 15
      KW <- 25
      knN <- c(1, 5, 11, 21)
      semimets <- c("metric.eucl", "metric.shorteucl", "metric.mean", "metric.relareas", 
          "metric.jump", "metric.max", "metric.min", "metric.points", "metric.scan", "metric.2.eucl", 
          "metric.2.shorteucl", "metric.2.mean", "metric.2.relareas", "metric.2.max", "metric.2.min", 
          "metric.2.points", "metric.2.scan")
                  
      ### "lengthh": number of measurement points
      ### "l.deg": list of predefined set of orders of derivation
      ### "l.tvec": list of predefined observation points (used in "metric.jump")
      ### "l.D.small": list of predefined domains of defintion (used in "metric.shorteucl", 
      ###     "metric.relareas", and their centered counterparts)
      lengthh <- length(tt)
      l.deg <- l.tvec <- l.D.small <- l.tt2 <- l.phi.func <- l.ii <- list()
      l.deg[[1]] <- 0
      l.deg[[2]] <- 1
      l.deg[[3]] <- 2
      l.tvec[[1]] <- c(36, 39, 36, 39)
      l.tvec[[2]] <- c(65, 68, 65, 68)
      l.D.small[[1]] <- list(tt[1:35])
      l.D.small[[2]] <- list(tt[36:40])
      l.D.small[[3]] <- list(tt[41:64])
      l.D.small[[4]] <- list(tt[65:69])
      l.D.small[[5]] <- list(tt[70:length(tt)])
      
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
      #kernel.func <- kernel1



#############################################
# calculate and save the ensemble members and coefficients
# on the whole data (results table 5 in the mentioned publication)
#############################################
### for details on the list "pis", see file "pi_calculation.r"
pis <- list()
pis <- pi.estimation(semimets = semimets, nb.covs = nb.covs, x = x, y = y, classes = y.classes.bin, 
                     kernel.func = kernel1, tt = tt, knN = knN, l.deg = l.deg, l.D.small = l.D.small, 
                     l.ii = l.ii, l.tvec = l.tvec, l.tt2 = l.tt2, jj = 3, no.sim = 0, phi.func = phi1, 
                     point.list = point.list)
# Liste 
# Enthält uA pi.hat
# und P.tot und z (die beiden werden für die Optimierung gebraucht)

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
filee = paste("Results_CellChip.rdata", sep = "")
if (file.exists(filee) == FALSE) save(file = filee, Cs = Cs, pis = pis)









################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################



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
Kfold <- 15
KW <- 25
semimets.test <- c("metric.eucl.test", "metric.shorteucl.test", "metric.mean.test", "metric.relareas.test", 
              "metric.jump.test", "metric.max.test", "metric.min.test", "metric.points.test", "metric.scan.test", "metric.2.eucl.test", 
              "metric.2.shorteucl.test", "metric.2.mean.test", "metric.2.relareas.test", "metric.2.max.test", "metric.2.min.test", 
              "metric.2.points.test", "metric.2.scan.test")

### the variables that have to be set by the user in order to apply the knnE, i.e. the above 
###     definitions of regions, points of impact, etc., will be reused in this example



#############################################
# calculate and save the ensemble members and coefficients,
# Brier scores and misclassification rates for both,
# calibration and validation data, by a 25times replicated
# 15-fold CV
#############################################
Cs.CV <- pis.CV <- pis.CV.val <- vector("list")
BS.cal <- BS.val <- MCR.cal <- MCR.val <- vector()
for (w in 1:KW) {
    Cs.CV[[w]] <- pis.CV[[w]] <- pis.CV.val[[w]] <- vector("list")
    for (rep.no in 1:Kfold) {
        Cs.CV[[w]][[rep.no]] <- pis.CV[[w]][[rep.no]] <- pis.CV.val[[w]][[rep.no]] <- vector("list")
        pii <- list()
        ### calculate posterior probability estimates for the actual calibration data set
        pii <- pi.estimation(semimets = semimets, nb.covs = nb.covs, x = x.inb.cal[[w]][[rep.no]], 
            y = y.inb.cal[[w]][[rep.no]], classes = y.classes.bin, kernel.func = kernel1, 
            tt = tt, knN = knN, l.deg = l.deg, l.D.small = l.D.small, l.ii = l.ii, 
            l.tvec = l.tvec, l.tt2 = l.tt2, jj = 3, no.sim = 0, phi.func = phi1, 
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
        
        
        
        ### calculate Brier score and MCR for actual calibration data set
        inb1 <- inb2 <- inb3 <- inb4 <- classes <- y.pred <- vector()
        inb1 <- pis.CV[[w]][[rep.no]]$z - (pis.CV[[w]][[rep.no]]$P.tot %*% Cs.CV[[w]][[rep.no]])
        if (rep.no == 1 && w == 1) 
            BS.cal <- t(inb1) %*% inb1 else BS.cal <- c(BS.cal, t(inb1) %*% inb1)
        classes <- as.integer(attributes(factor(y.inb.cal[[w]][[rep.no]]))$levels)
        inb1 <- as.vector(pis.CV[[w]][[rep.no]]$P.tot %*% Cs.CV[[w]][[rep.no]])
        inb2 <- length(classes)
        inb3 <- 1
        for (b in 1:length(y.inb.cal[[w]][[rep.no]])) {
            if (is.na(inb1[inb3]) == FALSE) {
              if (length(which(inb1[inb3:(inb3 + inb2 - 1)] == max(inb1[inb3:(inb3 + 
                inb2 - 1)]))) == 1) 
                y.pred[b] <- classes[which(inb1[inb3:(inb3 + inb2 - 1)] == max(inb1[inb3:(inb3 + 
                  inb2 - 1)]))] else y.pred[b] <- -1
            } else y.pred[b] <- NA
            inb3 <- inb3 + inb2
        }
        inb4 <- y.inb.cal[[w]][[rep.no]] - y.pred
        if (rep.no == 1 && w == 1) 
            MCR.cal <- mean(inb4 != 0, na.rm = TRUE) else MCR.cal <- c(MCR.cal, mean(inb4 != 0, na.rm = TRUE))
        
        
        
        ### calculate posterior probability estimates for the actual validation data set
        ### calculate Brier score and MCR for actual validation data set
        pii <- list()
        pii <- pi.estimation.test(semimets.test = semimets.test, nb.covs = nb.covs, xcal = x.inb.cal[[w]][[rep.no]], 
            ycal = y.inb.cal[[w]][[rep.no]], x = x.inb.val[[w]][[rep.no]], y = y.inb.val[[w]][[rep.no]], 
            classes = y.classes.bin, kernel.func = kernel1, tt = tt, knN = knN, l.deg = l.deg, l.D.small = l.D.small, 
            l.ii = l.ii, l.tvec = l.tvec, l.tt2 = l.tt2, jj = 3, no.sim = 0, phi.func = phi1, point.list = point.list)
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
        
        
        
        show(paste("done for: w", w, ", rep.no", rep.no))
        save(file = filee, Cs = Cs, pis = pis, Cs.CV = Cs.CV, pis.CV = pis.CV, 
            pis.CV.val = pis.CV.val, BS.cal = BS.cal, MCR.cal = MCR.cal, BS.val = BS.val, 
            MCR.val = MCR.val)
    }  #end (rep.no in 1:nb.reps)
    save(file = filee, Cs = Cs, pis = pis, Cs.CV = Cs.CV, pis.CV = pis.CV, pis.CV.val = pis.CV.val, 
        BS.cal = BS.cal, MCR.cal = MCR.cal, BS.val = BS.val, MCR.val = MCR.val)
    show(paste("done for: w", w))
    ptime <- proc.time()
    if (w == 1) 
        show(paste("time used for this parameter constellation:", attributes(ptime)$names[1], 
            as.numeric(ptime[1]), attributes(ptime)$names[2], as.numeric(ptime[2]), 
            attributes(ptime)$names[3], as.numeric(ptime[3])))
}  # for (w in 1:KW){  



#############################################
# summary of estimation failures across all folds and repetitions
#############################################
cvec<-vector()
for (a in 1:length(Cs.CV))
  for ( b in 1:length(Cs.CV[[a]]))
    cvec<-rbind(cvec,Cs.CV[[a]][[b]])
show(paste("Number of estimation failures in the crossvalidation:",length(which(rowSums(cvec)==0))))



#############################################
# boxplots of the brier score and MCR across all folds and repetitions
#############################################
pdf("Plots_cells_BS_MCRval.pdf",width=17)
par(cex=1.5,cex.axis=1.5,cex.lab=1.5,font=2,cex.sub=1.5,mar=c(10,4.5,3,1),mfrow=c(1,2))
boxplot(BS.val,at=1,xlab="",ylab="Brier score",font=2,cex.lab=1.5)
grid()
boxplot(MCR.val,at=1,xlab="",ylab="MCR",font=2,cex.lab=1.5)
grid()
dev.off()

