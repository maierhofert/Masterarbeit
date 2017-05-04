# Einlesen aller benötigten Funktionen
source("MY_NPCD_funcCollection_pene.r")
source("growth_study_data.R")

#############################################
# calculate and save the ensemble members and coefficients,
# Brier scores and misclassification rates for both,
# calibration and validation data, by a 10times replicated
# 10-fold CV
#############################################
# Parameters of the crossvalidation
KW = 10
Kfold = 10

BS.val <- MCR.val <- matrix(data = NA, nrow = KW, ncol = Kfold)

## Parameters of the function
# degree of derivation
l.deg = 0:2
semimets = c("deriv", "maximum", "minimum")
m.BS.val <- m.MCR.val <- matrix(data = NA, nrow = length(l.deg), ncol = length(semimets))



# Loop for test study
for(a in 1:length(l.deg)) {
  print(paste("Beginne Ableitungsgrad", l.deg[a]))
  
  for(semimet.no in 1:length(semimets)) {
    print(paste("Beginne Semimetrik", semimets[semimet.no]))
    
    for (w in 1:KW) {
      for (rep.no in 1:Kfold) {
        # Calculation of the prediction
        temp <- my.funopadi.knn.lcv(Classes = y.inb.cal[[w]][[rep.no]],
                                    CURVES = x.inb.cal[[w]][[rep.no]][[1]],
                                    PRED = x.inb.val[[w]][[rep.no]][[1]],
                                    q = l.deg[a], 
                                    nknot = 5, range.grid = c(1, 18),
                                    kind.of.kernel = "triangle",
                                    semimetric = semimets[semimet.no])
        
        # Brier Score on validation data
        BS.val[w, rep.no] <- 2 * sum((y.inb.val[[w]][[rep.no]] - temp$Prob.predicted) ^ 2)
        # Misclassification rate
        MCR.val[w, rep.no] <- mean(abs(y.inb.val[[w]][[rep.no]] - temp$Prob.predicted))
        # MCR.val[w, rep.no] <- mean(abs(y.inb.val[[w]][[rep.no]] - temp$Predicted.classnumber))
        
      }  # rep.no in 1:nb.reps)
    }  # w in 1:KW
    m.BS.val[a, semimet.no] <- mean(BS.val)
    m.MCR.val[a, semimet.no] <- mean(MCR.val)
  } # semimet.no in semimetrics
} # a in l.deg

m.MCR.val
m.BS.val



###############################################################################################
###############################################################################################
# Compute in paralel

library(doSNOW)
library(rbenchmark)
library(foreach)

cluster = makeCluster(5, type = "SOCK")
registerDoSNOW(cluster)

semimets = c("dtw", "dtw_path")

# Loop for test study
res <- foreach(a = 1:length(l.deg), 
               .combine = 'rbind',
               .packages = "dtw",
               .export = c(paste0("semimetric.", semimets),
                           "triangle", "symsolve", "warping_path_dist"))  %:% 
  foreach(semimet.no = 1:length(semimets)) %dopar% {
    BS.val <- MCR.val <- matrix(data = NA, nrow = KW, ncol = Kfold)
    for(w in 1:KW) {
      for(rep.no in 1:Kfold) {
        # Calculation of the prediction
        temp <- my.funopadi.knn.lcv(Classes = y.inb.cal[[w]][[rep.no]],
                                    CURVES = x.inb.cal[[w]][[rep.no]][[1]],
                                    PRED = x.inb.val[[w]][[rep.no]][[1]],
                                    q = l.deg[a], 
                                    nknot = 5, range.grid = c(1, 18),
                                    kind.of.kernel = "triangle",
                                    semimetric = semimets[semimet.no])
        
        # Brier Score on validation data
        BS.val[w, rep.no] <- 2 * sum((y.inb.val[[w]][[rep.no]] - temp$Prob.predicted) ^ 2)
        # Misclassification rate
        MCR.val[w, rep.no] <- mean(abs(y.inb.val[[w]][[rep.no]] - temp$Prob.predicted))
        # MCR.val[w, rep.no] <- mean(abs(y.inb.val[[w]][[rep.no]] - temp$Predicted.classnumber))
        
      }  # rep.no in 1:nb.reps)
    }  # w in 1:KW
    # m.BS.val[a, semimet.no] <- mean(BS.val)
    # m.MCR.val[a, semimet.no] <- mean(MCR.val)
    mean(MCR.val)
}
res

stopCluster(cluster)


round(matrix(as.numeric(res), ncol = 2) * 100, 1)

