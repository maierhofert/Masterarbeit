

#############################################
# load the semi-metric definitions
#############################################
source(file = "semimetrics_definition_val.r")









#############################################
# definition of phi-functions for semi-metric "scan"
#############################################

### gaussian
phi1 <- function(tt, t.l, sigmaa) {
  phi1 <- (1/(sigmaa * sqrt(2 * pi))) * exp(-0.5 * (((tt - t.l)/sigmaa)^2))
  phi1
}






#############################################
# posterior probability estimation (prediction)
#############################################

### posterior probability estimate calculation:
### the probability that x_vi falls in class g using a 
### semi-metric d_v based on the k nearest neighbours
### and data derivation a


### the function returns a list with the following elements:
###
### "metrics" contains the distances for each order of deri-
### vation "deg" of covariate type "covs", calculated for each
### semi-metric "b"
###
### "pi.hat" are the posterior probability estimates
### representing the ensemble members
### they are calculated via leaving-one-out for each cova-
### riate type "covs" and observation "a", each for each tuple of
### semi-metrics "metrics", order of derivation "deg" and 
### number of nearest neighbours "nb.k"
### 
### "z" is a vector of size NG containing the true response,
### coded into ones and zeros
###
### "P" is a list containing matrices of size NG x [# semi-metrics]
### for each covariate type "covs", order of derivation "deg" and 
### number of nearest neighbours "nb.k"
### 
### "P.tot" is the complete posterior probability matrix of size
### NG x p
### 
### 

pi.estimation.test <- function(semimets.test, nb.covs, xcal, ycal, x, y, classes, kernel.func, tt, knN, l.deg, l.D.small, l.ii, l.tvec, l.tt2, ...) {
    try(rm(inb3))
    pi.hat.igj <- P <- metrics <- list()
    z <- matrix(NA, length(y), length(classes))
    xtest <- x
    for (covs in 1:nb.covs) {
        pi.hat.igj[[covs]] <- list()
        P[[covs]] <- metrics[[covs]] <- list()
        for (deg in 1:length(l.deg)) {
            pi.hat.igj[[covs]][[deg]] <- list()
            P[[covs]][[deg]] <- metrics[[covs]][[deg]] <- list()
            for (a in 1:nrow(xtest[[covs]])) {
                metrics[[covs]][[deg]][[a]] <- list()
                x <- matrix(NA, nrow = nrow(xcal[[covs]]) + 1, ncol = ncol(xcal[[covs]]))
                x <- rbind(xtest[[covs]][a, ], xcal[[covs]])
                for (b in 1:length(semimets.test)) {
                  metrics[[covs]][[deg]][[a]][[b]] <- list()
                  metrics[[covs]][[deg]][[a]][[b]] <- metric(semimets = semimets.test[b], x, tt, l.deg[[deg]], l.D.small, l.tt2, l.ii, l.tvec, ...)
                }
            }
            for (nb.k in 1:length(knN)) {
                pi.hat.igj[[covs]][[deg]][[nb.k]] <- list()
                P[[covs]][[deg]][[nb.k]] <- vector()
                for (a in 1:nrow(xtest[[covs]])) {
                  pi.hat.igj[[covs]][[deg]][[nb.k]][[a]] <- matrix(NA, nrow = length(classes))
                  for (b in 1:length(semimets.test)) {
                    if (is.list(metrics[[covs]][[deg]][[a]][[b]]$distances[[1]])) {
                      for (d in 1:length(metrics[[covs]][[deg]][[a]][[b]]$distances)) {
                        ord.metric <- order(metrics[[covs]][[deg]][[a]][[b]]$distances[[d]][[1]])
                        if (length(ord.metric) < (nrow(xtest[[covs]]) - 1)) 
                          ord.metric <- rep(ord.metric, length = (nrow(xtest[[covs]]) - 1))
                        short.class <- ycal
                        inb.pi <- vector()
                        for (cc in 1:length(classes)) {
                          inb <- inb2 <- coded.class <- ord.coded.class <- vector()
                          coded.class[short.class != classes[cc]] <- 0
                          coded.class[short.class == classes[cc]] <- 1
                          ord.coded.class <- coded.class[ord.metric[1:knN[nb.k]]]
                          inb.pi <- c(inb.pi, sum(ord.coded.class)/length(ord.coded.class))
                          if (y[a] == classes[cc] && covs == 1 && nb.k == 1 && deg == 1) 
                            z[a, cc] <- 1
                          if (y[a] != classes[cc] && covs == 1 && nb.k == 1 && deg == 1) 
                            z[a, cc] <- 0
                        }
                        if (b == 1) 
                          pi.hat.igj[[covs]][[deg]][[nb.k]][[a]] <- inb.pi else pi.hat.igj[[covs]][[deg]][[nb.k]][[a]] <- cbind(pi.hat.igj[[covs]][[deg]][[nb.k]][[a]], inb.pi, deparse.level = 0)
                      }
                    } else {
                      ord.metric <- order(metrics[[covs]][[deg]][[a]][[b]]$distances[[1]])
                      if (length(ord.metric) < (nrow(xtest[[covs]]) - 1)) 
                        ord.metric <- rep(ord.metric, length = (nrow(xtest[[covs]]) - 1))
                      short.class <- ycal
                      inb.pi <- vector()
                      for (cc in 1:length(classes)) {
                        inb <- inb2 <- coded.class <- ord.coded.class <- vector()
                        coded.class[short.class != classes[cc]] <- 0
                        coded.class[short.class == classes[cc]] <- 1
                        ord.coded.class <- coded.class[ord.metric[1:knN[nb.k]]]
                        inb.pi <- c(inb.pi, sum(ord.coded.class)/length(ord.coded.class))
                        if (y[a] == classes[cc] && covs == 1 && nb.k == 1 && deg == 1) 
                          z[a, cc] <- 1
                        if (y[a] != classes[cc] && covs == 1 && nb.k == 1 && deg == 1) 
                          z[a, cc] <- 0
                      }
                      if (b == 1) 
                        pi.hat.igj[[covs]][[deg]][[nb.k]][[a]] <- inb.pi else pi.hat.igj[[covs]][[deg]][[nb.k]][[a]] <- cbind(pi.hat.igj[[covs]][[deg]][[nb.k]][[a]], inb.pi, deparse.level = 0)
                    }  # if(is.list(metrics[[covs]][[deg]][[a]][[b]]$distances[[1]]))
                  }  # for (b in 1:length(semimets.test))
                  if (a == 1) {
                    inb3 <- pi.hat.igj[[covs]][[deg]][[nb.k]][[a]] 
                  } else {
                    # # Original Code Fuchs. 
                    # Über Scoping wird die Variable semimets im Global Environment verwendet.
                    # Dies führt dazu, dass hier mit der falschen Länge verglichen wird und
                    # immer in diesen Fall gesprungen wird.
                    # if (length(semimets)==1){  
                    if (length(semimets.test)==1){
                      inb3 <- c(inb3, pi.hat.igj[[covs]][[deg]][[nb.k]][[a]])
                    } else {
                      inb3 <- rbind(inb3, pi.hat.igj[[covs]][[deg]][[nb.k]][[a]], deparse.level = 0)
                    }
                  }
                }  # for (a in 1:nrow(xtest[[covs]]))
                P[[covs]][[deg]][[nb.k]] <- inb3
                inb <- vector()
                if (covs == 1 && nb.k == 1 && deg == 1) {
                  for (a in 1:nrow(z)) inb <- c(inb, z[a, ])
                  z <- inb
                }
            }  # for (nb.k in 1:length(knN))
        }  #  for (deg in 1:length(l.deg)){
    }  #  for(covs in 1:nb.covs){
    P.tot <- vector()
    for (covs in 1:nb.covs) for (deg in 1:length(l.deg)) for (nb.k in 1:length(knN)) P.tot <- cbind(P.tot, P[[covs]][[deg]][[nb.k]])
    return(list(metrics = metrics, pi.hat = pi.hat.igj, P = P, z = z, P.tot = P.tot))
}


 
