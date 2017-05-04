
#############################################
#call of semi-metrics
#############################################
metric.test <- function(semimets, x, tt, deg, l.D.small, l.tt2, l.ii, l.tvec, ...) {
    metrics <- list()
    metrics <- do.call(semimets, args = list(x, tt, deg, l.D.small, l.tt2, l.ii, l.tvec, ...))
    return(metrics)
}




############################################# 
#definition of semi-metrics
############################################# 
metric.eucl.test <- function(x = x, tt = tt, deg = deg, ...) {
    metric.eucl <- list()
    metric.eucl$distances <- list()
    if (length(tt) <= 1) 
        metric.eucl$op.dist <- tt else metric.eucl$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.eucl$distances <- 0
    } else {
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        for (a in 1:1) {
            # (nrow(x))){
            metric.eucl$distances[[a]] <- vector()
            if (a == nrow(x)) {
                inb <- t(apply(x[1:(a - 1), ], MARGIN = 1, FUN = function(x, y) {
                  x - y
                }, y = x[a, ]))
                metric.eucl$distances[[a]] <- sqrt(metric.eucl$op.dist * rowSums(inb^2))
            } else {
                inb <- t(apply(rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ]), MARGIN = 1, FUN = function(x, y) {
                  x - y
                }, y = x[a, ]))
                metric.eucl$distances[[a]] <- sqrt(metric.eucl$op.dist * rowSums(inb^2))
            }
            if (a == 1) {
                metric.eucl$distances[[a]] <- vector()
                inb <- t(apply(x[(a + 1):nrow(x), ], MARGIN = 1, FUN = function(x, y) {
                  x - y
                }, y = x[a, ]))
                metric.eucl$distances[[a]] <- sqrt(metric.eucl$op.dist * rowSums(inb^2))
            }
        }
    }
    metric.eucl
}
############################################# 
metric.shorteucl.test <- function(x = x, tt = tt, deg = deg, l.D.small = l.D.small, ...) {
    metric.shorteucl <- list()
    metric.shorteucl$distances <- list()
    if (length(tt) <= 1) 
        metric.shorteucl$op.dist <- tt else metric.shorteucl$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.shorteucl$distances <- 0
    } else {
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        for (no.Dsmall in 1:length(l.D.small)) {
            metric.shorteucl$distances[[no.Dsmall]] <- list()
            D.small <- l.D.small[[no.Dsmall]]
            minn <- maxx <- x.inb <- vector()
            minn <- which(tt >= min(D.small[[1]]))
            maxx <- which(tt <= max(D.small[[1]]))
            lengthh <- length(maxx)
            x.inb <- x[, minn[1]:maxx[lengthh]]
            for (a in 1:1) {
                # (nrow(x.inb))){
                metric.shorteucl$distances[[no.Dsmall]][[a]] <- vector()
                if (a == (nrow(x.inb))) {
                  inb <- t(apply(x.inb[1:(a - 1), ], MARGIN = 1, FUN = function(x, y) {
                    x - y
                  }, y = x.inb[a, ]))
                  metric.shorteucl$distances[[no.Dsmall]][[a]] <- sqrt(metric.shorteucl$op.dist * rowSums(inb^2))
                } else {
                  inb <- t(apply(rbind(x.inb[1:(a - 1), ], x.inb[(a + 1):nrow(x.inb), ]), MARGIN = 1, FUN = function(x, y) {
                    x - y
                  }, y = x.inb[a, ]))
                  metric.shorteucl$distances[[no.Dsmall]][[a]] <- sqrt(metric.shorteucl$op.dist * rowSums(inb^2))
                }
                if (a == 1) {
                  metric.shorteucl$distances[[no.Dsmall]][[a]] <- vector()
                  inb <- t(apply(x.inb[(a + 1):nrow(x.inb), ], MARGIN = 1, FUN = function(x, y) {
                    x - y
                  }, y = x.inb[a, ]))
                  metric.shorteucl$distances[[no.Dsmall]][[a]] <- sqrt(metric.shorteucl$op.dist * rowSums(inb^2))
                }
            }
        }
    }
    metric.shorteucl
}
############################################# 
metric.mean.test <- function(x = x, tt = tt, deg = deg, ...) {
    metric.mean <- list()
    metric.mean$distances <- list()
    if (length(tt) <= 1) 
        metric.mean$op.dist <- tt else metric.mean$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.mean$distances <- 0
    } else {
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        for (a in 1:1) {
            # (nrow(x))){
            metric.mean$distances[[a]] <- vector()
            if (a == (nrow(x))) {
                inb <- metric.mean$op.dist * rowSums(x[1:(a - 1), ]) - metric.mean$op.dist * sum(x[a, ])
            } else {
                inb <- metric.mean$op.dist * rowSums(rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ])) - metric.mean$op.dist * sum(x[a, ])
            }
            if (a == 1) {
                rm(inb)
                inb <- metric.mean$op.dist * rowSums(x[(a + 1):nrow(x), ]) - metric.mean$op.dist * sum(x[a, ])
            }
            metric.mean$distances[[a]] <- abs(inb)
        }
    }
    metric.mean
}
#############################################
metric.relareas.test <- function(x = x, tt = tt, deg = deg, l.D.small = l.D.small, l.tt2 = l.tt2, l.ii = l.ii, jj = jj, ...) {
    metric.relareas <- list()
    metric.relareas$distances <- list()
    if (length(tt) <= 1) 
        metric.relareas$op.dist <- tt else metric.relareas$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.relareas$distances <- 0
    } else {
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        for (no.ii in 1:length(l.ii)) {
            if (length(l.D.small) < max(l.ii[[no.ii]], jj)) 
                D.small1 <- D.small2 <- l.D.small[[1]] else {
                metric.relareas$distances[[no.ii]] <- list()
                D.small1 <- l.D.small[[l.ii[[no.ii]]]]
                D.small2 <- l.D.small[[jj]]
                minn1 <- maxx1 <- vector()
                minn1 <- which(tt >= min(D.small1[[1]]))
                maxx1 <- which(tt <= max(D.small1[[1]]))
                lengthh1 <- length(maxx1)
                minn2 <- maxx2 <- vector()
                minn2 <- which(tt >= min(D.small2[[1]]))
                maxx2 <- which(tt <= max(D.small2[[1]]))
                lengthh2 <- length(maxx2)
                inb <- x[, minn1[1]:maxx1[lengthh1]]
                inb2 <- x[, minn2[1]:maxx2[lengthh2]]
                for (a in 1:1) {
                  # (nrow(x))){
                  metric.relareas$distances[[no.ii]][[a]] <- vector()
                  if (a == (nrow(x))) {
                    inb3 <- rowSums(inb[1:(a - 1), ])
                    inb4 <- rowSums(inb2[1:(a - 1), ])
                    inb5 <- sum(inb[a, ])
                    inb6 <- sum(inb2[a, ])
                    metric.relareas$distances[[no.ii]][[a]] <- sqrt((abs(inb3/inb4) - abs(inb5/inb6))^2)
                  } else {
                    inb3 <- rowSums(rbind(inb[1:(a - 1), ], inb[(a + 1):nrow(inb), ]))
                    inb4 <- rowSums(rbind(inb2[1:(a - 1), ], inb2[(a + 1):nrow(inb2), ]))
                    inb5 <- sum(inb[a, ])
                    inb6 <- sum(inb2[a, ])
                    metric.relareas$distances[[no.ii]][[a]] <- sqrt((abs(inb3/inb4) - abs(inb5/inb6))^2)
                  }
                  if (a == 1) {
                    metric.relareas$distances[[no.ii]][[a]] <- vector()
                    inb3 <- rowSums(inb[(a + 1):nrow(inb), ])
                    inb4 <- rowSums(inb2[(a + 1):nrow(inb2), ])
                    inb5 <- sum(inb[a, ])
                    inb6 <- sum(inb2[a, ])
                    metric.relareas$distances[[no.ii]][[a]] <- sqrt((abs(inb3/inb4) - abs(inb5/inb6))^2)
                  }
                }
            }
        }
    }
    metric.relareas
}
#############################################
metric.jump.test <- function(x = x, tt = tt, deg = deg, l.D.small = l.D.small, l.tt2 = l.tt2, l.ii = l.ii, l.tvec = l.tvec, ...) {
    metric.jump <- list()
    metric.jump$distances <- list()
    if (is.null(nrow(x))) {
        metric.jump$distances <- 0
    } else {
        for (no.tvec in 1:length(l.tvec)) {
            metric.jump$distances[[no.tvec]] <- list()
            tvec <- l.tvec[[no.tvec]]
            for (a in 1:1) {
                # (nrow(x))){
                metric.jump$distances[[no.tvec]][[a]] <- vector()
                if (a == (nrow(x))) {
                  inb <- x[a, tvec[1]] - x[a, tvec[2]]
                  inb2 <- x[1:(a - 1), tvec[3]] - x[1:(a - 1), tvec[4]]
                  metric.jump$distances[[no.tvec]][[a]] <- abs(sqrt(inb^2) - sqrt(inb2^2))
                } else {
                  inb <- x[a, tvec[1]] - x[a, tvec[2]]
                  inb2 <- rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ])
                  inb3 <- inb2[, tvec[3]] - inb2[, tvec[4]]
                  metric.jump$distances[[no.tvec]][[a]] <- abs(sqrt(inb^2) - sqrt(inb3^2))
                }
                if (a == 1) {
                  metric.jump$distances[[no.tvec]][[a]] <- vector()
                  inb <- x[a, tvec[1]] - x[a, tvec[2]]
                  inb2 <- x[(a + 1):nrow(x), tvec[3]] - x[(a + 1):nrow(x), tvec[4]]
                  metric.jump$distances[[no.tvec]][[a]] <- abs(sqrt(inb^2) - sqrt(inb2^2))
                }
            }
        }
    }
    metric.jump
}
#############################################
metric.max.test <- function(x = x, tt = tt, deg = deg, ...) {
    metric.max <- list()
    metric.max$distances <- list()
    if (length(tt) <= 1) 
        metric.max$op.dist <- tt else metric.max$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.max$distances <- 0
    } else {
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        for (a in 1:1) {
            # (nrow(x))){
            metric.max$distances[[a]] <- vector()
            if (a == (nrow(x))) {
                maxx <- max(x[a, ])
                inb <- rbind(x[1:(a - 1), ])
                inb2 <- apply(inb, MARG = 1, max)
                metric.max$distances[[a]] <- abs(inb2 - maxx)
            } else {
                maxx <- max(x[a, ])
                inb <- rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ])
                inb2 <- apply(inb, MARG = 1, max)
                metric.max$distances[[a]] <- abs(inb2 - maxx)
            }
            if (a == 1) {
                metric.max$distances[[a]] <- vector()
                maxx <- max(x[a, ])
                inb <- rbind(x[(a + 1):nrow(x), ])
                inb2 <- apply(inb, MARG = 1, max)
                metric.max$distances[[a]] <- abs(inb2 - maxx)
            }
        }
    }
    metric.max
}
############################################# 
metric.min.test <- function(x = x, tt = tt, deg = deg, ...) {
    metric.min <- list()
    metric.min$distances <- list()
    if (length(tt) <= 1) 
        metric.min$op.dist <- tt else metric.min$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.min$distances <- 0
    } else {
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        for (a in 1:1) {
            # (nrow(x))){
            metric.min$distances[[a]] <- vector()
            if (a == (nrow(x))) {
                minn <- min(x[a, ])
                inb <- rbind(x[1:(a - 1), ])
                inb3 <- apply(inb, MARG = 1, min)
                metric.min$distances[[a]] <- abs(inb3 - minn)
            } else {
                minn <- min(x[a, ])
                inb <- rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ])
                inb3 <- apply(inb, MARG = 1, min)
                metric.min$distances[[a]] <- abs(inb3 - minn)
            }
            if (a == 1) {
                metric.min$distances[[a]] <- vector()
                minn <- min(x[a, ])
                inb <- rbind(x[(a + 1):nrow(x), ])
                inb3 <- apply(inb, MARG = 1, min)
                metric.min$distances[[a]] <- abs(inb3 - minn)
            }
        }
    }
    metric.min
}
############################################# 
metric.points.test <- function(x = x, tt = tt, deg = deg, point.list = point.list, no.sim = no.sim, ...) {
    metric.points <- list()
    metric.points$distances <- list()
    if (length(tt) <= 1) 
        metric.points$op.dist <- tt else metric.points$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.points$distances <- 0
    } else {
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        pl.short <- point.list[point.list < length(tt)]
        for (a in 1:1) {
            # (nrow(x))){
            inb <- inb2 <- vector()
            metric.points$distances[[a]] <- vector()
            if (a == nrow(x)) {
                inb <- t(apply(x[1:(a - 1), ], MARGIN = 1, FUN = function(x, y) {
                  x - y
                }, y = x[a, ]))
                for (p in 1:length(pl.short)) if (p == 1) 
                  inb2 <- inb[, pl.short[p]] else inb2 <- cbind(inb2, inb[, pl.short[p]])
                metric.points$distances[[a]] <- (1/length(pl.short)) * rowSums(abs(inb2))
            } else {
                inb <- t(apply(rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ]), MARGIN = 1, FUN = function(x, y) {
                  x - y
                }, y = x[a, ]))
                for (p in 1:length(pl.short)) if (p == 1) 
                  inb2 <- inb[, pl.short[p]] else inb2 <- cbind(inb2, inb[, pl.short[p]])
                metric.points$distances[[a]] <- (1/length(pl.short)) * rowSums(abs(inb2))
            }
            if (a == 1) {
                metric.points$distances[[a]] <- vector()
                inb <- t(apply(x[(a + 1):nrow(x), ], MARGIN = 1, FUN = function(x, y) {
                  x - y
                }, y = x[a, ]))
                for (p in 1:length(pl.short)) if (p == 1) 
                  inb2 <- inb[, pl.short[p]] else inb2 <- cbind(inb2, inb[, pl.short[p]])
                metric.points$distances[[a]] <- (1/length(pl.short)) * rowSums(abs(inb2))
            }
        }
    }
    metric.points
}
##############################################
metric.scan.test <- function(x = x, tt = tt, deg = deg, phi.func = phi.func, no.sim = no.sim, ...) {
    metric.scan <- list()
    metric.scan$distances <- list()
    if (length(tt) <= 1) 
        metric.scan$op.dist <- tt else metric.scan$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.scan$distances <- 0
    } else {
        ### define sigma and a vector for tau, called "l.t.l", used in the semi-metric "scan"
        sigmaa <- 10
        l.t.l <- seq(1, length(tt), length = length(tt)/7)  #t_l -Raster von \phi(t)
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        maxx <- max(x)
        l.t.l <- floor(l.t.l)
        phi.inb <- list()
        for (b in 1:length(l.t.l)) {
            metric.scan$distances[[b]] <- list()
            phi.inb[[b]] <- matrix(NA, 1, ncol(x))
            phi.inb[[b]] <- phi.func(tt = tt, t.l = tt[l.t.l[b]], sigmaa = sigmaa)
            if (maxx != 0) 
                phi.inb[[b]] <- phi.inb[[b]] * (maxx/max(phi.inb[[b]]))
            for (a in 1:1) {
            # (nrow(x))){
                metric.scan$distances[[b]][[a]] <- vector()
                if (a == nrow(x)) {
                  inb <- t(apply(x[1:(a - 1), ], MARGIN = 1, FUN = function(x, y) {
                    x - y
                  }, y = x[a, ]))
                  inb2 <- t(apply(inb, MARGIN = 1, FUN = function(x, y) {
                    x * y
                  }, y = phi.inb[[b]]))
                  inb2 <- inb2^2
                  metric.scan$distances[[b]][[a]] <- sqrt(metric.scan$op.dist * rowSums(inb2))
                } else {
                  inb <- t(apply(rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ]), MARGIN = 1, FUN = function(x, y) {
                    x - y
                  }, y = x[a, ]))
                  inb2 <- t(apply(inb, MARGIN = 1, FUN = function(x, y) {
                    x * y
                  }, y = phi.inb[[b]]))
                  inb2 <- inb2^2
                  metric.scan$distances[[b]][[a]] <- sqrt(metric.scan$op.dist * rowSums(inb2))
                }
                if (a == 1) {
                  metric.scan$distances[[b]][[a]] <- vector()
                  inb <- t(apply(x[(a + 1):nrow(x), ], MARGIN = 1, FUN = function(x, y) {
                    x - y
                  }, y = x[a, ]))
                  inb2 <- t(apply(inb, MARGIN = 1, FUN = function(x, y) {
                    x * y
                  }, y = phi.inb[[b]]))
                  inb2 <- inb2^2
                  metric.scan$distances[[b]][[a]] <- sqrt(metric.scan$op.dist * rowSums(inb2))
                }
            }
        }
    }
    metric.scan
}
#############################################
#metrics for centered covariates
#############################################
metric.2.eucl.test <- function(x = x, tt = tt, deg = deg, ...) {
    metric.2.eucl <- list()
    metric.2.eucl$distances <- list()
    if (length(tt) <= 1) 
        metric.2.eucl$op.dist <- tt else metric.2.eucl$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.2.eucl$distances <- 0
    } else {
        inb <- rowMeans(x, na.rm = TRUE)
        inb2 <- x - inb
        x <- inb2
        rm(inb)
        rm(inb2)
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        for (a in 1:1) {
            # (nrow(x))){
            metric.2.eucl$distances[[a]] <- vector()
            if (a == nrow(x)) {
                inb <- t(apply(x[1:(a - 1), ], MARGIN = 1, FUN = function(x, y) {
                  x - y
                }, y = x[a, ]))
                metric.2.eucl$distances[[a]] <- sqrt(metric.2.eucl$op.dist * rowSums(inb^2))
            } else {
                inb <- t(apply(rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ]), MARGIN = 1, FUN = function(x, y) {
                  x - y
                }, y = x[a, ]))
                metric.2.eucl$distances[[a]] <- sqrt(metric.2.eucl$op.dist * rowSums(inb^2))
            }
            if (a == 1) {
                metric.2.eucl$distances[[a]] <- vector()
                inb <- t(apply(x[(a + 1):nrow(x), ], MARGIN = 1, FUN = function(x, y) {
                  x - y
                }, y = x[a, ]))
                metric.2.eucl$distances[[a]] <- sqrt(metric.2.eucl$op.dist * rowSums(inb^2))
            }
        }
    }
    metric.2.eucl
}
############################################# 
metric.2.shorteucl.test <- function(x = x, tt = tt, deg = deg, l.D.small = l.D.small, ...) {
    metric.2.shorteucl <- list()
    metric.2.shorteucl$distances <- list()
    if (length(tt) <= 1) 
        metric.2.shorteucl$op.dist <- tt else metric.2.shorteucl$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.2.shorteucl$distances <- 0
    } else {
        inb <- rowMeans(x, na.rm = TRUE)
        inb2 <- x - inb
        x <- inb2
        rm(inb)
        rm(inb2)
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        for (no.Dsmall in 1:length(l.D.small)) {
            metric.2.shorteucl$distances[[no.Dsmall]] <- list()
            D.small <- l.D.small[[no.Dsmall]]
            minn <- maxx <- x.inb <- vector()
            minn <- which(tt >= min(D.small[[1]]))
            maxx <- which(tt <= max(D.small[[1]]))
            lengthh <- length(maxx)
            x.inb <- x[, minn[1]:maxx[lengthh]]
            for (a in 1:1) {
                # (nrow(x.inb))){
                metric.2.shorteucl$distances[[no.Dsmall]][[a]] <- vector()
                if (a == (nrow(x.inb))) {
                  inb <- t(apply(x.inb[1:(a - 1), ], MARGIN = 1, FUN = function(x, y) {
                    x - y
                  }, y = x.inb[a, ]))
                  metric.2.shorteucl$distances[[no.Dsmall]][[a]] <- sqrt(metric.2.shorteucl$op.dist * rowSums(inb^2))
                } else {
                  inb <- t(apply(rbind(x.inb[1:(a - 1), ], x.inb[(a + 1):nrow(x.inb), ]), MARGIN = 1, FUN = function(x, y) {
                    x - y
                  }, y = x.inb[a, ]))
                  metric.2.shorteucl$distances[[no.Dsmall]][[a]] <- sqrt(metric.2.shorteucl$op.dist * rowSums(inb^2))
                }
                if (a == 1) {
                  metric.2.shorteucl$distances[[no.Dsmall]][[a]] <- vector()
                  inb <- t(apply(x.inb[(a + 1):nrow(x.inb), ], MARGIN = 1, FUN = function(x, y) {
                    x - y
                  }, y = x.inb[a, ]))
                  metric.2.shorteucl$distances[[no.Dsmall]][[a]] <- sqrt(metric.2.shorteucl$op.dist * rowSums(inb^2))
                }
            }
        }
    }
    metric.2.shorteucl
}
#############################################
metric.2.mean.test <- function(x = x, tt = tt, deg = deg, ...) {
    metric.2.mean <- list()
    metric.2.mean$distances <- list()
    if (length(tt) <= 1) 
        metric.2.mean$op.dist <- tt else metric.2.mean$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.2.mean$distances <- 0
    } else {
        inb <- rowMeans(x, na.rm = TRUE)
        inb2 <- x - inb
        x <- inb2
        rm(inb)
        rm(inb2)
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        for (a in 1:1) {
            # (nrow(x))){
            metric.2.mean$distances[[a]] <- vector()
            if (a == (nrow(x))) {
                inb <- metric.2.mean$op.dist * rowSums(x[1:(a - 1), ]) - metric.2.mean$op.dist * sum(x[a, ])
            } else {
                inb <- metric.2.mean$op.dist * rowSums(rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ])) - metric.2.mean$op.dist * sum(x[a, ])
            }
            if (a == 1) {
                rm(inb)
                inb <- metric.2.mean$op.dist * rowSums(x[(a + 1):nrow(x), ]) - metric.2.mean$op.dist * sum(x[a, ])
            }
            metric.2.mean$distances[[a]] <- abs(inb)
        }
    }
    metric.2.mean
}
#############################################
metric.2.relareas.test <- function(x = x, tt = tt, deg = deg, l.D.small = l.D.small, l.tt2 = l.tt2, l.ii = l.ii, jj = jj, ...) {
    metric.2.relareas <- list()
    metric.2.relareas$distances <- list()
    if (length(tt) <= 1) 
        metric.2.relareas$op.dist <- tt else metric.2.relareas$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.2.relareas$distances <- 0
    } else {
        inb <- rowMeans(x, na.rm = TRUE)
        inb2 <- x - inb
        x <- inb2
        rm(inb)
        rm(inb2)
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        for (no.ii in 1:length(l.ii)) {
            if (length(l.D.small) < max(l.ii[[no.ii]], jj)) 
                D.small1 <- D.small2 <- l.D.small[[1]] else {
                metric.2.relareas$distances[[no.ii]] <- list()
                D.small1 <- l.D.small[[l.ii[[no.ii]]]]
                D.small2 <- l.D.small[[jj]]
                minn1 <- maxx1 <- vector()
                minn1 <- which(tt >= min(D.small1[[1]]))
                maxx1 <- which(tt <= max(D.small1[[1]]))
                lengthh1 <- length(maxx1)
                minn2 <- maxx2 <- vector()
                minn2 <- which(tt >= min(D.small2[[1]]))
                maxx2 <- which(tt <= max(D.small2[[1]]))
                lengthh2 <- length(maxx2)
                inb <- x[, minn1[1]:maxx1[lengthh1]]
                inb2 <- x[, minn2[1]:maxx2[lengthh2]]
                for (a in 1:1) {
                  # (nrow(x))){
                  metric.2.relareas$distances[[no.ii]][[a]] <- vector()
                  if (a == (nrow(x))) {
                    inb3 <- rowSums(inb[1:(a - 1), ])
                    inb4 <- rowSums(inb2[1:(a - 1), ])
                    inb5 <- sum(inb[a, ])
                    inb6 <- sum(inb2[a, ])
                    metric.2.relareas$distances[[no.ii]][[a]] <- sqrt((abs(inb3/inb4) - abs(inb5/inb6))^2)
                  } else {
                    inb3 <- rowSums(rbind(inb[1:(a - 1), ], inb[(a + 1):nrow(inb), ]))
                    inb4 <- rowSums(rbind(inb2[1:(a - 1), ], inb2[(a + 1):nrow(inb2), ]))
                    inb5 <- sum(inb[a, ])
                    inb6 <- sum(inb2[a, ])
                    metric.2.relareas$distances[[no.ii]][[a]] <- sqrt((abs(inb3/inb4) - abs(inb5/inb6))^2)
                  }
                  if (a == 1) {
                    metric.2.relareas$distances[[no.ii]][[a]] <- vector()
                    inb3 <- rowSums(inb[(a + 1):nrow(inb), ])
                    inb4 <- rowSums(inb2[(a + 1):nrow(inb2), ])
                    inb5 <- sum(inb[a, ])
                    inb6 <- sum(inb2[a, ])
                    metric.2.relareas$distances[[no.ii]][[a]] <- sqrt((abs(inb3/inb4) - abs(inb5/inb6))^2)
                  }
                }
            }
        }
    }
    metric.2.relareas
}
#############################################
metric.2.max.test <- function(x = x, tt = tt, deg = deg, ...) {
    metric.2.max <- list()
    metric.2.max$distances <- list()
    if (length(tt) <= 1) 
        metric.2.max$op.dist <- tt else metric.2.max$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.2.max$distances <- 0
    } else {
        inb <- rowMeans(x, na.rm = TRUE)
        inb2 <- x - inb
        x <- inb2
        rm(inb)
        rm(inb2)
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        for (a in 1:1) {
            # (nrow(x))){
            metric.2.max$distances[[a]] <- vector()
            if (a == (nrow(x))) {
                maxx <- max(x[a, ])
                inb <- rbind(x[1:(a - 1), ])
                inb2 <- apply(inb, MARG = 1, max)
                metric.2.max$distances[[a]] <- abs(inb2 - maxx)
            } else {
                maxx <- max(x[a, ])
                inb <- rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ])
                inb2 <- apply(inb, MARG = 1, max)
                metric.2.max$distances[[a]] <- abs(inb2 - maxx)
            }
            if (a == 1) {
                metric.2.max$distances[[a]] <- vector()
                maxx <- max(x[a, ])
                inb <- rbind(x[(a + 1):nrow(x), ])
                inb2 <- apply(inb, MARG = 1, max)
                metric.2.max$distances[[a]] <- abs(inb2 - maxx)
            }
        }
    }
    metric.2.max
}
############################################# 
metric.2.min.test <- function(x = x, tt = tt, deg = deg, ...) {
    metric.2.min <- list()
    metric.2.min$distances <- list()
    if (length(tt) <= 1) 
        metric.2.min$op.dist <- tt else metric.2.min$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.2.min$distances <- 0
    } else {
        inb <- rowMeans(x, na.rm = TRUE)
        inb2 <- x - inb
        x <- inb2
        rm(inb)
        rm(inb2)
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        for (a in 1:1) {
            # (nrow(x))){
            metric.2.min$distances[[a]] <- vector()
            if (a == (nrow(x))) {
                minn <- min(x[a, ])
                inb <- rbind(x[1:(a - 1), ])
                inb3 <- apply(inb, MARG = 1, min)
                metric.2.min$distances[[a]] <- abs(inb3 - minn)
            } else {
                minn <- min(x[a, ])
                inb <- rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ])
                inb3 <- apply(inb, MARG = 1, min)
                metric.2.min$distances[[a]] <- abs(inb3 - minn)
            }
            if (a == 1) {
                metric.2.min$distances[[a]] <- vector()
                minn <- min(x[a, ])
                inb <- rbind(x[(a + 1):nrow(x), ])
                inb3 <- apply(inb, MARG = 1, min)
                metric.2.min$distances[[a]] <- abs(inb3 - minn)
            }
        }
    }
    metric.2.min
}
############################################# 
metric.2.points.test <- function(x = x, tt = tt, deg = deg, point.list = point.list, no.sim = no.sim, ...) {
    metric.2.points <- list()
    metric.2.points$distances <- list()
    if (length(tt) <= 1) 
        metric.2.points$op.dist <- tt else metric.2.points$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.2.points$distances <- 0
    } else {
        inb <- rowMeans(x, na.rm = TRUE)
        inb2 <- x - inb
        x <- inb2
        rm(inb)
        rm(inb2)
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        pl.short <- point.list[point.list < length(tt)]
        for (a in 1:1) {
            # (nrow(x))){
            inb <- inb2 <- vector()
            metric.2.points$distances[[a]] <- vector()
            if (a == nrow(x)) {
                inb <- t(apply(x[1:(a - 1), ], MARGIN = 1, FUN = function(x, y) {
                  x - y
                }, y = x[a, ]))
                for (p in 1:length(pl.short)) if (p == 1) 
                  inb2 <- inb[, pl.short[p]] else inb2 <- cbind(inb2, inb[, pl.short[p]])
                metric.2.points$distances[[a]] <- (1/length(pl.short)) * rowSums(abs(inb2))
            } else {
                inb <- t(apply(rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ]), MARGIN = 1, FUN = function(x, y) {
                  x - y
                }, y = x[a, ]))
                for (p in 1:length(pl.short)) if (p == 1) 
                  inb2 <- inb[, pl.short[p]] else inb2 <- cbind(inb2, inb[, pl.short[p]])
                metric.2.points$distances[[a]] <- (1/length(pl.short)) * rowSums(abs(inb2))
            }
            if (a == 1) {
                metric.2.points$distances[[a]] <- vector()
                inb <- t(apply(x[(a + 1):nrow(x), ], MARGIN = 1, FUN = function(x, y) {
                  x - y
                }, y = x[a, ]))
                for (p in 1:length(pl.short)) if (p == 1) 
                  inb2 <- inb[, pl.short[p]] else inb2 <- cbind(inb2, inb[, pl.short[p]])
                metric.2.points$distances[[a]] <- (1/length(pl.short)) * rowSums(abs(inb2))
            }
        }
    }
    metric.2.points
}
##############################################
metric.2.scan.test <- function(x = x, tt = tt, deg = deg, phi.func = phi.func, no.sim = no.sim, ...) {
    metric.2.scan <- list()
    metric.2.scan$distances <- list()
    if (length(tt) <= 1) 
        metric.2.scan$op.dist <- tt else metric.2.scan$op.dist <- abs(tt[1] - tt[2])
    if (is.null(nrow(x))) {
        metric.2.scan$distances <- 0
    } else {
        inb <- rowMeans(x, na.rm = TRUE)
        inb2 <- x - inb
        x <- inb2
        rm(inb)
        rm(inb2)
        ### define sigma and a vector for tau, called "l.t.l", used in the semi-metric "scan"
        sigmaa <- 10
        l.t.l <- seq(1, length(tt), length = length(tt)/7)  #t_l -Raster von \phi(t)
        if (deg >= 1) {
            for (a in 1:deg) {
                if (a == 1) {
                  inb <- matrix(NA, nrow(x), ncol(x) - 1)
                  t.inb <- vector()
                  for (b in 1:(ncol(x) - 1)) if (length(tt) <= 1) 
                    inb[, b] <- (x[, b + 1] - x[, b]) else inb[, b] <- (x[, b + 1] - x[, b])/(tt[b + 1] - tt[b])
                  t.inb <- tt[1:(length(tt) - 1)]
                } else {
                  inb <- matrix(NA, nrow(inb2), ncol(inb2) - 1)
                  for (b in 1:(ncol(inb2) - 1)) if (length(t.inb) <= 1) 
                    inb[, b] <- (inb2[, b + 1] - inb2[, b]) else inb[, b] <- (inb2[, b + 1] - inb2[, b])/(t.inb[b + 1] - t.inb[b])
                  rm(inb2)
                  inb2 <- length(t.inb) - 1
                  t.inb <- t.inb[1:inb2]
                  rm(inb2)
                }
                inb2 <- matrix(NA, nrow(inb), ncol(inb))
                inb2 <- inb
                rm(inb)
            }
            rm(x)
            rm(tt)
            tt <- t.inb
            rm(t.inb)
            x <- inb2
            rm(inb2)
        }
        maxx <- max(x)
        l.t.l <- floor(l.t.l)
        phi.inb <- list()
        for (b in 1:length(l.t.l)) {
            metric.2.scan$distances[[b]] <- list()
            phi.inb[[b]] <- matrix(NA, 1, ncol(x))
            phi.inb[[b]] <- phi.func(tt = tt, t.l = tt[l.t.l[b]], sigmaa = sigmaa)
            if (maxx != 0) 
                phi.inb[[b]] <- phi.inb[[b]] * (maxx/max(phi.inb[[b]]))
            for (a in 1:1) {
            # (nrow(x))){
                metric.2.scan$distances[[b]][[a]] <- vector()
                if (a == nrow(x)) {
                  inb <- t(apply(x[1:(a - 1), ], MARGIN = 1, FUN = function(x, y) {
                    x - y
                  }, y = x[a, ]))
                  inb2 <- t(apply(inb, MARGIN = 1, FUN = function(x, y) {
                    x * y
                  }, y = phi.inb[[b]]))
                  inb2 <- inb2^2
                  metric.2.scan$distances[[b]][[a]] <- sqrt(metric.2.scan$op.dist * rowSums(inb2))
                } else {
                  inb <- t(apply(rbind(x[1:(a - 1), ], x[(a + 1):nrow(x), ]), MARGIN = 1, FUN = function(x, y) {
                    x - y
                  }, y = x[a, ]))
                  inb2 <- t(apply(inb, MARGIN = 1, FUN = function(x, y) {
                    x * y
                  }, y = phi.inb[[b]]))
                  inb2 <- inb2^2
                  metric.2.scan$distances[[b]][[a]] <- sqrt(metric.2.scan$op.dist * rowSums(inb2))
                }
                if (a == 1) {
                  metric.2.scan$distances[[b]][[a]] <- vector()
                  inb <- t(apply(x[(a + 1):nrow(x), ], MARGIN = 1, FUN = function(x, y) {
                    x - y
                  }, y = x[a, ]))
                  inb2 <- t(apply(inb, MARGIN = 1, FUN = function(x, y) {
                    x * y
                  }, y = phi.inb[[b]]))
                  inb2 <- inb2^2
                  metric.2.scan$distances[[b]][[a]] <- sqrt(metric.2.scan$op.dist * rowSums(inb2))
                }
            }
        }
    }
    metric.2.scan
}



 
