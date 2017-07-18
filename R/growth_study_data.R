# This file reads in the berkeley growth study data
# and generates various file formats from it

library("fda")
library("reshape2")
library("plyr")

## Clean up data using the fda package
# Smooth the girls
girlGrowth.fd <- as.fd(with(growth, smooth.basisPar(argvals=age, y=hgtf,
                                                    lambda=0.1)))
girlGrowth.fd_der <- deriv.fd(girlGrowth.fd, 1)
girlGrowth.fd_der2 <- deriv.fd(girlGrowth.fd, 2)
girlGrowth.fd_der3 <- deriv.fd(girlGrowth.fd, 3)

# Get the values on an even grid
girlGrowth <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                      fdobj = girlGrowth.fd)
girlGrowth_der <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                          fdobj = girlGrowth.fd_der)
girlGrowth_der2 <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                           fdobj = girlGrowth.fd_der2)
girlGrowth_der3 <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                           fdobj = girlGrowth.fd_der3)

# and smooth the boys
boyGrowth.fd <- as.fd(with(growth, smooth.basisPar(argvals=age, y=hgtm,
                                                   lambda=0.1)))
boyGrowth.fd_der <- deriv.fd(boyGrowth.fd, 1)
boyGrowth.fd_der2 <- deriv.fd(boyGrowth.fd, 2)
boyGrowth.fd_der3 <- deriv.fd(boyGrowth.fd, 3)

# Get the values on an even grid
boyGrowth <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                     fdobj = boyGrowth.fd)
boyGrowth_der <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                         fdobj = boyGrowth.fd_der)
boyGrowth_der2 <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                          fdobj = boyGrowth.fd_der2)
boyGrowth_der3 <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                          fdobj = boyGrowth.fd_der3)

## Transform the boys
# boys <- melt(growth$hgtm, value.name = "height")
boys <- melt(boyGrowth, value.name = "height")
boys$sex = "male"
colnames(boys) <- c("time", "ID", "height", "sex")
boys$deriv = 0

boys_der <- melt(boyGrowth_der, value.name = "height")
boys_der$sex = "male"
colnames(boys_der) <- c("time", "ID", "height", "sex")
boys_der$deriv = 1

boys_der2 <- melt(boyGrowth_der2, value.name = "height")
boys_der2$sex = "male"
colnames(boys_der2) <- c("time", "ID", "height", "sex")
boys_der2$deriv = 2

boys_der3 <- melt(boyGrowth_der3, value.name = "height")
boys_der3$sex = "male"
colnames(boys_der3) <- c("time", "ID", "height", "sex")
boys_der3$deriv = 3

## Transform the girls
# girls <- melt(growth$hgtf, value.name = "height")
girls <- melt(girlGrowth, value.name = "height")
girls$sex = "female"
colnames(girls) <- c("time", "ID", "height", "sex")
girls$deriv = 0

girls_der <- melt(girlGrowth_der, value.name = "height")
girls_der$sex = "female"
colnames(girls_der) <- c("time", "ID", "height", "sex")
girls_der$deriv = 1

girls_der2 <- melt(girlGrowth_der2, value.name = "height")
girls_der2$sex = "female"
colnames(girls_der2) <- c("time", "ID", "height", "sex")
girls_der2$deriv = 2

girls_der3 <- melt(girlGrowth_der3, value.name = "height")
girls_der3$sex = "female"
colnames(girls_der3) <- c("time", "ID", "height", "sex")
girls_der3$deriv = 3

# Bind boys and girls together
growth_long <- rbind(boys, boys_der, boys_der2, boys_der3,
                     girls, girls_der, girls_der2, girls_der2, girls_der3)
growth_long$time <- growth_long$time / 10

# Cast the data frame into the wide format
# Careful here with the fixed
growth_wide <- dcast(ID + sex + deriv ~ time, value.var = "height", 
                     fun.aggregate = mean, data = growth_long)
colnames(growth_wide)[4:ncol(growth_wide)] <- paste0("t", colnames(growth_wide)[4:ncol(growth_wide)])

growth_wide = growth_wide[growth_wide$deriv == 0, -3]
