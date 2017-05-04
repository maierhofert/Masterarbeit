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

# Get the values on an even grid
girlGrowth <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                      fdobj = girlGrowth.fd)
girlGrowth_der <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                          fdobj = girlGrowth.fd_der)
girlGrowth_der2 <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                           fdobj = girlGrowth.fd_der2)

# and smooth the boys
boyGrowth.fd <- as.fd(with(growth, smooth.basisPar(argvals=age, y=hgtm,
                                                   lambda=0.1)))
boyGrowth.fd_der <- deriv.fd(boyGrowth.fd, 1)
boyGrowth.fd_der2 <- deriv.fd(boyGrowth.fd, 2)


# Get the values on an even grid
boyGrowth <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                     fdobj = boyGrowth.fd)
boyGrowth_der <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                         fdobj = boyGrowth.fd_der)
boyGrowth_der2 <- eval.fd(evalarg = seq(1, 18, by = 0.1),
                          fdobj = boyGrowth.fd_der2)


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


# Bind boys and girls together
growth_long <- rbind(boys, boys_der, boys_der2,
                     girls, girls_der, girls_der2)
growth_long$time <- growth_long$time / 10
# growth_long$sex <- factor(dat$sex)


# Cast the data frame into the wide format
# Careful here with the fixed
growth_wide <- dcast(ID + sex + deriv ~ time, value.var = "height", data = growth_long)
colnames(growth_wide)[4:ncol(growth_wide)] <- paste0("t", colnames(growth_wide)[4:ncol(growth_wide)])


# Transform this into the notation of Fuchs
x <- list(as.matrix(growth_wide[growth_wide$deriv == 0, 4:ncol(growth_wide)]))
xul <- x[[1]]
y <- rep(0, nrow(growth_wide[growth_wide$deriv == 0,]))
y[growth_wide[growth_wide$deriv == 0,2] == "female"] <- 1

# tt <- growth_long$time[1:31]
tt <- unique(growth_long$time)

# 10 times repeated 10 fold cross validation
nfolds = 10
repetitions = 10

# Set up the CV data sets
x.inb.cal <- list()
x.inb.val <- list()

y.inb.cal <- list()
y.inb.val <- list()

set.seed("29112016")

for(times in 1:repetitions) {
  # Initialize list for every repetition
  x.inb.cal[[times]] <- list()
  x.inb.val[[times]] <- list()
  y.inb.cal[[times]] <- list()
  y.inb.val[[times]] <- list()
  
  # Split randomly into the nfold groups
  group <- sample(rep(1:nfolds, length.out = nrow(xul)), 
                  size = nrow(xul), replace = FALSE)
  
  # get the according samples from x or y
  for(i in 1:nfolds) {
    x.inb.cal[[times]][[i]] <- list(xul[group != i,])
    x.inb.val[[times]][[i]] <- list(xul[group == i,])
    y.inb.cal[[times]][[i]] <- y[group != i]
    y.inb.val[[times]][[i]] <- y[group == i]
  }
}

### Plotting the growth study data

library("ggplot2")

# create new variables for plotting

# # derivatives
# growth_long <- ddply(growth_long, .(ID), function(dat) {
#   dat <- mutate(dat, height_deriv = c(diff(dat$height) / diff(dat$time), 0))
#   dat <- mutate(dat, height_deriv2 = c(diff(dat$height_deriv)[-31] / 
#                                          diff(dat$time)[-31], 0))
#   mutate(dat, height_deriv3 = c(diff(dat$height_deriv2)[-30:-31] / 
#                                   diff(dat$time)[-30:-31], 0, 0))
# })



# prettier German sex
growth_long$Geschlecht = "männl."
growth_long$Geschlecht[growth_long$sex == "female"] = "weibl."

growth_long$Geschlecht2 = "männlich"
growth_long$Geschlecht2[growth_long$sex == "female"] = "weiblich"





