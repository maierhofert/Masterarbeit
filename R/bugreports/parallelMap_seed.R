
library("parallelMap")

# benchmark in parallel
# setting a seed does not seem to work
# set.seed(1234, "L'Ecuyer-CMRG")

parallelMap::parallelStartSocket(cpus = 4)

# you havce to set a seed here like this
# otherwise it won´t work
parallel::clusterSetRNGStream(iseed = 42)

warning(parallelMap::parallelLapply(1:10, rnorm)[[10]])
