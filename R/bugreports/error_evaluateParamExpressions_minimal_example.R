# minimal example for evaluateParamExpressions
k = 1
par.set = makeParamSet(
  makeIntegerParam(id = "int", default = expression(k)),
  keys = "k"
)

# this works
evaluateParamExpressions(obj = par.set, dict = as.list(environment()))

# this does not work
evaluateParamExpressions(obj = par.set, dict = environment())
