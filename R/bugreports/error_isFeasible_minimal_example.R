# minimal example for isFeasible
par.set = makeParamSet(
  makeIntegerParam(id = "int", default = 1L),
  makeLogicalParam(id = "logic", default = TRUE)
)

# this works
par.list = list(int = 2L, logic = TRUE)
isFeasible(par.set, par.list)

# this does not work
par.list2 = list(logic = TRUE, int = 2L)
isFeasible(par.set, par.list2)
