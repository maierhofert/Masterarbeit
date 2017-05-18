# this file looks into the results of the benchmark analysis
bmr = readRDS("Benchmark_results/bmr.RDS")

# # data frame containing results
# getBMRAggrPerformances(bmr, as.df = TRUE)

plotBMRSummary(bmr, trafo = "rank", jitter = 0, pretty.names = FALSE)
plotBMRRanksAsBarChart(bmr, pretty.names = FALSE)

#################################################################
# visualize benchmark results
plotBMRBoxplots(bmr, measure = timeboth, pretty.names = FALSE)
plotBMRBoxplots(bmr, measure = multiclass.brier, pretty.names = FALSE)

# Friedman Test
friedmanTestBMR(bmr, measure = multiclass.brier)

# Nemenyi Post-Hoc Test
friedmanPostHocTestBMR(bmr, measure = multiclass.brier)

# critical difference diagram
g = generateCritDifferencesData(bmr, measure = multiclass.brier,
                                p.value = 0.1, test = "nemenyi")
plotCritDifferences(g, pretty.names = FALSE) #+ coord_cartesian(xlim = c(-1,5), ylim = c(0,2))
