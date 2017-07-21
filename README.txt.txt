This file described the digital supplement. The folder Masterarbeit contains an R-Project
which is tracked on Github "maierhofert/Masterarbeit".
Structure of Files:
Masterarbeit.Rproj: master file for this project. Open this in RStudio to run all R Code.
Benchmark Experiments: Results of the benchmark experiments as .RDS, saved with date and 
of the source file
Daten: -Simulated Data: - random_splines: random splines data
			- random_srvastava: warped bimodal data
			- random_trigonometric: warped trigonometric data
			- warped_greven: contains warped splines data
	- TSC Probles: all data from the UCR Times Series Classification Repository
Grafiken: - benchmark: all figures for benchmark experiments
	  - Berkley_growth_study: example figures for the berkley growth study data
	  - beetlefly: plot of BeetleFly data set of UCR TSCR
  	  - Bsplines_basis3: plot of basis functions for B-splines
	  - simu_data...: figures of simulated data
	  - classiFunc_downloads: no of downloads of classiFucn package
	  - mlr_downloads: -"-
	  - dtw_figure: figure for DTW from Wikipedia
	  - tree_plot..., rf_plot: plots fro random forest and classification tree
	  - weight_plot_p...: plots showing weights in nn ensemble
	  - weightplot_..._ensemble: weights in ensemble models
Literatur: collection of Literature for the thesis
R: RCode -bugreports: collection of oddities and bugs found in R
	- benchmark_server_...: master file to run ... benchmark on server
	- benchmark...: code containing the actual benchmark for ...
	- benchmark_results...: plot benchmark results for ...
	- create...learners: create ... learners in mlr for all benchmark experiments
	- growth_study_data: extract and preprocess growth study data
	- plot_...: create plots for ...
	- simulate_data: create ... simulated data
	- UCR_TSC_data: preprocess UCR TSCR data for benchmark experiments
_benchmark_server_... .Rout: server output for ... benchmark experiment
Masterarbeit.Rproj: master file for this project. Open this in RStudio to run all R Code.
..._ensemble.RDS: trained ... ensemble model.
