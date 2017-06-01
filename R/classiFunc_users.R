
require(installr)

# The first two functions might take a good deal of time to run (depending on the date range)
RStudio_CRAN_data_folder <- download_RStudio_CRAN_data(START = '2017-05-29', END = '2017-06-01') # around the time R 3.0.0 was released
my_RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_data_folder)

# barplots: (more functions can easily be added in the future)
barplot_package_users_per_day("classiFunc", my_RStudio_CRAN_data, remove_dups = TRUE)
