# Path to this R-File
data_path = "Daten/TSC Problems"

# This file reads in the data sets from the UCR TSC archive
data_names = list.dirs(data_path, full.names = FALSE, recursive = FALSE)
data_names = data_names[!data_names %in% c("", "Data Descriptions", 
                                           "ElectricDeviceOn", "ECGMeditation", 
                                          "EpilepsyX", "EthanolLevel", "HeartbeatBIDMC", "Yoga",
                                          "WormsTwoClass")]

# absolute paths to the data sets
data_paths = paste0(data_path, "/", data_names, "/", data_names, ".arff")
names(data_paths) = data_names

# read in the data sets
library("foreign")

# data_list = lapply(data_paths, read.arff)
data_list = list()

for(i in 1:length(data_paths)) {
  data_list[[i]] = read.arff(data_paths[i])
}
names(data_list) = data_names

# # create the data sets for classiFunc package
# Phoneme = read.arff(data_paths[65])
# Phoneme = Phoneme[1:100, c(TRUE, rep(FALSE, 15))]
# devtools::use_data(Phoneme)
# ArrowHead = read.arff(data_paths[4])
# ArrowHead = ArrowHead[1:100, c(FALSE, FALSE, TRUE)]
# devtools::use_data(ArrowHead)
# Yoga = read.arff(data_paths[96])
# dim(Yoga)
# save(Yoga, file = "data/Yoga.RData")
# devtools::use_data(Yoga)

# Create artificial name colum
for(i in 1:length(data_list)) {
  data_list[[i]]["name"] = factor(names(data_list)[i])
}

# create FDA tasks
library("mlr")
# tsks = lapply(data_list, function(dat) {
#   makeFDAClassifTask(data = dat[,1:(ncol(dat) - 1)],
#                      id = toString(dat$name[1]),
#                      fd.features = list(ff = 1:(ncol(dat) - 2)),
#                      target = "target")
# })

tsks = list()
for(i in 1:length(data_list)) {
  dat = data_list[[i]]
  tsks[[i]] = makeFDAClassifTask(data = dat[,1:(ncol(dat) - 1)],
                       id = toString(dat$name[1]),
                       fd.features = list(ff = 1:(ncol(dat) - 2)),
                       target = "target")
}

# select feasible tasks
name = nobs = obslen = rep(NA, length(tsks))
for(i in 1:length(tsks)) {
  name[i] = getTaskId(tsks[[i]])
  nobs[i] = getTaskSize(tsks[[i]])
  obslen[i] = getTaskNFeats(tsks[[i]])
}
df = data.frame(name, nobs, obslen, nobs*obslen)
hist(df$nobs...obslen, breaks = 100)
summary(df$nobs...obslen)
quantile(df$nobs...obslen, 0.4)
df_red = df[df$nobs...obslen <= 100000,]
nrow(df_red)

# 
tsks = tsks[df$nobs...obslen <= 100000]


