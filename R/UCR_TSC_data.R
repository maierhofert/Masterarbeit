# Path to this R-File
data_path = "Daten/TSC Problems"

# This file reads in the data sets from the UCR TSC archive
data_names = list.dirs(data_path, full.names = FALSE)
data_names = data_names[data_names != ""]

# absolute paths to the data sets
data_paths = paste0(data_path, "/", data_names, "/", data_names, ".arff")
names(data_paths) = data_names


# subset of data sets to be used
test <- c(5:6)

# take out car if CV folds = 100 because it does not have enough observations
# take ightCurves because too big
# FordAB are quite large as well
sensor = c("Car", 
           "Earthquakes",
           # "FordA", "FordB", 
           "InsectWingbeatSound", "ItalyPowerDemand",
           "Lightning2", "Lightning7", "MoteStrain", 
           # "Phoneme", "Plane",
           "SonyAIBORobotSurface1", "SonyAIBORobotSurface2",
           # "StarLightCurves", 
           "Trace") #, 
# "Wafer")
spectro = c("Beef", "Coffee", "Ham", "Meat", 
            # "OliveOil", 
            "Strawberry", "Wine")
images = c("Adiac", "ArrowHead", "BeetleFly", "BirdChicken",
           "DiatomSizeReduction", 
           "DistalPhalanxOutlineAgeGroup",
           "DistalPhalanxOutlineCorrect", "DistalPhalanxTW",
           "FaceAll", "FaceFour", "FacesUCR", 
           # "FiftyWords",
           "Fish", 
           # "HandOutlines", 
           "Herring", "MedicalImages",
           "MiddlePhalanxOutlineAgeGroup", "MiddlePhalanxOutlineCorrect", "MiddlePhalanxTW",
           "OSULeaf",
           "PhalangesOutlinesCorrect",
           "ProximalPhalanxOutlineAgeGroup",
           "ProximalPhalanxOutlineCorrect",
           "ProximalPhalanxTW",
           "ShapesAll", "SwedishLeaf", "Symbols", 
           "WordSynonyms", "Yoga")
# read in the data sets
library("foreign")
data_list = lapply(data_paths[c(images, sensor, spectro)], read.arff)

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
tsks = lapply(data_list, function(dat) {
  makeFDAClassifTask(data = dat[,1:(ncol(dat) - 1)],
                     id = toString(dat$name[1]),
                     fd.features = list(ff = 1:(ncol(dat) - 2)),
                     target = "target")
})
