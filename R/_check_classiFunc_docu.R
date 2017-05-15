# run this file to check the documentation of the classiFunc package
# note: you need rtools to use the devtools package
# install.packages("devtools")
library("devtools")

# install the classiFunc package directly from github
devtools::install_github("maierhofert/classiFunc", build_vignettes = TRUE)
library("classiFunc")

# open the vignette
browseVignettes(package = "classiFunc")
# click on HTML to see the actual vignette
# please read the vignette, see whether it is understandable
# does it contains all the information you need to get started with the package?
# check that all the code chunks runs

### can you please check the documentation to the amin functions of the package?
# where would you wish for (more/better) examples?
# Are all the arguments named in a meaningful way? Is it clear what they do?

# package docu
?classiFunc

# k nearest neigbor estimator
?classiKnn

# kernel estimator
?classiKernel

# distance function
?computeDistMat

# data contained in the package
?ArrowHead
?Phoneme

# not so important functions, only have a glance at the docu
?metric.choices
?ker.choices
?fdataTransform
