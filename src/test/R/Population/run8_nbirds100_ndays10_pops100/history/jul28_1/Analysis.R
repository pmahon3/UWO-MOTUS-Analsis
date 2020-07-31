library(stringr)
library(HDInterval)
source("Inputs.R")
print(getwd())
file.names <- dir("./results/samples/")
print(file.names)

delta1Cov = 0
delta2Cov = 0
muDelta1Cov = 0
muDelta2Cov = 0
deltaPrimeCov = 0