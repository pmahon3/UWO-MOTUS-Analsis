library(HDInterval)
library(dplyr)

for (i in 1:NPOPS){
  samplesFile = paste("./results/samples/population", toString(i), ".rds", sep="")
  parametersFile = paste("./results/data/simulatedParams", toString(i), ".rds", sep="")
  
  samples = data.frame(readRDS(samplesFile))
  parameters = readRDS(parametersFile)
  
  plot(seq(1,length(samples$muDelta.prime)), samples$muDelta.prime, type = 'l')
}