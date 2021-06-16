## Load packages
# library(foreach) 
#library(doParallel)
library(nimble)
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(HDInterval)
# library(extraDistr)

## MCMC parameters
thin <- 10
niter <- 20000

## Load inputs (true parameters and constants)
source("Inputs.R")

## Source simulation code
source("Simulation.R")

## Simulate data
data <- dataSimulation(-1, constants, trueParams, FALSE)

## Source model code
source("Model.R")

## Initialized nimble model
model <- nimbleModel(code = modelCode,
                     name = "model",
                     constants = constants,
                     data = data,
                     calculate = FALSE)

## Configure model
configured <- configureMCMC(model)
#configured$resetMonitors() #Why is this necessary?
configured$setThin(thin)  

## Set samplers (Is this necessary?)
## configured$setSamplers(c("muDelta.prime",
##                          "sigmaDelta.prime",
##                          "delta.prime",
##                          "muMuDelta",
##                          "muDelta",
##                          "delta",
##                          "sigmaDelta",
##                          "muMuY",
##                          "sigmaMuY",
##                          "muY",
##                          "muSdY",
##                          "sigmaSdY",
##                          "sigmaY"))

## Add monitors
configured$addMonitors(c("muDelta.prime",
                         "sigmaDelta.prime",
                         "delta.prime",
                         "muMuDelta",
                         "muDelta",
                         "delta",
                         "sigmaDelta",
                         "muMuY",
                         "sigmaMuY",
                         "muY",
                         "muSdY",
                         "sigmaSdY",
                         "sigmaY"))

## Build model
built <- buildMCMC(configured)

## Compile model
compiled <- compileNimble( model, built, showCompilerOutput = TRUE )

## Run model
system.time(compiled$built$run(niter = niter))
  
## Extract samples
samples <- as.matrix(compiled$built$mvSamples)

samplesdf <- samples %>%
    as_tibble() %>%
    rowid_to_column(var = "Iteration") %>%
    gather(key = "Parameter", value = "Value", -Iteration)

## Examine results

## 1) muDelta.prime and sigmaDelta.prime
plot1 <- samplesdf %>%
    filter(Parameter == "muDelta.prime") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$muDelta.prime, colour = "red", lty = 2)

plot2 <- samplesdf %>%
    filter(Parameter == "sigmaDelta.prime") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$sigmaDelta.prime, colour = "red", lty = 2)

plot1 / plot2

## 2) muMuDelta
plot1 <- samplesdf %>%
    filter(Parameter == "muMuDelta[1]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$muMuDelta[1], colour = "red", lty = 2)

plot2 <- samplesdf %>%
    filter(Parameter == "muMuDelta[2]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$muMuDelta[2], colour = "red", lty = 2)

plot1 / plot2

## 3) sigmaMuDelta
plot1 <- samplesdf %>%
    filter(Parameter == "sigmaMuDelta[1]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$sigmaMuDelta[1], colour = "red", lty = 2)

plot2 <- samplesdf %>%
    filter(Parameter == "sigmaMuDelta[2]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$sigmaMuDelta[2], colour = "red", lty = 2)

plot1 / plot2

## 4) sigmaDelta
plot1 <- samplesdf %>%
    filter(Parameter == "sigmaDelta[1]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$sigmaDelta[1], colour = "red", lty = 2)

plot2 <- samplesdf %>%
    filter(Parameter == "sigmaDelta[2]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$sigmaDelta[2], colour = "red", lty = 2)

plot1 / plot2

## 4) muMuY
plot1 <- samplesdf %>%
    filter(Parameter == "muMuY[1]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$muMuY[1], colour = "red", lty = 2)

plot2 <- samplesdf %>%
    filter(Parameter == "muMuY[2]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$muMuY[2], colour = "red", lty = 2)

plot3 <- samplesdf %>%
    filter(Parameter == "muMuY[3]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$muMuY[3], colour = "red", lty = 2)

plot1 / plot2 / plot3


## 5) sigmaMuY
plot1 <- samplesdf %>%
    filter(Parameter == "sigmaMuY[1]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$sdMuY[1], colour = "red", lty = 2)

plot2 <- samplesdf %>%
    filter(Parameter == "sigmaMuY[2]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$sdMuY[2], colour = "red", lty = 2)

plot3 <- samplesdf %>%
    filter(Parameter == "sigmaMuY[3]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$sdMuY[3], colour = "red", lty = 2)

plot1 / plot2 / plot3

## 6) muSdY
plot1 <- samplesdf %>%
    filter(Parameter == "muSdY[1]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$muSdY[1], colour = "red", lty = 2)

plot2 <- samplesdf %>%
    filter(Parameter == "muSdY[2]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$muSdY[2], colour = "red", lty = 2)

plot3 <- samplesdf %>%
    filter(Parameter == "muSdY[3]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$muSdY[3], colour = "red", lty = 2)

plot1 / plot2 / plot3

## 6) sdSdY
plot1 <- samplesdf %>%
    filter(Parameter == "sigmaSdY[1]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$sdSdY[1], colour = "red", lty = 2)

plot2 <- samplesdf %>%
    filter(Parameter == "sigmaSdY[2]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$sdSdY[2], colour = "red", lty = 2)

plot3 <- samplesdf %>%
    filter(Parameter == "sigmaSdY[3]") %>%
    ggplot(aes(x = Iteration, y = Value)) +
    geom_line() +
    geom_hline(yintercept = trueParams$sdSdY[3], colour = "red", lty = 2)

plot1 / plot2 / plot3

## 7) Delta.prime
samplesdf %>%
    filter(grepl("delta.prime\\[",Parameter)) %>%
    group_by(Parameter) %>%
    summarize(
        Mean = mean(Value),
        Lower = quantile(Value, .025),
        Upper = quantile(Value, .975))

## 8) delta[1]
samplesdf %>%
    filter(grepl("delta\\[([0-9]), 15, 2\\]",Parameter)) %>%
    group_by(Parameter) %>%
    summarize(
        Mean = mean(Value),
        Lower = quantile(Value, .025),
        Upper = quantile(Value, .975))

