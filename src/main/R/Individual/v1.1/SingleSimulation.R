## Load packages
library(nimble)
library(tidyverse)
library(patchwork)
library(coda)
library(ggmcmc)

## Simulation parameters
NBIRDS = 100

## Load input files
source("Inputs.R")
source("DataSimulation.R")
source("GenerateInits.R")
source("Hyperpriors.R")
source("Model.R")

## Set seed
set.seed(7777)

## Simulate data
dataAndConstants <- dataSimulation(x, constants, trueParams)

deltas <- dataAndConstants$data$ds

## Convert data to a tibble
mydat <- lapply(1:10, function(day){
  tibble(time = dataAndConstants$constants$t[day,],
         y = dataAndConstants$data$y[day,]) %>%
    add_column(Day = day, .before = 1)
}) %>%
  bind_rows() %>%
  mutate(CP1 = deltas[Day,1],
         CP2 = deltas[Day,2] + (Day == dataAndConstants$constants$nDays) * trueParams$delta,
         Period = 1 + (time > CP1) + (time > CP2))

## Explore data
day <- 10

plotAM <- mydat %>%
  filter(Day == day, time < 12) %>%
  ggplot(aes(x = time, y = y)) +
  geom_line() +
  geom_vline(xintercept = deltas[day,1],colour = "red") +
  geom_hline(yintercept = trueParams$muY[day,1:2],lty=2, colour = "blue")

plotPM <- mydat %>%
  filter(Day == day, time >= 12) %>%
  ggplot(aes(x = time, y = y)) +
  geom_line() +
  geom_vline(xintercept = deltas[day,2] + ifelse(day == trueParams$nDays, trueParams$delta, 0) ,colour = "red") +
  geom_hline(yintercept = trueParams$muY[day,2:3],lty=2, colour = "blue")


plotAM / plotPM

## Generate initial values
inits <- genInits(dataAndConstants$data, dataAndConstants$constants)

## Add parameters of hyperpriors to the constants
constants <- hyperpriors(dataAndConstants[["constants"]],
                                          median_xiDelta = .1,
                                          median_xi_y = 10,
                                          median_xi = 10)
  
## Configure nimble model
params <- c("etaDelta", "xiDelta", "tauDelta", "delta","delta.prime",
            "mu", "xi", "tau",
            "mu_y", "xi_y","tau_y")

model <- nimbleModel( code = modelCode,
                     name = "model",
                     constants = constants,
                     data = dataAndConstants[["data"]],
                     inits = inits,
                     calculate = FALSE)

configured <- configureMCMC(model)
configured$resetMonitors()
configured$setSamplers(params)
configured$addMonitors(params)
configured$setThin(10)

## Build and compile
built <- buildMCMC(configured)
compiled <- compileNimble( model, built, showCompilerOutput = TRUE )

## Run sampler
compiled$built$run(niter = 5000)

## Formate samples
samples <- as.mcmc(as.matrix(compiled$built$mvSamples)) %>%
  window(start = 100)

## Compute summaries
summ <- summary(samples)

## Examine traceplots for select parameters
ggsamples <- samples %>%
  as_tibble() %>%
  rowid_to_column(var = "Iteration") %>%
  gather(key = Parameter, value = Value, -Iteration)

## mu_y
tmp <- grep("mu_y",rownames(summ[[1]]))

cbind(summ[[1]][tmp,c("Mean","SD")],
      summ[[2]][tmp,c("2.5%","97.5%")])

mydat %>%
  group_by(Period) %>%
  summarise(n = n(),
            ybar = mean(y),
            se = sd(y)/sqrt(n),
            lower = ybar - 1.96 * se,
            upper = ybar + 1.96 * se)

ggsamples %>%
  filter(grepl("mu_y", Parameter)) %>%
  ggplot(aes(x = Iteration, y = Value)) +
  geom_line()  +
  facet_grid(Parameter ~ ., scales = "free")


## mu
day <- 10

tmp <- grep(paste0("mu\\[",day,","),rownames(summ[[1]]))

cbind(summ[[1]][tmp,c("Mean","SD"), drop = FALSE],
      summ[[2]][tmp,c("2.5%","97.5%"), drop = FALSE])

ggsamples %>%
  filter(grepl(paste0("mu\\[",day,","), Parameter)) %>%
  ggplot(aes(x = Iteration, y = Value)) +
  geom_line()  +
  facet_grid(Parameter ~ ., scales = "free")

## etaDelta
ggsamples %>%
  filter(grepl("etaDelta",Parameter)) %>%
  ggplot(aes(x = Iteration, y = Value)) +
  geom_line() +
  facet_grid(Parameter ~ ., scales = "free")

## sigmaDelta


## xi
tmp <- grep("xi",rownames(summ[[1]]))

cbind(summ[[1]][tmp,c("Mean","SD")],
      summ[[2]][tmp,c("2.5%","97.5%")])

ggsamples %>%
  filter(grepl("xi\\[", Parameter)) %>%
  ggplot(aes(x = Iteration, y = Value)) +
  geom_line()  +
  facet_grid(Parameter ~ ., scales = "free")

## delta1
ggsamples %>%
  filter(grepl("delta\\[1",Parameter)) %>%
  ggplot(aes(x = Iteration, y = Value)) +
  geom_line() +
  facet_grid(Parameter ~ ., scales = "free")

## delta2
ggsamples %>%
  filter(grepl("delta\\[2",Parameter)) %>%
  ggplot(aes(x = Iteration, y = Value)) +
  geom_line() +
  facet_grid(Parameter ~ ., scales = "free")

## delta2[nDays] and delta.prime

c(summ[[1]]["delta.prime",c("Mean","SD")],
  summ[[2]]["delta.prime",c("2.5%","97.5%")])

bind_rows(filter(ggsamples, Parameter == paste0("delta[2, ",trueParams$nDays,"]")),
          filter(ggsamples, Parameter == "delta.prime")) %>%
  ggplot(aes(x = Iteration, y = Value)) +
  geom_line() +
  facet_grid(Parameter ~ ., scales = "free")
