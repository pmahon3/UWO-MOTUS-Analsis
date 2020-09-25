

source("Inputs.R")

delta = trueParams["delta"]
delta1 = trueParams["delta1"]
delta2 = trueParams["delta2"]
mu1 = trueParams$muY[[1]]
mu2 = trueParams$muY[[2]]
mu3 = trueParams$muY[[3]]
coverage = vector( mode = "numeric", length = 6 )
sd = vector(mode = "numeric", length = 6 )
names = list( "delta", "delta1", "delta2", "mu1", "mu2", "mu3")
names(coverage) = names
names(sd) = names
delta_rope = 0

for ( sim in 1:100) {
    dat = readRDS(paste("results/samples/bird", toString(sim), ".rds", sep = ""))
    delta_bounds = quantile(dat[,1], probs = c(0.025, 0.975))
    delta1_bounds = quantile(dat[,2], probs = c(0.025, 0.975))
    delta2_bounds = quantile(dat[,3], probs = c(0.025, 0.975))
    mu1_bounds = quantile(dat[,4], probs = c(0.025, 0.975))
    mu2_bounds = quantile(dat[,5], probs = c(0.025, 0.975))
    mu3_bounds = quantile(dat[,6], probs = c(0.025, 0.975))
    sd[1] = sd[1] + sd(dat[,1])
    sd[2] = sd[2] + sd(dat[,2])
    sd[3] = sd[3] + sd(dat[,3])
    sd[4] = sd[4] + sd(dat[,4])
    sd[5] = sd[5] + sd(dat[,5])
    sd[6] = sd[6] + sd(dat[,6])
    if ( delta_bounds[1] <= delta && delta <= delta_bounds[2] ) { coverage[1] = coverage[1] + 1 }
    if ( 0 <= delta_bounds[1] || 0 >= delta_bounds[2] ) { delta_rope = delta_rope + 1 }
    if ( delta1_bounds[1] <= delta1 && delta1 <= delta1_bounds[2] ) { coverage[2] = coverage[2] + 1 }
    if ( delta2_bounds[1] <= delta2 && delta2 <= delta2_bounds[2] ) { coverage[3] = coverage[3] + 1 }
    if ( mu1_bounds[1] <= mu1 && mu1 <= mu1_bounds[2] ) { coverage[4] = coverage[4] + 1 }
    if ( mu2_bounds[1] <= mu2 && mu2 <= mu2_bounds[2] ) { coverage[5] = coverage[5] + 1 }
    if ( mu3_bounds[1] <= mu3 && mu3 <= mu3_bounds[2] ) { coverage[6] = coverage[6] + 1 }
}

coverage = coverage / 100
sd = sd / 100

print("Coverages")
print(coverage)
print("SD's")
print(sd)
print(delta_rope)
