using Mamba

## SCRIPTS TO INCLUDE

include("Params.jl")			# Sets parameters for simulation
include("Funcs.jl")			# Builds functions for simulation


## POPULATION SIMULATION 


birdPop = simPopulationParams( nBirds, mu_mu1, mu_mu2, mu_mu3, sd_mu1, sd_mu2, sd_mu3, mu_sd1, mu_sd2, mu_sd3, sd_sd1, sd_sd2, sd_sd3, mu_mu_delta1, mu_mu_delta2, sd_mu_delta1, sd_mu_delta2, 1)
birdDat = simPopulationData( birdPop, nBirds, tStep, tSpan, 1 )

## DATA FORMATTING FOR MCMC CALLS

nBirds = size(birdDat[:y], 1)
nObs = size(birdDat[:y], 2)


# Vector initialization

mu_mu_delta = Vector{Float64}(undef, 2)
sd_mu_delta = Vector{Float64}(undef, 2)
mu_sd_delta = Vector{Float64}(undef, 2)
sd_sd_delta = Vector{Float64}(undef, 2)
mu_mu = Vector{Float64}(undef, 3)
sd_mu = Vector{Float64}(undef, 3)
delta1 = Vector{Float64}(undef, nBirds)
delta2 = Vector{Float64}(undef, nBirds)
m_y = Array{Float64}(undef, nBirds, 3)
sigma_y = Array{Float64}(undef, nBirds, 3)
per = Array{Float64}(undef, nBirds, nObs)

## Populating vectors with initial values

for i in 1:nBirds delta1[i] = 6 end
for i in 1:nBirds delta2[i] = 18 end

for i in 1:nBirds
	m_y[i, 1] = -80
	m_y[i, 2] = -40
	m_y[i, 3] = -80

	sigma_y[i, 1] = 5
	sigma_y[i, 2] = 5
	sigma_y[i, 3] = 5
end

times = Array{Float64}(undef, nBirds, nObs)
for i in 1:nBirds
	t = tStep;
	for j in 1nObs
		times[i, j] = t
		t = t + tStep  
	end
end

for i in 1:nBirds
	for j in 1:nObs
		t = times[i,j]
		per[i, j] = period(delta1[1], delta2[1], t )
	end
end

mu_delta1 = 5.5
mu_delta2 = 19.5
sd_delta1 = 1
sd_delta2 = 1
mu_mu[1] = -80
mu_mu[2] = -40
mu_mu[3] = -80
sd_mu[1] = 5
sd_mu[2] = 5
sd_mu[3] = 5
mu_mu_delta[1] = 5.5
mu_mu_delta[2] = 19.5
sd_mu_delta[1] = 1
sd_mu_delta[2] = 1
mu_sd_delta[1] = 1
mu_sd_delta[2] = 1
sd_sd_delta[1] = 0.01
sd_sd_delta[2] = 0.01

# CALL TO INCLUDE MODEL 

include("Model.jl")


# CONSTRUCT INITIAL VALUE OBJECTS
# Stored in a dictionary with (symbol)-(vector/scalar) pairs. 

# Inits array is for stochastic node initial values
inits = [						
			
Dict{Symbol, Any}(
	:y => birdDat[:y],
	:sd_delta1 => sd_delta1,
	:sd_delta2 => sd_delta2,
	:mu_delta1 => mu_delta1,
	:mu_delta2 => mu_delta2,
	:delta1 => delta1,
	:delta2 => delta2,
	:m_y => m_y,
	:sigma_y => sigma_y
)
for i in 1:3				# Iterate 3 times for 3 chains
]


# Inputs array is for deterministic node values

inputs = Dict{Symbol, Any}(					
	:times => times,
	:nObs => nObs,
	:nBirds => nBirds,
	:mu_mu => mu_mu,
	:sd_mu => sd_mu,
	:mu_mu_delta => mu_mu_delta,
	:sd_mu_delta => sd_mu_delta,
	:mu_sd_delta => mu_sd_delta,
	:sd_sd_delta => sd_sd_delta,
)

## SAMPLING METHOD & MONITROS
# No U-Turn Sampler with monitors on mu_deltas 

scheme = [HMC(:mu_delta1, 0.002, 5),
	  HMC(:mu_delta2, 0.002, 5)
	 ] 
setsamplers!(model, scheme)

## RUN CHAINS

sim = mcmc( model, inputs, inits, 100000, burnin = 2000, thin = 2, chains = 3)

## OUTPUT

p = plot(sim);
draw(p, filename="summaryplot.svg");
draw(model, filename="DAG.dot");
describe(sim)

# UNIX only
run(`dot -Tps DAG.dot -o DAG.pdf`);
run(`xdg-open DAG.pdf summaryplot.svg`);
