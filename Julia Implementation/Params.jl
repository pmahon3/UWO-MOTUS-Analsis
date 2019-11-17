output = 0

nBirds = 100
tStep = 0.005  ## 18 second step size
tSpan = 24
nObs = convert(Int, tSpan / tStep)

mu_mu1 = -80
mu_mu2 = -40
mu_mu3 = -80
sd_mu1 = 5
sd_mu2 = 5
sd_mu3 = 5

mu_sd1 = 5
mu_sd2 = 5
mu_sd3 = 5
sd_sd1 = .001
sd_sd2 = .001
sd_sd3 = .001

mu_mu_delta1 = 6
mu_mu_delta2 = 20
sd_mu_delta1 = 1
sd_mu_delta2 = 1

if output == 0

	println("The parameters of the simulation are: ", "\n",
		"nBirds = ",  nBirds, "\n",
		"tStep = ", tStep, "\n",
		"tSpan = ", tSpan, "\n",
		"nObs = ", nObs, "\n",

		"mu_mu1 = ", mu_mu1, "\n",
		"mu_mu2 = ", mu_mu2, "\n",
		"mu_mu3 = ", mu_mu3, "\n",
		"sd_mu1 = ", sd_mu1, "\n",
		"sd_mu2 = ", sd_mu2, "\n",
		"sd_mu3 = ", sd_mu3, "\n",

		"mu_sd1 = ", mu_sd1, "\n",
		"mu_sd2 = ", mu_sd2, "\n",
		"mu_sd3 = ", mu_sd3, "\n",
		"sd_sd1 = ", sd_sd1, "\n",
		"sd_sd2 = ", sd_sd2, "\n",
		"sd_sd3 = ", sd_sd3, "\n",

		"mu_mu_delta1 = ", mu_mu_delta1, "\n",
		"mu_mu_delta2 = ", mu_mu_delta2, "\n",
		"sd_mu_delta1 = ", sd_mu_delta1, "\n",
		"sd_mu_delta2 = ", sd_mu_delta2, "\n",
	)
end
