output = 0

nBirds = 100
tStep = 12
tSpan = 86400
n = div(tStep,tSpan)

mu_mu1 = -80
mu_mu2 = -40
mu_mu3 = -80
sd_mu_mu1 = 5
sd_mu_mu2 = 5
sd_mu_mu3 = 5

mu_sd1 = 5
mu_sd2 = 5
mu_sd3 = 5
sd_mu_sd1 = .001
sd_mu_sd2 = .001
sd_mu_sd3 = .001

mu_delta1 = 0.25 * 86400
mu_delta2 = 0.75 * 86400
sd_mu_delta1 = 60
sd_mu_delta2 = 60

if output == 0

	println("The parameters of the simulation are: ", "\n",
		"nBirds = ",  nBirds, "\n",
		"tStep = ", tStep, "\n",
		"tSpan = ", tSpan, "\n",
		"n = ", n, "\n",

		"mu_mu1 = ", mu_mu1, "\n",
		"mu_mu2 = ", mu_mu2, "\n",
		"mu_mu3 = ", mu_mu3, "\n",
		"sd_mu_mu1 = ", sd_mu_mu1, "\n",
		"sd_mu_mu2 = ", sd_mu_mu2, "\n",
		"sd_mu_mu3 = ", sd_mu_mu3, "\n",

		"mu_sd1 = ", mu_sd1, "\n",
		"mu_sd2 = ", mu_sd2, "\n",
		"mu_sd3 = ", mu_sd3, "\n",
		"sd_mu_sd1 = ", sd_mu_sd1, "\n",
		"sd_mu_sd2 = ", sd_mu_sd2, "\n",
		"sd_mu_sd3 = ", sd_mu_sd3, "\n",

		"mu_delta1 = ", mu_delta1, "\n",
		"mu_delta2 = ", mu_delta2, "\n",
		"sd_mu_delta1 = ", sd_mu_delta1, "\n",
		"sd_mu_delta2 = ", sd_mu_delta2, "\n",
	)
end
