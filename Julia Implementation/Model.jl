## HELPER FUNCTIONS

function heaviside(t0, t)
	if t-t0 > 0
		return 1
	else
		return 0
	end
end

function period(delt1, delt2, t)
	return heaviside(delt1, t) + heaviside(delt2, t) + 1
end

## MODEL DESCRIPTION

model = Model(

	y = Stochastic(2,
		(delta1, delta2, times, sigma_y, m_y, nObs) ->
			UnivariateDistribution[(
				p = period(delta1[i], delta2[i], times[i,j]);
				Normal( m_y[i, p ], 1 / sigma_y[i, p ]^2 )) for i in 1:nBirds, j in 1:nObs
			],
		false
	),

	m_y = Stochastic(2,

		( mu_mu, sd_mu, nBirds ) ->
			UnivariateDistribution[(
				Normal( mu_mu[j], sd_mu[j] )) for i in 1:nBirds, j in 1:3
			],
		false
	),

	sigma_y = Stochastic(2,
		( nBirds, nObs ) ->
			UnivariateDistribution[
				(Gamma( 2, 10 )) for i in 1:nBirds, j in 1:3
			],
		false
	),


	delta1 = Stochastic(1,
		( mu_delta1, sd_delta1, nBirds ) ->
			UnivariateDistribution[(
				Normal( mu_delta1, sd_delta1^2 )) for i in 1:nBirds
			],
		false
	),

	delta2 = Stochastic(1,
		( mu_delta2, sd_delta2, nBirds ) ->
			UnivariateDistribution[(
				Normal( mu_delta2, sd_delta2^2 )) for i in 1:nBirds
			],
		false
	),

	mu_delta1 = Stochastic(
		(mu_mu_delta, sd_mu_delta ) -> Normal( mu_mu_delta[1], sd_mu_delta[1]^2 ),
		true
	),

	mu_delta2 = Stochastic(
		(mu_mu_delta, sd_mu_delta ) -> Normal( mu_mu_delta[2], sd_mu_delta[2]^2 ),
		true
	),

	sd_delta1 = Stochastic(
		( mu_sd_delta, sd_sd_delta ) -> Normal( mu_sd_delta[1], sd_sd_delta[1]^2 ),
		false
	),

	sd_delta2 = Stochastic(
		( mu_sd_delta, sd_sd_delta ) -> Normal( mu_sd_delta[2], sd_sd_delta[2]^2 ),
		false
	)
)