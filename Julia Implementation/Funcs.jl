using Random, Distributions

function simPopulationParams( nBirds, mu_mu1, mu_mu2, mu_mu3, sd_mu_mu1, sd_mu_mu2, sd_mu_mu3, mu_sd1, mu_sd2, mu_sd3, sd_mu_sd1, sd_mu_sd2, sd_mu_sd3, mu_delta1, mu_delta2, sd_delta1, sd_delta2, debug )

  #List of params for nBirds

    if debug == 0
	println("BP1SP")
    end

    mu1 = rand(Distributions.Normal( mu_mu1, sd_mu_mu1), nBirds)
    mu2 = rand(Distributions.Normal( mu_mu2, sd_mu_mu2), nBirds)
    mu3 = rand(Distributions.Normal( mu_mu3, sd_mu_mu3), nBirds)

    if debug == 0
	println("BP2SP")
    end

    sd_mu1 = Vector{Float64}(undef, nBirds)
    sd_mu2 = Vector{Float64}(undef, nBirds)
    sd_mu3 = Vector{Float64}(undef, nBirds)

    if debug == 0
	println("BP3SP")
    end

    for i in 1:nBirds

      sd_mu1[i] = rand(Distributions.Normal(mu_sd1, sd_mu_sd1), 1)[1]
      sd_mu2[i] = rand(Distributions.Normal(mu_sd2, sd_mu_sd2), 1)[1]
      sd_mu3[i] = rand(Distributions.Normal( mu_sd3, sd_mu_sd3), 1)[1]

      while sd_mu1[i] <= 0
        sd_mu1[i] = rand(Distributions.Normal( mu_sd1, sd_mu_sd1), 1)[1]
      end

      while sd_mu2[i] <= 0
        sd_mu2[i] = rand(Distributions.Normal( mu_sd2, sd_mu_sd2), 1)[1]
      end
      while sd_mu3[i] <= 0
        sd_mu3[i] = rand(Distributions.Normal(mu_sd3, sd_mu_sd3), 1)[1]
      end
    end

    if debug == 0
	println("BP4SP")
    end


    delta1 = rand(Distributions.Normal( mu_delta1, sd_delta1 ), nBirds)
    delta2 = rand(Distributions.Normal( mu_delta2, sd_delta2 ), nBirds)

	if debug == 0
		println("BP5SP")
	end

    birds = Dict{Symbol, Any}(
		 :mu1 => mu1,
		 :mu2 => mu2,
		 :mu3 => mu3,
		 :sd_mu1 => sd_mu1,
		 :sd_mu2 => sd_mu2,
		 :sd_mu3 => sd_mu3,
		 :delta1 => delta1,
		 :delta2 => delta2
		)
    return(birds)
  end

  function  simPopulationData( birds, nBirds, tStep, tSpan, debug )
	  nObs = div(tSpan, tStep)
      times = [1:tStep:tSpan;]
      msrmnts = Array{Float64}(undef, nBirds, nObs)

	  if debug == 0
		  println("BP1SD")
	  end

	  for i in 1:nBirds
		delta1 = birds[:delta1][i]
		delta2 = birds[:delta2][i]
      	for j in 1:nObs
        	if ( times[i] < delta1 )
          		mean = birds[:mu1][i]
          		sd = birds[:sd_mu1][i]
        	elseif ( times[i] >= delta1 && times[i] < delta2 )
          		mean = birds[:mu2][i]
          		sd = birds[:sd_mu2][i]
        	else
          		mean = birds[:mu3][i]
          		sd = birds[:sd_mu3][i]
        	end

        	msrmnts[i , j] = rand(Distributions.Normal( mean[1], sd[1]), 1)[1]

        	while( msrmnts[i, j] >= 0 )
          		msrmnts[i, j] = rand(Distributions.Normal( mean, sd), 1)[1]
				if debug == 0
					println("BP2SD")
					println(msrmnts[i, j])
				end
        	end
      	end
	end

      return(
      	Dict{Symbol, Any}(
    		:y => msrmnts
    	)
      )
  end

	function step(t0, t)
		if t0 < t
			return 1
		else
			return 0
		end
 	end

	function period(delt1, delt2, t)
		return step(delt1, t) + step(delt2, t) + 1
	end
