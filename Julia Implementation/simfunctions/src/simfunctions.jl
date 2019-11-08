module simfunctions

import DataFrames
import Distributions
import Random

greet() = print("Hello World!")

function simBirdData(  tStep, tSpan, mu1, mu2, mu3, sd_mu1, sd_mu2, sd_mu3, delta1, delta2 )
  nObs = div(tSpan, tStep)
  times = [1:tStep:tSpan;]
  msrmnts = Vector{Float64}(undef, nObs)

  for i in 1:nOb
    if ( times[i] < delta1 )
      mean = mu1
      sd = sd_mu1
    elseif ( times[i] >= delta1 && times[i] < delta2 )
      mean = mu2
      sd = sd_mu2
    else
      mean = mu3
      sd = sd_mu3
    end

    msrmnts[i] <- rand(Distributions.Normal( mean, sd), 1)[1]

    while( times[i] <= 0 )
      msrmnts[i] <- rand(Distributions.Normal( mean, sd), 1)[1]
    end
  end

  return( DataFrame( times, msrmnts ) )
end

function simPopulationParams( nBirds, mu_mu1, mu_mu2, mu_mu3, sd_mu_mu1, sd_mu_mu2, sd_mu_mu3, mu_sd1, mu_sd2, mu_sd3, sd_mu_sd1, sd_mu_sd2, sd_mu_sd3, mu_delta1, mu_delta2, sd_delta1, sd_delta2 )

  #List of params for nBirds

    mu1 = rand(Distributions.Normal( mu_mu1, sd_mu_mu1), nBirds)
    mu2 = rand(Distributions.Normal( mu_mu2, sd_mu_mu2), nBirds)
    mu3 = rand(Distributions.Normal( mu_mu3, sd_mu_mu3), nBirds)

    sd_mu1 = Vector{Float64}(undef, nBirds)
    sd_mu2 = Vector{Float64}(undef, nBirds)
    sd_mu3 = Vector{Float64}(undef, nBirds)

    for i in 1:nBirds

      sd_mu1[i] = rand(Distributions.Normal(mu_sd1, sd_mu_sd1), 1)[1]
      sd_mu2[i] = rand(Distributions.Normal(mu_sd2, sd_mu_sd2), 1)[1]
      sd_mu3[i] = rand(Distributions.Normal( mu_sd3, sd_mu_sd3), 1)[1]

      while sd_mu1[i] <= 0
        sd_mu1[i] <- rand(Distributions.Normal( mu_sd1, sd_mu_sd1), 1)[1]
      end
      while sd_mu2[i] <= 0
        sd_mu2[i] <- rand(Distributions.Normal( mu_sd2, sd_mu_sd2), 1)[1]
      end
      while sd_mu3[i] <= 0
        sd_mu3[i] <- rand(Distributions.Normal(mu_sd3, sd_mu_sd3), 1)[1]
      end
    end


    delta1 = rand(Distributions.Normal( nBirds, mu_delta1, sd_delta1 ), 1)
    delta2 = rand(Distributions.Normal( nBirds, mu_delta2, sd_delta2 ), 1)

    birds = DataFrame(mu1, mu2, mu3, sd_mu1, sd_mu2, sd_mu3, delta1, delta2)
    return(birds)
  end

  function  simPopulationData( birds, tStep, tSpan )
    n = nrow( birds )
    dat = vector( mode = "list", length = n)

    for row in 1:n
      newBird <- simBirdData( tStep, tSpan, birds[row, "mu1"], birds[row, "mu2"], birds[row, "mu3"], birds[row, "sd_mu1"], birds[row, "sd_mu2"], birds[row, "sd_mu3"], birds[row, "delta1"] , birds[row, "delta2"])
      dat[[ row ]] = newBird
    end
    return( dat )
  end
end # module
