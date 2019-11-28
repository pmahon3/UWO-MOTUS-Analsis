include(Parallel);
include(simfunctions);

pars_mat <- gen_pars_mat();
cl <- makeCluster(getOption("cl.cores", 6))
clusterEvalQ(cl, library(rjags))
out <- clusterApply(cl, 1:nrow(pars_mat), fun = sim_function, pars_mat = pars_mat)
