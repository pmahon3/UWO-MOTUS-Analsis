library(rjags)

NPOPS = 10 
NBIRDS = 10
NDAYS = 10

for ( pop in 1:NPOPS){
  obsdat <- readRDS(paste("Out/Data/Data", pop,".RDS", sep ="" ))
  moddat <- readRDS(paste("Out/HPD/HPD", pop, ".RDS", sep=""))
  pdf(paste(".Plots/Population", pop, ".pdf", sep = ""))
  plot(moddat)
  dev.off()
  for ( bird in 1:NBIRDS ){
    
    pdf(paste(".Plots/Population", pop, "Bird", bird, ".pdf", sep = ""))
    plot(dat[[bird]][[day]])
    title(paste("Population", pop, "Bird", bird, sep = " "))
    
  
    for ( day in 1:(NDAYS-1)){
      points(obsdat[[bird]][[day]])
    }
    
    points(obsdat[[bird]][[NDAYS]], col = "red")
    dev.off()
  }
}

