# yaps - YAPS (Yet Another Positioning Solver) (R package)
The yaps package requires [devtools] and [TMB](https://github.com/kaskr/adcomp). 
Please see https://github.com/kaskr/adcomp/wiki/Download for instructions on TMB installation. Remember to install Rtools needed for TMB to run.


Installation:
```
install.packages('devtools')
devtools::install_github("baktoft/yaps")
```

Usage example:
```
rm(list=ls())	
library(yaps)
set.seed(42)

# Simulate "true" track of animal movement of n seconds
trueTrack <- simTrueTrack(model='crw', n = 15000, deltaTime=1, shape=1, scale=0.5, addDielPattern=TRUE)

# Simulate telemetry observations from true track.
# Format and parameters depend on type of transmitter burst interval (BI) - stable (sbi) or random (rbi).
pingType <- 'rbi'

if(pingType == 'sbi') {
	# # # # stable BI
	sbi_mean <- 5; sbi_sd <- 1e-4;
	teleTrack <- simTelemetryTrack(trueTrack, ss='rw', pingType=pingType, sbi_mean=sbi_mean, sbi_sd=sbi_sd)
} else if(pingType == 'rbi'){
	# # # # random BI
	pingType <- 'rbi'; rbi_min <- 20; rbi_max <- 40;
	teleTrack <- simTelemetryTrack(trueTrack, ss='rw', pingType=pingType, rbi_min=rbi_min, rbi_max=rbi_max)
}

# Simulate hydrophone array
hydros <- simHydros(auto=TRUE, trueTrack=trueTrack)
toa_list <- simToa(teleTrack, hydros, pingType, sigmaToa=1e-4, pNA=0.25, pMP=0.01)
toa <- toa_list$toa

if(pingType == 'sbi'){
	inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=0)
} else if(pingType == 'rbi'){
	inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=0, rbi_min=rbi_min, rbi_max=rbi_max)
} 
str(inp)

pl <- c()
maxIter <- ifelse(pingType=="sbi", 500, 5000)
outTmb <- runTmb(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE)
str(outTmb)

# Estimates 
pl <- outTmb$pl
# Error estimates
plsd <- outTmb$plsd

# plot the resulting estimated track
plot(y~x, data=trueTrack, type="l", xlim=range(hydros$hx), ylim=range(hydros$hy), asp=1)
lines(y~x, data=teleTrack)
points(hy~hx, data=hydros, col="green", pch=20, cex=3)
lines(pl$Y~pl$X, col="red")

```