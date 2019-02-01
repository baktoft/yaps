# YAPS - (Yet Another Positioning Solver) 
Based on the original [YAPS](https://www.nature.com/articles/s41598-017-14278-z.pdf) presented in Baktoft, Gjelland, Økland & Thygesen (2017): Positioning of aquatic animals based on time-of-arrival and random walk models using YAPS (Yet Another Positioning Solver). [DOI:10.1038/s41598-017-14278-z](https://www.nature.com/articles/s41598-017-14278-z.pdf)  

A few changes have been made to allow track estimation from random burst interval transmitters (by popular demand) and to improve performance.  

To use on own data, compile a toa-matrix based on synchronized hydrophone data and replace the hydros dataframe with actual hydrophone positions. 

The yaps package requires [devtools](https://cran.r-project.org/web/packages/devtools/index.html) and [TMB](https://github.com/kaskr/adcomp).  
Please see for [instructions](https://github.com/kaskr/adcomp/wiki/Download) on TMB installation. Remember to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) as specified in the TMB documentation.

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

# Simulate true track of animal movement of n seconds
trueTrack <- simTrueTrack(model='crw', n = 15000, deltaTime=1, shape=1, scale=0.5, addDielPattern=TRUE)

# Simulate telemetry observations from true track.
# Format and parameters depend on type of transmitter burst interval (BI) - stable (sbi) or random (rbi).
pingType <- 'sbi'

if(pingType == 'sbi') { # stable BI
	sbi_mean <- 30; sbi_sd <- 1e-4;
	teleTrack <- simTelemetryTrack(trueTrack, ss='rw', pingType=pingType, sbi_mean=sbi_mean, sbi_sd=sbi_sd)
} else if(pingType == 'rbi'){ # random BI
	pingType <- 'rbi'; rbi_min <- 20; rbi_max <- 40;
	teleTrack <- simTelemetryTrack(trueTrack, ss='rw', pingType=pingType, rbi_min=rbi_min, rbi_max=rbi_max)
}

# Simulate hydrophone array
hydros <- simHydros(auto=TRUE, trueTrack=trueTrack)
toa_list <- simToa(teleTrack, hydros, pingType, sigmaToa=1e-4, pNA=0.25, pMP=0.01)
toa <- toa_list$toa

# Specify whether to use ss_data from measured water temperature (ss_data_what <- 'data') or to estimate ss in the model (ss_data_what <- 'est')
ss_data_what <- 'data'
if(ss_data_what == 'data') {ss_data <- teleTrack$ss} else {ss_data <- 0}


if(pingType == 'sbi'){
	inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=0, ss_data_what=ss_data_what, ss_data=ss_data)
} else if(pingType == 'rbi'){
	inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=0, rbi_min=rbi_min, rbi_max=rbi_max, ss_data_what=ss_data_what, ss_data=ss_data)
} 
str(inp)

pl <- c()
maxIter <- ifelse(pingType=="sbi", 500, 5000)
outTmb <- runTmb(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE)
str(outTmb)

# Estimates in pl
pl <- outTmb$pl

# Error estimates in plsd
plsd <- outTmb$plsd

# plot the resulting estimated track
plot(y~x, data=trueTrack, type="l", xlim=range(hydros$hx), ylim=range(hydros$hy), asp=1)
lines(y~x, data=teleTrack)
points(hy~hx, data=hydros, col="green", pch=20, cex=3)
lines(pl$Y~pl$X, col="red")
```
