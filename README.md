
<!-- README_sync.md is generated from README_sync.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version/yaps)](https://cran.r-project.org/package=yaps)
[![R-CMD-check](https://github.com/baktoft/yaps/workflows/R-CMD-check/badge.svg)](https://github.com/baktoft/yaps/actions)
[![Travis build
status](https://travis-ci.org/baktoft/yaps.svg?branch=master)](https://travis-ci.org/baktoft/yaps)
[![Codecov test
coverage](https://codecov.io/gh/baktoft/yaps/branch/master/graph/badge.svg)](https://codecov.io/gh/baktoft/yaps?branch=master)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/yaps)](https://cran.r-project.org/package=yaps)
<!-- badges: end -->

# YAPS - (Yet Another Positioning Solver)<img src="man/logo/yaps_logo.png" align="right" width="120" />

## IMPORTANT
<b>YAPS was archived on CRAN due to dependency on package splusTimeSeries which were deprecated. 
We are currently working on YAPS v2. Besides not using splusTimeSeries function, it features a new approach to sync and several other improvements. Stay tuned!</b>

## Introduction
Welcome to the `yaps` repository. The `yaps` package is based on the
original YAPS presented in Baktoft, Gjelland, Økland & Thygesen (2017):
[Positioning of aquatic animals based on time-of-arrival and random walk
models using YAPS (Yet Another Positioning
Solver)](https://www.nature.com/articles/s41598-017-14278-z.pdf)

To use `yaps` on own data, you need to compile a TOA-matrix based on
synchronized hydrophone data and replace the hydros dataframe with
actual hydrophone positions. A complete step-by-step guide on how to do
this, can be found in our pre-print paper [Opening the black box of fish
tracking using acoustic
telemetry](https://www.biorxiv.org/content/10.1101/2019.12.16.877688v1).
The example in this guide is based on data collected using a 69 kHz
PPM-based system (Vemco VR2). We are working towards adding examples
based on data collected using other manufacturers.

## Dependencies

The `yaps` package requires
[devtools](https://CRAN.R-project.org/package=devtools) and
[TMB](https://github.com/kaskr/adcomp). Please see
[instructions](https://github.com/kaskr/adcomp/wiki/Download) on TMB
installation. If working on Windows, you might also need to install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) as specified in
the TMB documentation.

## Disclaimer

**`yaps` obeys the fundamental rule of “garbage in, garbage out”.
Therefore, DO NOT expect `yaps` to salvage a poorly designed study, nor
to turn crappy data into gold.**  
We have attempted to make both synchronization process and track
estimation user-friendly. However, it is not trivial to synchronize
hydrophones (let alone automating the process) based on detections in a
variable and often noisy environment. Hydrophones might be
replaced/shifted and if not fixed securely, hydrophones might move/be
moved during a study. Additionally, hydrophone performance and output
format varies considerably among (and within) manufacturers. On top of
that, hydrophones don’t always behave and perform as expected. For
instance, some hydrophone models autonomously initiate reboots causing
perturbation of varying magnitude and/or duration of the internal clock
at apparently random time intervals. Therefore, the functions in `yaps`
might perform sub-optimal or even fail miserably when applied to new
data. If/when this happens, please let us know through a direct message
or leave a bug-report. Also note, the to-do list for improvements and
tweaks is long and growing, so stay tuned for updates.

## Installation

Make sure you have the newest version of `yaps` installed. Run
`install.packages('yaps')` to get the latest version on CRAN.

## Processing example data ssu1

The code below is based on the example workflow presented in [Opening
the black box of fish tracking using acoustic
telemetry](https://www.biorxiv.org/content/10.1101/2019.12.16.877688v1).
See the pre-print for further explantion of parameters and workflow.

``` r
library(yaps)
set.seed(42) # Just to keep consistency in this example

# # # Example using the ssu1 data included in package. See ?ssu1 for info.
# # # Set parameters to use in the sync model - these will differ per study
max_epo_diff <- 120
min_hydros <- 2
time_keeper_idx <- 5
fixed_hydros_idx <- c(2:3, 6, 8, 11, 13:17)
n_offset_day <- 2
n_ss_day <- 2
keep_rate <- 20

# # # Get input data ready for getSyncModel()
inp_sync <- getInpSync(sync_dat=ssu1, max_epo_diff, min_hydros, time_keeper_idx, 
    fixed_hydros_idx, n_offset_day, n_ss_day, keep_rate=keep_rate, silent_check=TRUE)

# # # Check that inp_sync is ok
checkInpSync(inp_sync, silent_check=FALSE)

# # # Also take a look at coverage of the sync data
getSyncCoverage(inp_sync, plot=TRUE)

# # # Fit the sync model
sync_model <- getSyncModel(inp_sync, silent=TRUE, max_iter=200, tmb_smartsearch = TRUE)

# # # On some systems it might work better, if we disbale the smartsearch feature in TMB
# # # To do so, set tmb_smartsearch = FALSE in getSyncModel()

# # # Visualize the resulting sync model
plotSyncModelResids(sync_model, by = "overall")
plotSyncModelResids(sync_model, by = "quantiles")
plotSyncModelResids(sync_model, by = "sync_tag")
plotSyncModelResids(sync_model, by = "hydro")
plotSyncModelResids(sync_model, by = "temporal_hydro")
plotSyncModelResids(sync_model, by = "temporal_sync_tag")

# # # If the above plots show outliers, sync_model can be fine tuned by excluding these.
# # # Use fineTuneSyncModel() for this.
# # # This should typically be done sequentially using eps_thresholds of e.g. 1E4, 1E3, 1E2, 1E2
sync_model <- fineTuneSyncModel(sync_model, eps_threshold=1E3, silent=TRUE)
sync_model <- fineTuneSyncModel(sync_model, eps_threshold=1E2, silent=TRUE)

# # # Apply the sync_model to detections data.
detections_synced <- applySync(toa=ssu1$detections, hydros=ssu1$hydros, sync_model)

# # # Prepare data for running yaps
hydros_yaps <- data.table::data.table(sync_model$pl$TRUE_H)
colnames(hydros_yaps) <- c('hx','hy','hz')
focal_tag <- 15266
rbi_min <- 20
rbi_max <- 40
synced_dat <- detections_synced[tag == focal_tag]
toa <- getToaYaps(synced_dat=synced_dat, hydros=hydros_yaps, pingType='rbi', 
  rbi_min=rbi_min, rbi_max=rbi_max)
bbox <- getBbox(hydros_yaps, buffer=50, pen=1e6)
inp <- getInp(hydros_yaps, toa, E_dist="Mixture", n_ss=5, pingType="rbi", 
  sdInits=1, rbi_min=rbi_min, rbi_max=rbi_max, ss_data_what="est", ss_data=0, bbox=bbox)

# # # Check that inp is ok
checkInp(inp)

# # # Run yaps on the prepared data to estimate track
yaps_out <- runYaps(inp, silent=TRUE, tmb_smartsearch=TRUE, maxIter=5000) 

# # # Plot the results and compare to "the truth" obtained using gps

oldpar <- par(no.readonly = TRUE) 
par(mfrow=c(2,2))
plot(hy~hx, data=hydros_yaps, asp=1, xlab="UTM X", ylab="UTM Y", pch=20, col="green")
lines(utm_y~utm_x, data=ssu1$gps, col="blue", lwd=2)
lines(y~x, data=yaps_out$track, col="red")

plot(utm_x~ts, data=ssu1$gps, col="blue", type="l", lwd=2)
points(x~top, data=yaps_out$track, col="red")
lines(x~top, data=yaps_out$track, col="red")
lines(x-2*x_sd~top, data=yaps_out$track, col="red", lty=2)
lines(x+2*x_sd~top, data=yaps_out$track, col="red", lty=2)

plot(utm_y~ts, data=ssu1$gps, col="blue", type="l", lwd=2)
points(y~top, data=yaps_out$track, col="red")
lines(y~top, data=yaps_out$track, col="red")
lines(y-2*y_sd~top, data=yaps_out$track, col="red", lty=2)
lines(y+2*y_sd~top, data=yaps_out$track, col="red", lty=2)

plot(nobs~top, data=yaps_out$track, type="p", main="#detecting hydros per ping")
lines(caTools::runmean(nobs, k=10)~top, data=yaps_out$track, col="orange", lwd=2)
par(oldpar)
```

### Example using YAPS on simulated data

``` r
rm(list=ls())   
library(yaps)

# Simulate true track of animal movement of n seconds
trueTrack <- simTrueTrack(model='crw', n = 15000, deltaTime=1, shape=1, scale=0.5, addDielPattern=TRUE, ss='rw')

# Simulate telemetry observations from true track.
# Format and parameters depend on type of transmitter burst interval (BI) - stable (sbi) or random (rbi).
pingType <- 'sbi'

if(pingType == 'sbi') { # stable BI
    sbi_mean <- 30; sbi_sd <- 1e-4;
    teleTrack <- simTelemetryTrack(trueTrack, pingType=pingType, sbi_mean=sbi_mean, sbi_sd=sbi_sd)
} else if(pingType == 'rbi'){ # random BI
    pingType <- 'rbi'; rbi_min <- 20; rbi_max <- 40;
    teleTrack <- simTelemetryTrack(trueTrack, pingType=pingType, rbi_min=rbi_min, rbi_max=rbi_max)
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

yaps_out <- c()
maxIter <- ifelse(pingType=="sbi", 500, 5000)
yaps_out <- runTmb(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE)
str(yaps_out)

# Estimates in pl
pl <- yaps_out$pl
# Correcting for hydrophone centering
pl$X <- yaps_out$pl$X + inp$inp_params$Hx0
pl$Y <- yaps_out$pl$Y + inp$inp_params$Hy0


# Error estimates in plsd
plsd <- yaps_out$plsd

# plot the resulting estimated track and the true simulated track
par(mfrow=c(2,2))
plot(hy~hx, data=hydros, asp=1, xlab="UTM X", ylab="UTM Y", pch=20, col="green")
lines(y~x, data=trueTrack, col="blue", lwd=2)
lines(y~x, data=yaps_out$track, col="red")

plot(x~time, data=trueTrack, col="blue", type="l", lwd=2)
points(x~top, data=yaps_out$track, col="red")
lines(x~top, data=yaps_out$track, col="red")
lines(x-2*x_sd~top, data=yaps_out$track, col="red", lty=2)
lines(x+2*x_sd~top, data=yaps_out$track, col="red", lty=2)

plot(y~time, data=trueTrack, col="blue", type="l", lwd=2)
points(y~top, data=yaps_out$track, col="red")
lines(y~top, data=yaps_out$track, col="red")
lines(y-2*y_sd~top, data=yaps_out$track, col="red", lty=2)
lines(y+2*y_sd~top, data=yaps_out$track, col="red", lty=2)

plot(nobs~top, data=yaps_out$track, type="p", main="#detecting hydros per ping")
lines(caTools::runmean(nobs, k=10)~top, data=yaps_out$track, col="orange", lwd=2)
```

# Papers using or relating to YAPS

## 2020

-   Baktoft, H., Gjelland, K., Szabo-Meszaros, M. et al (2020). Can
    energy depletion of wild atlantic salmon kelts negotiating
    hydropower facilities lead to reduced survival? Sustain. 12, 1–12.
    <https://doi.org/10.3390/SU12187341>

-   Hubert, J., Campbell, J. & Slabbekoorn, H. (2020). Effects of
    seismic airgun playbacks on swimming patterns and behavioural states
    of Atlantic cod in a net pen. Marine Pollution Bulletin. 160.
    111680. <https://doi.org/10.1016/j.marpolbul.2020.111680>

-   Vergeynst, J., Pauwels, I., Baeyens, R. et al. (2020). Shipping
    canals on the downstream migration route of European eel ( Anguilla
    anguilla ): Opportunity or bottleneck?. Ecology of Freshwater
    Fish. 30. <https://doi.org/10.1111/eff.12565>

-   Vergeynst, J., Vanwyck, T., Baeyens, R. et al. (2020). Acoustic
    positioning in a reflective environment: going beyond point-by-point
    algorithms. Anim Biotelemetry 8, 16.
    <https://doi.org/10.1186/s40317-020-00203-1>

-   Vergeynst, J., Baktoft, H., Mouton, A. et al. (2020). The influence
    of system settings on positioning accuracy in acoustic telemetry,
    using the YAPS algorithm. Anim. Biotelemetry, 8, 1–12,
    <https://doi.org/10.1186/s40317-020-00211-1>

## 2019

-   Baktoft, H., Gjelland, K.Ø., Økland, F., Rehage, J.S., Rodemann,
    J.R., Corujo, R.S., Viadero, N., Thygesen, U.H. (2019). Opening the
    black box of fish tracking using acoustic telemetry. bioRxiv
    2019.12.16.877688; doi: <https://doi.org/10.1101/2019.12.16.877688>

-   Silva, A.T., Bærum, K.M., Hedger, R.D., Baktoft, H., Fjeldstad, H.,
    Gjelland, K.Ø., Økland, F. Forseth, T. (2019). Science of the Total
    Environment The effects of hydrodynamics on the three-dimensional
    downstream migratory movement of Atlantic salmon. Science of the
    Total Environment, 135773.
    <https://doi.org/10.1016/j.scitotenv.2019.135773>

-   Szabo-Meszaros, M., Forseth, T., Baktoft, H., Fjeldstad, H.-P.,
    Silva, A.T., Gjelland, K.Ø., Økland, F., Uglem, I., Alfredsen, K.
    (2019). Modelling mitigation measures for smolt migration at dammed
    river sections. Ecohydrology, e2131.
    <https://doi.org/10.1002/eco.2131>

## 2017

-   Baktoft, H., Gjelland, K.Ø., Økland, F., Thygesen, U.H. (2017).
    Positioning of aquatic animals based on time-of-arrival and random
    walk models using YAPS (Yet Another Positioning Solver). Sci.
    Rep. 2017, 7, 14294. <https://doi.org/10.1038/s41598-017-14278-z>
